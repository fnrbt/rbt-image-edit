namespace ImageEdit

open System
open System.Text
open System.Globalization
open Renderer

/// S-expression front-end: parse an `.imed` document into a `Layer` (+ `ImageConfig`). The layer
/// AST is an expression tree, so sexp is its native notation. Structure is parens; adjustment names
/// resolve against `Adjustments.lower`; templates are parameterized subtrees expanded by
/// substitution ($param); rasters reference a `(sources …)` table. The reader is lifted verbatim
/// from `VideoEdit.Sexp`.
module Sexp =

    // ── reader ──
    type Sx =
        | SAtom of string        // symbol / number / #color / :keyword / $var
        | SStr of string         // "quoted"
        | SList of Sx list

    let parseAll (input: string) : Sx list =
        let n = input.Length
        let mutable i = 0
        let isDelim c = Char.IsWhiteSpace c || c = '(' || c = ')' || c = '"' || c = ';'
        let skipWs () =
            let mutable go = true
            while go && i < n do
                if input.[i] = ';' then (while i < n && input.[i] <> '\n' do i <- i + 1)
                elif Char.IsWhiteSpace input.[i] then i <- i + 1
                else go <- false
        let readStr () =
            i <- i + 1
            let sb = StringBuilder()
            while i < n && input.[i] <> '"' do
                if input.[i] = '\\' && i + 1 < n then (sb.Append input.[i + 1] |> ignore; i <- i + 2)
                else (sb.Append input.[i] |> ignore; i <- i + 1)
            i <- i + 1
            SStr(sb.ToString())
        let readAtom () =
            let s = i
            while i < n && not (isDelim input.[i]) do i <- i + 1
            SAtom(input.Substring(s, i - s))
        let rec readExpr () : Sx =
            skipWs ()
            if i >= n then failwith "sexp: unexpected EOF"
            elif input.[i] = '(' then
                i <- i + 1
                let items = ResizeArray<Sx>()
                skipWs ()
                while i < n && input.[i] <> ')' do
                    items.Add(readExpr ())
                    skipWs ()
                if i >= n then failwith "sexp: unclosed '('"
                i <- i + 1
                SList(List.ofSeq items)
            elif input.[i] = '"' then readStr ()
            elif input.[i] = ')' then failwith "sexp: unexpected ')'"
            else readAtom ()
        let out = ResizeArray<Sx>()
        skipWs ()
        while i < n do out.Add(readExpr ()); skipWs ()
        List.ofSeq out

    // ── document state ──
    type ImageConfig = { W: int; H: int; Bg: Color; Out: string }
    type Ctx = {
        Env: Map<string, Sx>
        Sources: Map<string, string>
        ResolveSource: string -> ImageRef
        ResolveFont: string -> Text.FontData
        W: int
        H: int
    }
    let private templates = System.Collections.Generic.Dictionary<string, string list * Map<string, Sx> * Sx>()

    /// Heads that build a layer (so `splitArgs` treats them as child layers). Excludes the
    /// document sections (sources/template/def/canvas/image) and adjustment names (those live
    /// inside `->` / `adjust`).
    let private layerKinds =
        set [ "stack"; "group"; "blend"; "screen"; "multiply"; "overlay"; "darken"; "lighten"
              "place"; "placed"; "mask"; "masked"; "clip"; "crop"; "raster"; "image"; "solid"
              "fill"; "gradient"; "linear"; "radial"; "shape"; "rect"; "circle"; "ellipse"
              "roundrect"; "text"; "adjust"; "adjust-below"; "->" ]

    let private splitArgs (items: Sx list) : Sx list * Map<string, Sx> * Sx list =
        let mutable pos = []
        let mutable kw = Map.empty
        let mutable kids = []
        let rec go items =
            match items with
            | [] -> ()
            | SAtom k :: v :: rest when k.StartsWith ":" -> kw <- Map.add (k.Substring 1) v kw; go rest
            | [ SAtom k ] when k.StartsWith ":" -> ()
            | (SList (SAtom h :: _) as node) :: rest when layerKinds.Contains h || templates.ContainsKey h ->
                kids <- kids @ [ node ]; go rest
            | other :: rest -> pos <- pos @ [ other ]; go rest
        go items
        (pos, kw, kids)

    // ── value readers (resolve $vars from the template env) ──
    let rec private deref (ctx: Ctx) (sx: Sx) : Sx =
        match sx with
        | SAtom a when a.StartsWith "$" ->
            match Map.tryFind (a.Substring 1) ctx.Env with
            | Some s -> deref ctx s
            | None -> failwithf "sexp: unbound $%s" (a.Substring 1)
        | _ -> sx
    let private atomStr ctx sx = match deref ctx sx with SAtom a -> a | SStr s -> s | _ -> failwithf "sexp: expected atom, got %A" sx
    let private vsym = atomStr
    let private vstr = atomStr
    let private num (s: string) = Double.Parse(s, CultureInfo.InvariantCulture)
    let private vfloat ctx sx = num (atomStr ctx sx)
    let private vint ctx sx = int (vfloat ctx sx)
    let private vbool ctx sx = match atomStr ctx sx with "true" -> true | _ -> false
    let private vcolor ctx sx =
        let s = (atomStr ctx sx).TrimStart('#')
        let b i = Convert.ToByte(s.Substring(i, 2), 16)
        if s.Length >= 8 then Color.rgba (b 0) (b 2) (b 4) (b 6)
        elif s.Length >= 6 then Color.rgb (b 0) (b 2) (b 4)
        else Color.rgb 255uy 255uy 255uy

    /// auto-detect a flat Param from a value sexp (for adjustment params).
    let private toParam ctx sx : Param =
        match deref ctx sx with
        | SStr s -> PStr s
        | SAtom a when a.StartsWith "#" -> PColor(vcolor ctx (SAtom a))
        | SAtom "true" -> PBool true
        | SAtom "false" -> PBool false
        | SAtom a ->
            match Double.TryParse(a, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, v -> PFloat v
            | _ -> PStr a
        | SList xs -> PList(xs |> List.map (fun x -> match Double.TryParse((match deref ctx x with SAtom a -> a | _ -> ""), NumberStyles.Float, CultureInfo.InvariantCulture) with true, v -> PFloat v | _ -> PStr "0"))
    let private kwParams ctx (kw: Map<string, Sx>) (drop: string list) : Params =
        kw |> Map.filter (fun k _ -> not (List.contains k drop)) |> Map.map (fun _ v -> toParam ctx v)

    let private parseBlend (s: string) : BlendMode =
        match s.ToLowerInvariant() with
        | "multiply" -> Multiply | "screen" -> Screen | "overlay" -> Overlay
        | "lighten" | "add" -> Lighten | "darken" -> Darken
        | "colordodge" | "dodge" -> ColorDodge | "colorburn" | "burn" -> ColorBurn
        | "difference" | "diff" -> Difference | "exclusion" -> Exclusion
        | "softlight" -> SoftLight | "hue" -> Hue | "saturation" -> Saturation | "luminosity" -> Luminosity
        | _ -> Normal

    let private vec2f ctx sx = match deref ctx sx with SList [ a; b ] -> vec2 (vfloat ctx a) (vfloat ctx b) | _ -> vec2 0.0 0.0

    // ── build ──
    let rec build (ctx: Ctx) (sx: Sx) : Layer =
        match sx with
        | SList (SAtom head :: tail) ->
            let pos, kw, kids = splitArgs tail
            let kwv k = Map.tryFind k kw
            let child0 () = build ctx (List.head kids)
            match head with
            | "stack" -> Stack(kids |> List.map (build ctx))
            | "group" ->
                let name = match pos with p :: _ -> vstr ctx p | [] -> (kwv "name" |> Option.map (vstr ctx) |> Option.defaultValue "group")
                let vis = kwv "visible" |> Option.map (vbool ctx) |> Option.defaultValue true
                let op = kwv "opacity" |> Option.map (vfloat ctx) |> Option.defaultValue 1.0
                Folder(name, vis, op, (match kids with [ k ] -> build ctx k | ks -> Stack(ks |> List.map (build ctx))))
            | "blend" -> Blended(parseBlend (vsym ctx pos.[0]), child0 ())
            | "screen" -> Blended(Screen, child0 ())
            | "multiply" -> Blended(Multiply, child0 ())
            | "overlay" -> Blended(Overlay, child0 ())
            | "darken" -> Blended(Darken, child0 ())
            | "lighten" -> Blended(Lighten, child0 ())
            | "place" | "placed" -> buildPlace ctx kw (child0 ())
            | "mask" | "masked" -> Masked(build ctx kids.[0], build ctx kids.[1])
            | "clip" | "crop" -> Clipped(buildRect ctx pos.[0], child0 ())
            | "raster" | "image" -> Bitmap(resolveSource ctx pos.[0])
            | "solid" | "fill" -> SolidFill(vcolor ctx (match pos with c :: _ -> c | [] -> kwv "color" |> Option.get))
            | "gradient" | "linear" | "radial" -> GradientFill(buildGradient ctx head pos)
            | "shape" | "rect" | "circle" | "ellipse" | "roundrect" -> buildShape ctx head pos kw
            | "text" -> buildText ctx pos kw
            | "adjust" -> Adjust({ Name = vsym ctx pos.[0]; Params = kwParams ctx kw [] }, child0 ())
            | "adjust-below" ->
                let region = kwv "region" |> Option.map (fun r -> rectPath (buildRect ctx r))
                AdjustBelow({ Name = vsym ctx pos.[0]; Params = kwParams ctx kw [ "region" ] }, region)
            | "->" -> buildPipe ctx tail
            | name when templates.ContainsKey name -> expand ctx name kw
            | other -> failwithf "sexp: unknown node '%s'" other
        | other -> failwithf "sexp: expected (node ...), got %A" other

    and buildPipe ctx (items: Sx list) : Layer =
        match items with
        | [] -> Empty
        | first :: adjs ->
            adjs |> List.fold (fun acc e ->
                match deref ctx e with
                | SList (SAtom name :: eargs) -> let _, ekw, _ = splitArgs eargs in Adjust({ Name = name; Params = kwParams ctx ekw [] }, acc)
                | SAtom name -> Adjust({ Name = name; Params = Map.empty }, acc)
                | _ -> acc) (build ctx first)

    and buildPlace ctx (kw: Map<string, Sx>) (child: Layer) : Layer =
        let g k def = match Map.tryFind k kw with Some v -> vfloat ctx v | None -> def
        let x, y, s, rot, op = g "x" 0.0, g "y" 0.0, g "scale" 1.0, g "rot" 0.0, g "opacity" 1.0
        let cx, cy = float ctx.W / 2.0, float ctx.H / 2.0
        let core = aboutPivot cx cy (Matrix3x2.multiply (Matrix3x2.scale s s) (Matrix3x2.rotate rot))
        Placed(child, { M = Matrix3x2.multiply core (Matrix3x2.translate x y); Opacity = op })

    and buildRect ctx sx : Rect =
        match deref ctx sx with
        | SList (SAtom "rect" :: a) -> let p, _, _ = splitArgs a in rect (vfloat ctx p.[0]) (vfloat ctx p.[1]) (vfloat ctx p.[2]) (vfloat ctx p.[3])
        | SList ps when ps.Length >= 4 -> rect (vfloat ctx ps.[0]) (vfloat ctx ps.[1]) (vfloat ctx ps.[2]) (vfloat ctx ps.[3])
        | _ -> rect 0.0 0.0 (float ctx.W) (float ctx.H)

    and buildShape ctx head (pos: Sx list) (kw: Map<string, Sx>) : Layer =
        let fill = match Map.tryFind "fill" kw with Some c -> Solid(vcolor ctx c) | None -> Solid Color.white
        let segs =
            match head with
            | "rect" -> rectPath (rect (vfloat ctx pos.[0]) (vfloat ctx pos.[1]) (vfloat ctx pos.[2]) (vfloat ctx pos.[3]))
            | "roundrect" -> Raster.roundedRectPath (rect (vfloat ctx pos.[0]) (vfloat ctx pos.[1]) (vfloat ctx pos.[2]) (vfloat ctx pos.[3])) (vfloat ctx pos.[4] * 1.0<px>)
            | "circle" -> let r = vfloat ctx pos.[2] in Raster.ellipsePath (vfloat ctx pos.[0]) (vfloat ctx pos.[1]) r r
            | "ellipse" -> Raster.ellipsePath (vfloat ctx pos.[0]) (vfloat ctx pos.[1]) (vfloat ctx pos.[2]) (vfloat ctx pos.[3])
            | _ -> rectPath (rect 0.0 0.0 (float ctx.W) (float ctx.H))
        Shape(segs, fill, NonZero)

    and buildText ctx (pos: Sx list) (kw: Map<string, Sx>) : Layer =
        let str = match pos with p :: _ -> vstr ctx p | [] -> (kw |> Map.tryFind "text" |> Option.map (vstr ctx) |> Option.defaultValue "")
        let g k def = match Map.tryFind k kw with Some v -> vfloat ctx v | None -> def
        Caption { Text = str
                  Size = g "size" 64.0
                  Fill = (match Map.tryFind "fill" kw with Some c -> vcolor ctx c | None -> Color.white)
                  X = g "x" 0.0; Y = g "y" 0.0
                  Bold = (match Map.tryFind "bold" kw with Some v -> vbool ctx v | None -> true)
                  Font = Map.tryFind "font" kw |> Option.map (vstr ctx >> ctx.ResolveFont) }

    and buildGradient ctx head (pos: Sx list) : Paint =
        let colors = pos |> List.choose (fun p -> match deref ctx p with SAtom a when a.StartsWith "#" -> Some(vcolor ctx (SAtom a)) | _ -> None)
        let stops = colors |> List.mapi (fun i c -> { Offset = (if colors.Length <= 1 then 0.0 else float i / float (colors.Length - 1)); StopColor = c })
        match head with
        | "radial" ->
            // `(radial (cx cy) [radius] #c0 #c1 …)` — center tuple, optional numeric radius, colours.
            let center = pos |> List.tryPick (fun p -> match deref ctx p with SList _ -> Some(vec2f ctx p) | _ -> None)
                             |> Option.defaultValue (vec2 (float ctx.W / 2.0) (float ctx.H / 2.0))
            let r = pos |> List.tryPick (fun p -> match deref ctx p with SAtom a when not (a.StartsWith "#") -> Some(num a) | _ -> None)
                        |> Option.defaultValue ((sqrt (float ctx.W * float ctx.W + float ctx.H * float ctx.H)) / 2.0)
            RadialGradient(center, r * 1.0<px>, stops, Pad)
        | "linear" ->
            let pts = pos |> List.choose (fun p -> match deref ctx p with SList _ -> Some(vec2f ctx p) | _ -> None)
            let p0 = match pts with a :: _ -> a | [] -> vec2 0.0 0.0
            let p1 = match pts with _ :: b :: _ -> b | _ -> vec2 0.0 (float ctx.H)
            LinearGradient(p0, p1, stops, Pad)
        | _ -> LinearGradient(vec2 0.0 0.0, vec2 0.0 (float ctx.H), stops, Pad)   // "gradient" = vertical

    and resolveSource ctx sx : ImageRef =
        let name = vsym ctx sx
        let path = match Map.tryFind name ctx.Sources with Some p -> p | None -> name
        ctx.ResolveSource path

    // expand a template by substitution (eager): bind args into the body's $params, then build.
    and expand ctx name (kw: Map<string, Sx>) : Layer =
        match templates.TryGetValue name with
        | true, (_, defaults, body) ->
            let env = Map.fold (fun acc k v -> Map.add k v acc) defaults kw
            let keyParam =
                [ "title"; "name"; "label"; "text" ]
                |> List.tryPick (fun k -> Map.tryFind k kw |> Option.map (fun v -> match v with SStr s -> s | SAtom a -> a | _ -> ""))
            let label = match keyParam with Some s when s <> "" -> sprintf "%s · %s" name s | _ -> name
            Folder(label, true, 1.0, build { ctx with Env = env } body)
        | _ -> failwithf "sexp: unknown template '%s'" name

    // ── document ──
    let private parseParams (sx: Sx) : string list * Map<string, Sx> =
        match sx with
        | SList items ->
            let mutable names = []
            let mutable defs = Map.empty
            for it in items do
                match it with
                | SAtom nm -> names <- names @ [ nm ]
                | SList [ SAtom nm; d ] -> names <- names @ [ nm ]; defs <- Map.add nm d defs
                | _ -> ()
            names, defs
        | _ -> [], Map.empty

    let loadDocWithAssets
        (resolveSource: string -> ImageRef)
        (resolveFont: string -> Text.FontData)
        (text: string)
        : ImageConfig * Layer =
        templates.Clear()
        match parseAll text with
        | [ SList (SAtom "image" :: SList dims :: rest) ] ->
            let w = match dims with a :: _ -> int (num (match a with SAtom s -> s | _ -> "1280")) | _ -> 1280
            let h = match dims with _ :: b :: _ -> int (num (match b with SAtom s -> s | _ -> "720")) | _ -> 720
            let pos, kw, _ = splitArgs rest
            let kwS k = Map.tryFind k kw |> Option.map (fun v -> match v with SStr s -> s | SAtom s -> s | _ -> "")
            let bg = match Map.tryFind "bg" kw with
                     | Some v -> let s = (match v with SAtom a -> a | _ -> "#00000000").TrimStart('#')
                                 let b i = Convert.ToByte(s.Substring(i, 2), 16)
                                 if s.Length >= 8 then Color.rgba (b 0) (b 2) (b 4) (b 6)
                                 elif s.Length >= 6 then Color.rgb (b 0) (b 2) (b 4) else Color.rgba 0uy 0uy 0uy 0uy
                     | None -> Color.rgba 0uy 0uy 0uy 0uy
            let out = kwS "out" |> Option.defaultValue "out/image.png"
            let sections = pos |> List.choose (function SList (SAtom h :: t) -> Some(h, t) | _ -> None)
            // pass 1: sources + templates
            let mutable sources = Map.empty
            for (head, t) in sections do
                match head with
                | "sources" ->
                    for a in t do
                        match a with
                        | SList [ SAtom nm; SStr path ] -> sources <- Map.add nm path sources
                        | _ -> ()
                | "template" ->
                    match t with SAtom nm :: ps :: body :: _ -> let names, defs = parseParams ps in templates.[nm] <- (names, defs, body) | _ -> ()
                | "def" ->
                    match t with SAtom nm :: body :: _ -> templates.[nm] <- ([], Map.empty, body) | _ -> ()
                | _ -> ()
            // pass 2: canvas
            let ctx = { Env = Map.empty; Sources = sources; ResolveSource = resolveSource; ResolveFont = resolveFont; W = w; H = h }
            let root =
                sections |> List.tryPick (fun (head, t) -> if head = "canvas" then Some(build ctx (List.head t)) else None)
                |> Option.defaultWith (fun () -> failwith "sexp: no (canvas ...) section")
            { W = w; H = h; Bg = bg; Out = out }, root
        | _ -> failwith "sexp: document must be a single (image (W H) ...) form"

    let loadDocWith (resolveSource: string -> ImageRef) (text: string) : ImageConfig * Layer =
        loadDocWithAssets resolveSource Font.load text

    let loadDoc (text: string) : ImageConfig * Layer =
        loadDocWith (fun path -> { Path = path; Fb = Image.loadCached path }) text
