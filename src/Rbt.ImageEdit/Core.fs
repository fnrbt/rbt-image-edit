namespace Rbt.ImageEdit

open Rbt.Renderer

/// Shared foundations for the image editor: canvas geometry, the serializable `Param` type, the
/// effect/template reference records, fit/rect helpers, and a small font cache. Mirrors
/// `VideoEdit.Core`, minus the time dimension — an image has a 2D canvas, not a timeline.
[<AutoOpen>]
module Core =

    /// The canvas geometry, in pixels. Threaded through evaluation so full-canvas fills and
    /// centred graphics know how big the document is.
    type Frame = { W: float; H: float }

    /// A serializable parameter value for name-resolved adjustments / templates. Keeping params as
    /// pure data is what lets the layer AST diff into human-readable edits (see `Diff.fs`).
    type Param =
        | PFloat of float
        | PInt of int
        | PBool of bool
        | PColor of Color
        | PStr of string
        | PList of Param list

    type Params = Map<string, Param>

    let pmap (xs: (string * Param) list) : Params = Map.ofList xs

    let pfloat (ps: Params) (key: string) (def: float) =
        match Map.tryFind key ps with
        | Some (PFloat v) -> v
        | Some (PInt v) -> float v
        | _ -> def

    let pint (ps: Params) (key: string) (def: int) =
        match Map.tryFind key ps with
        | Some (PInt v) -> v
        | Some (PFloat v) -> int v
        | _ -> def

    let pbool (ps: Params) (key: string) (def: bool) =
        match Map.tryFind key ps with Some (PBool v) -> v | _ -> def

    let pstr (ps: Params) (key: string) (def: string) =
        match Map.tryFind key ps with Some (PStr v) -> v | _ -> def

    let pcolor (ps: Params) (key: string) (def: Color) =
        match Map.tryFind key ps with Some (PColor v) -> v | _ -> def

    let plist (xs: Param list) : Param = PList xs

    /// Read a list of floats from a PList param (e.g. curve control points).
    let pfloats (ps: Params) (key: string) (def: float list) =
        match Map.tryFind key ps with
        | Some (PList xs) -> xs |> List.choose (function PFloat v -> Some v | PInt v -> Some(float v) | _ -> None)
        | _ -> def

    /// Human-readable rendering of a Param (for diffs / inspectors).
    let rec showParam (p: Param) : string =
        match p with
        | PFloat f -> sprintf "%.4g" f
        | PInt i -> string i
        | PBool b -> string b
        | PStr s -> "\"" + s + "\""
        | PColor c -> sprintf "#%02X%02X%02X" c.R c.G c.B
        | PList xs -> "[" + String.concat ", " (List.map showParam xs) + "]"

    /// Serializable reference to a named adjustment/filter. Identity is the name; the actual pixel
    /// implementation is resolved by `Adjustments.lower` (a plain function, not a string registry).
    type AdjustRef = { Name: string; Params: Params }

    /// Reference to a named layer template (data-driven instance / "smart object").
    type InstRef = { Name: string; Params: Params }

    /// Compose a 2D transform `m` so it is applied about pivot (px,py) rather than the origin —
    /// used for scale/rotate about the canvas centre.
    let aboutPivot (px: float) (py: float) (m: Matrix3x2) : Matrix3x2 =
        Matrix3x2.multiply (Matrix3x2.multiply (Matrix3x2.translate -px -py) m) (Matrix3x2.translate px py)

    /// How a source image maps onto a destination rect.
    type Fit = Cover | Contain | Stretch

    /// Destination rect for fitting a srcW×srcH image into the frame.
    let fitRect (frame: Frame) (srcW: float) (srcH: float) (fit: Fit) : Rect =
        match fit with
        | Stretch -> rect 0.0 0.0 frame.W frame.H
        | _ ->
            let pick = if fit = Cover then max else min
            let scl = pick (frame.W / srcW) (frame.H / srcH)
            let w = srcW * scl
            let h = srcH * scl
            rect ((frame.W - w) / 2.0) ((frame.H - h) / 2.0) w h

    /// A closed rectangle as a path (for gradient fills and clip regions).
    let rectPath (r: Rect) : PathSeg list =
        let x0 = float r.Pos.X
        let y0 = float r.Pos.Y
        let x1 = x0 + float r.Size.X
        let y1 = y0 + float r.Size.Y
        [ MoveTo (vec2 x0 y0); LineTo (vec2 x1 y0); LineTo (vec2 x1 y1); LineTo (vec2 x0 y1); Close ]

    /// Wrap vector content (shapes / text) in a 4× antialias pass.
    let aa (cmds: Cmd list) : Cmd list =
        match cmds with [] -> [] | _ -> [ AntiAlias(4, cmds) ]

    /// Font loading + cache, keyed by path. Image text layers + the editor UI both go through this.
    module Font =
        let private cache = System.Collections.Generic.Dictionary<string, Text.FontData>()
        let load (path: string) : Text.FontData =
            match cache.TryGetValue path with
            | true, f -> f
            | _ -> let f = Text.loadFontFile path in cache.[path] <- f; f
        /// Load an Rbt.OpenType font supplied by the embedding application.
        /// Rbt.Renderer caches decoded font data by content hash. WOFF2 remains
        /// suitable for browser/SVG embedding, but transformed webfont glyphs
        /// are not yet a reliable Rbt.Renderer input; use original TTF/OTF bytes.
        let loadBytes (bytes: byte[]) : Text.FontData =
            if bytes.Length >= 4 && bytes.[0] = byte 'w' && bytes.[1] = byte 'O' && bytes.[2] = byte 'F' && bytes.[3] = byte '2' then
                invalidArg "bytes" "Rbt.Renderer requires TTF/OTF font bytes; keep WOFF2 for SVG/browser embedding"
            Text.loadFont bytes
        let private firstAvailable kind paths =
            paths
            |> List.tryFind System.IO.File.Exists
            |> Option.map load
            |> Option.defaultWith (fun () ->
                failwithf "Rbt.ImageEdit could not find a %s font. Tried: %s" kind (System.String.Join(", ", paths)))
        let sans () =
            firstAvailable "sans-serif" [
                "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
                "/renderer/NotoSans-Regular.ttf"
            ]
        let sansBold () =
            firstAvailable "bold sans-serif" [
                "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf"
                "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"
                "/usr/share/fonts/truetype/noto/NotoSans-Bold.ttf"
                "/renderer/NotoSans-Regular.ttf"
            ]
