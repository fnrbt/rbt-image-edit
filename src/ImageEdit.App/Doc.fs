module ImageEdit.App.Doc

// Structural edits over the immutable `Layer` document, and a flattened view of the top-level
// layer stack for the Layers panel. Every edit returns a NEW doc (no mutation).

open Renderer
open ImageEdit

/// A row in the Layers panel — derived from a top-level `Folder` in the root `Stack`.
type LayerInfo = { Index: int; Name: string; Visible: bool; Opacity: float; Blend: string }

let private nameOf (l: Layer) : string =
    match l with
    | Bitmap r -> System.IO.Path.GetFileName r.Path
    | SolidFill _ -> "fill"
    | GradientFill _ -> "gradient"
    | Caption t -> "text: " + t.Text
    | Shape _ -> "shape"
    | Adjust (a, _) -> "adjust: " + a.Name
    | AdjustBelow (a, _) -> "adjust: " + a.Name
    | Stack _ -> "group"
    | Masked _ -> "masked"
    | Blended (_, _) -> "layer"
    | _ -> "layer"

let blendModes = [ Normal; Multiply; Screen; Overlay; Darken; Lighten ]
let blendName (m: BlendMode) =
    match m with
    | Normal -> "normal" | Multiply -> "multiply" | Screen -> "screen"
    | Overlay -> "overlay" | Darken -> "darken" | Lighten -> "lighten" | other -> (sprintf "%A" other).ToLowerInvariant()

let private contentBlend (content: Layer) = match content with Blended (m, _) -> m | _ -> Normal

/// Normalize a loaded doc so the root is a `Stack` of `Folder`s (each a panel-editable layer).
let normalize (root: Layer) : Layer =
    let wrap i (k: Layer) =
        match k with
        | Folder _ -> k
        | _ -> Folder(sprintf "%d · %s" i (nameOf k), true, 1.0, k)
    match root with
    | Stack kids -> Stack(kids |> List.mapi wrap)
    | other -> Stack [ wrap 0 other ]

/// Top-level layers, TOP of the stack first (panel order).
let layers (root: Layer) : LayerInfo list =
    match root with
    | Stack kids ->
        kids
        |> List.mapi (fun i k ->
            match k with
            | Folder (name, vis, op, content) ->
                { Index = i; Name = name; Visible = vis; Opacity = op; Blend = blendName (contentBlend content) }
            | other -> { Index = i; Name = nameOf other; Visible = true; Opacity = 1.0; Blend = "normal" })
        |> List.rev
    | _ -> []

let private mapChild (i: int) (f: Layer -> Layer) (root: Layer) : Layer =
    match root with
    | Stack kids -> Stack(kids |> List.mapi (fun j k -> if j = i then f k else k))
    | other -> other

let setVisible i v root = mapChild i (function Folder (n, _, o, c) -> Folder(n, v, o, c) | k -> k) root
let setOpacity i o root = mapChild i (function Folder (n, vi, _, c) -> Folder(n, vi, max 0.0 (min 1.0 o), c) | k -> k) root

/// Cycle a layer's blend mode through `blendModes` (Normal unwraps the `Blended`).
let cycleBlend i root =
    mapChild i
        (function
         | Folder (n, vi, o, content) ->
             let cur = contentBlend content
             let inner = match content with Blended (_, c) -> c | c -> c
             let next = blendModes.[(List.findIndex ((=) cur) blendModes + 1) % blendModes.Length]
             let content' = if next = Normal then inner else Blended(next, inner)
             Folder(n, vi, o, content')
         | k -> k)
        root

/// Move layer at panel position toward top/bottom of the stack.
let moveLayer (fromIdx: int) (toIdx: int) (root: Layer) : Layer =
    match root with
    | Stack kids when fromIdx >= 0 && fromIdx < kids.Length && toIdx >= 0 && toIdx < kids.Length ->
        let arr = ResizeArray(kids)
        let item = arr.[fromIdx]
        arr.RemoveAt fromIdx
        arr.Insert(toIdx, item)
        Stack(List.ofSeq arr)
    | _ -> root

let layerCount (root: Layer) = match root with Stack kids -> kids.Length | _ -> 1

/// Wrap a layer's content in an adjustment (non-destructive). Used by the adjustment dialogs.
let applyAdjust (idx: int) (ref: AdjustRef) (root: Layer) : Layer =
    mapChild idx (function Folder (n, vi, o, content) -> Folder(n, vi, o, Adjust(ref, content)) | k -> k) root

// ── tool edits ───────────────────────────────────────────
/// Append a new top-of-stack layer (wrapped as a Folder).
let appendLayer (name: string) (content: Layer) (root: Layer) : Layer =
    let folder = Folder(name, true, 1.0, content)
    match root with Stack kids -> Stack(kids @ [ folder ]) | other -> Stack [ other; folder ]

/// Set a layer's translation (non-destructive `Placed` wrapper, replacing any prior translate).
let setTranslate (idx: int) (tx: float) (ty: float) (root: Layer) : Layer =
    mapChild idx
        (function
         | Folder (n, vi, o, content) ->
             let baseContent = match content with Placed (inner, _) -> inner | c -> c
             Folder(n, vi, o, Placed(baseContent, { M = Matrix3x2.translate tx ty; Opacity = 1.0 }))
         | k -> k)
        root

/// Current translation of a layer (0,0 if none).
let getTranslate (idx: int) (root: Layer) : float * float =
    match root with
    | Stack kids when idx >= 0 && idx < kids.Length ->
        match kids.[idx] with Folder (_, _, _, Placed (_, xf)) -> xf.M.DX, xf.M.DY | _ -> 0.0, 0.0
    | _ -> 0.0, 0.0

/// Remove the layer at a stack index.
let removeLayer (idx: int) (root: Layer) : Layer =
    match root with
    | Stack kids when idx >= 0 && idx < kids.Length ->
        Stack(kids |> List.indexed |> List.filter (fun (j, _) -> j <> idx) |> List.map snd)
    | _ -> root

/// Duplicate the layer at a stack index (the copy sits just above it).
let duplicateLayer (idx: int) (root: Layer) : Layer =
    match root with
    | Stack kids when idx >= 0 && idx < kids.Length ->
        Stack(kids |> List.indexed |> List.collect (fun (j, k) -> if j = idx then [ k; k ] else [ k ]))
    | _ -> root

let textLayer (x: float) (y: float) (str: string) : Layer =
    Caption { Text = str; Size = 72.0; Fill = Color.white; X = x; Y = y; Bold = true; Font = None }

/// Text layer with explicit size / colour / weight (driven by the Text tool options).
let textLayerEx (x: float) (y: float) (str: string) (size: float) (color: Color) (bold: bool) : Layer =
    Caption { Text = str; Size = size; Fill = color; X = x; Y = y; Bold = bold; Font = None }

let shapeRectLayer (r: Rect) (color: Color) : Layer = Shape(rectPath r, Solid color, NonZero)

/// A filled ellipse inscribed in the dragged rect.
let shapeEllipseLayer (r: Rect) (color: Color) : Layer =
    let x, y, w, h = float r.Pos.X, float r.Pos.Y, float r.Size.X, float r.Size.Y
    Shape(Raster.ellipsePath (x + w / 2.0) (y + h / 2.0) (w / 2.0) (h / 2.0), Solid color, EvenOdd)

/// A linear-gradient fill clipped to the dragged rect (horizontal or vertical).
let gradientLayer (r: Rect) (c0: Color) (c1: Color) (horizontal: bool) : Layer =
    let x, y, w, h = float r.Pos.X, float r.Pos.Y, float r.Size.X, float r.Size.Y
    let p0, p1 =
        if horizontal then vec2 x (y + h / 2.0), vec2 (x + w) (y + h / 2.0)
        else vec2 (x + w / 2.0) y, vec2 (x + w / 2.0) (y + h)
    let stops = [ { Offset = 0.0; StopColor = c0 }; { Offset = 1.0; StopColor = c1 } ]
    Shape(rectPath r, LinearGradient(p0, p1, stops, Pad), NonZero)
