namespace Rbt.ImageEdit

open Rbt.Renderer

/// Thin smart-constructors over the layer algebra, for authoring documents in F# (and for tests).
/// The S-expression front-end (`Sexp.fs`) is the primary authoring surface; this is the
/// programmatic mirror.
[<AutoOpen>]
module Dsl =

    let solid (c: Color) : Layer = SolidFill c
    let bitmap (img: ImageRef) : Layer = Bitmap img
    let stack (cs: Layer list) : Layer = Stack cs
    let folder (name: string) (inner: Layer) : Layer = Folder(name, true, 1.0, inner)
    let blend (mode: BlendMode) (l: Layer) : Layer = Blended(mode, l)
    let screen l = Blended(Screen, l)
    let multiply l = Blended(Multiply, l)
    let overlay l = Blended(Overlay, l)
    /// Mask `l` by mask layer `m` (m's luminance/alpha modulates l).
    let mask (m: Layer) (l: Layer) : Layer = Masked(l, m)
    let crop (r: Rect) (l: Layer) : Layer = Clipped(r, l)
    let adjust (name: string) (ps: (string * Param) list) (l: Layer) : Layer = Adjust({ Name = name; Params = pmap ps }, l)
    let adjustBelow (name: string) (ps: (string * Param) list) : Layer = AdjustBelow({ Name = name; Params = pmap ps }, None)
    /// Chain adjustments over a layer: `pipe img [ "curves", [...]; "saturate", ["amount", PFloat 1.1] ]`.
    let pipe (l: Layer) (adjs: (string * (string * Param) list) list) : Layer =
        adjs |> List.fold (fun acc (name, ps) -> Adjust({ Name = name; Params = pmap ps }, acc)) l

    let place (x: float) (y: float) (l: Layer) : Layer = Placed(l, { idXf2 with M = Matrix3x2.translate x y })
    let opacity (o: float) (l: Layer) : Layer = Placed(l, { idXf2 with Opacity = o })
    let text (str: string) (size: float) (fill: Color) (x: float) (y: float) : Layer =
        Caption { Text = str; Size = size; Fill = fill; X = x; Y = y; Bold = true; Font = None }
