namespace ImageEdit

open Renderer

/// The layer algebra. Structural combinators are a closed DU; adjustments are OPEN — referenced by
/// serializable name+params (`AdjustRef`) and lowered to renderer filters at eval time. `eval`
/// composites a layer tree into a renderer `Cmd list`. The spine is `Stack` (bottom→top
/// compositing) — the image-editing analog of the video timeline's temporal `Concat`. There is no
/// time: everything is a pure function of the canvas geometry.
[<AutoOpen>]
module Layers =

    /// A decoded raster source: original path (for diffs / reload) + its premultiplied pixels.
    type ImageRef = { Path: string; Fb: Framebuffer }

    /// A text layer.
    type TextSpec = { Text: string; Size: float; Fill: Color; X: float; Y: float; Bold: bool; Font: Text.FontData option }

    /// A static 2D transform of a whole layer (move / scale / rotate) + layer opacity.
    type Xf2 = { M: Matrix3x2; Opacity: float }
    let idXf2 = { M = Matrix3x2.identity; Opacity = 1.0 }

    type Layer =
        // leaves / sources
        | Empty                                              // transparent — identity for Stack
        | SolidFill of Color                                 // fills the whole canvas
        | GradientFill of Paint                              // fills the whole canvas with a gradient
        | Bitmap of ImageRef                                 // a decoded image, cover-fit to the canvas
        | Shape of PathSeg list * Paint * FillRule           // a vector fill
        | Caption of TextSpec                                // text
        // structural combinators (closed core)
        | Stack of Layer list                                // bottom → top compositing  (the spine)
        | Folder of name: string * visible: bool * opacity: float * Layer   // a layer group
        | Blended of BlendMode * Layer                       // composite onto what's beneath
        | Placed of Layer * Xf2                              // move / scale / rotate / opacity
        | Masked of content: Layer * mask: Layer             // mask layer modulates content's alpha
        | Clipped of Rect * Layer                            // crop to a rect
        // open, non-destructive extension points (name+params → diffable, chat-editable)
        | Adjust of AdjustRef * Layer                        // adjustment clipped to THIS layer
        | AdjustBelow of AdjustRef * region: PathSeg list option   // adjustment LAYER (affects everything beneath)

    /// Composite a layer tree at canvas size `frame` into renderer commands. `Stack` paints
    /// bottom→top, so an `AdjustBelow` re-processes the already-drawn backdrop — exactly the
    /// Photoshop adjustment-layer semantic, via the renderer's `BackdropFilter`. (Cmd union cases
    /// `Group`/`Blend`/`Mask`/`Clip` are qualified to keep them distinct from layer cases.)
    let eval (frame: Frame) (root: Layer) : Cmd list =
        let full = rectPath (rect 0.0 0.0 frame.W frame.H)
        let rec go (l: Layer) : Cmd list =
            match l with
            | Empty -> []
            | SolidFill c -> [ FillRect(rect 0.0 0.0 frame.W frame.H, c) ]
            | GradientFill paint -> [ FillPath(full, paint, NonZero) ]
            | Bitmap img ->
                let dst = fitRect frame (float img.Fb.W) (float img.Fb.H) Cover
                [ DrawImage(img.Fb, rect 0.0 0.0 (float img.Fb.W) (float img.Fb.H), dst, true) ]
            | Shape (segs, paint, rule) -> aa [ FillPath(segs, paint, rule) ]
            | Caption t ->
                let font = t.Font |> Option.defaultWith (fun () -> if t.Bold then Font.sansBold () else Font.sans ())
                aa [ DrawGlyphs(vec2 t.X t.Y, Text.layout font t.Size t.Text, Solid t.Fill) ]
            | Stack cs -> cs |> List.collect go
            | Folder (_, visible, opacity, inner) ->
                if not visible then []
                else match go inner with [] -> [] | cs -> [ Cmd.Group(Matrix3x2.identity, max 0.0 (min 1.0 opacity), cs) ]
            | Blended (mode, inner) ->
                match go inner with [] -> [] | cs -> [ Cmd.Blend(mode, cs) ]
            | Placed (inner, xf) ->
                match go inner with [] -> [] | cs -> [ Cmd.Group(xf.M, max 0.0 (min 1.0 xf.Opacity), cs) ]
            | Masked (content, mask) ->
                match go content with [] -> [] | cs -> [ Cmd.Mask(go mask, cs) ]
            | Clipped (r, inner) ->
                match go inner with [] -> [] | cs -> [ Cmd.Clip(rectPath r, NonZero, cs) ]
            | Adjust (ref, inner) ->
                match go inner with [] -> [] | cs -> [ Filtered([ Adjustments.lower frame ref ], cs) ]
            | AdjustBelow (ref, region) ->
                let clip = match region with Some p -> p | None -> full
                [ BackdropFilter(clip, NonZero, [ Adjustments.lower frame ref ], []) ]
        go root
