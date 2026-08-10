namespace ImageEdit

open Renderer

/// The library of non-destructive adjustments / filters. `lower` maps a serializable `AdjustRef`
/// (name + params, straight from the document) to a renderer `Filter`. Simple colour/tone ops reuse
/// the renderer's built-in CPU filters; the Photoshop-specific ones (levels, curves, sharpen,
/// threshold, posterize) are `Filter.Custom` pixel passes whose implementation lives HERE, not in
/// the shared renderer. The mapping is a plain `match` — no string-keyed registry; names never
/// cross into the renderer (only the chosen `Filter` does).
module Adjustments =

    let private clampB (x: float) : byte = byte (max 0.0 (min 255.0 (round x)))

    /// Apply three per-channel 256-entry LUTs to every pixel, operating in straight colour space so
    /// premultiplied / semi-transparent pixels stay correct.
    let private lut3 (lr: byte[]) (lg: byte[]) (lb: byte[]) : PixelPass =
        fun _w _h px ->
            for i in 0 .. px.Length - 1 do
                if Pixel.a px.[i] <> 0uy then
                    let c = Color.fromPremul (Pixel.toColor px.[i])
                    px.[i] <- Pixel.fromColorPremul { c with R = lr.[int c.R]; G = lg.[int c.G]; B = lb.[int c.B] }
            px

    /// levels: input black/white points + gamma → an output LUT.
    let private levelsLut (black: float) (white: float) (gamma: float) : byte[] =
        let lo = black
        let hi = if white <= black then black + 1.0 else white
        let g = if gamma <= 0.0 then 1.0 else gamma
        Array.init 256 (fun v ->
            let t = max 0.0 (min 1.0 ((float v - lo) / (hi - lo)))
            clampB (t ** (1.0 / g) * 255.0))

    /// curves: a piecewise-linear LUT through sorted (x,y) control points in 0..255.
    let private curvesLut (pts: (float * float) list) : byte[] =
        let arr = (if pts.IsEmpty then [ 0.0, 0.0; 255.0, 255.0 ] else pts) |> List.sortBy fst |> List.toArray
        Array.init 256 (fun v ->
            let x = float v
            let mutable j = 0
            while j < arr.Length - 1 && fst arr.[j + 1] < x do j <- j + 1
            let (x0, y0) = arr.[min (arr.Length - 1) j]
            let (x1, y1) = arr.[min (arr.Length - 1) (j + 1)]
            clampB (if x1 = x0 then y0 else y0 + (y1 - y0) * (x - x0) / (x1 - x0)))

    let private posterizeLut (levels: int) : byte[] =
        let step = float (max 2 levels - 1)
        Array.init 256 (fun v -> clampB (round (float v / 255.0 * step) / step * 255.0))

    let private thresholdPass (level: float) : PixelPass =
        fun _w _h px ->
            for i in 0 .. px.Length - 1 do
                if Pixel.a px.[i] <> 0uy then
                    let c = Color.fromPremul (Pixel.toColor px.[i])
                    let lum = 0.299 * float c.R + 0.587 * float c.G + 0.114 * float c.B
                    let v = if lum >= level then 255uy else 0uy
                    px.[i] <- Pixel.fromColorPremul { c with R = v; G = v; B = v }
            px

    /// 3×3 unsharp kernel: out = c·(1+4a) − a·(up+down+left+right). Operates on a snapshot so the
    /// pass doesn't feed back on itself.
    let private sharpenPass (amount: float) : PixelPass =
        fun w h px ->
            let src = Array.copy px
            let straight i = Color.fromPremul (Pixel.toColor src.[i])
            let at x y = straight ((min (h - 1) (max 0 y)) * w + (min (w - 1) (max 0 x)))
            for y in 0 .. h - 1 do
                for x in 0 .. w - 1 do
                    let i = y * w + x
                    if Pixel.a src.[i] <> 0uy then
                        let c = straight i
                        let u, d, l, r = at x (y - 1), at x (y + 1), at (x - 1) y, at (x + 1) y
                        let chan (cc: byte) (uu: byte) (dd: byte) (ll: byte) (rr: byte) =
                            clampB (float cc * (1.0 + 4.0 * amount) - amount * (float uu + float dd + float ll + float rr))
                        px.[i] <- Pixel.fromColorPremul
                                    { c with R = chan c.R u.R d.R l.R r.R
                                             G = chan c.G u.G d.G l.G r.G
                                             B = chan c.B u.B d.B l.B r.B }
            px

    let private lum (c: Color) = 0.299 * float c.R + 0.587 * float c.G + 0.114 * float c.B

    /// vignette: darken toward the corners. `amount` = strength, `radius` = where falloff starts
    /// (0..1 of the centre→corner distance). A spatial pass — uses the pixel's (x,y).
    let private vignettePass (amount: float) (radius: float) : PixelPass =
        fun w h px ->
            let cx, cy = float w / 2.0, float h / 2.0
            let maxd = sqrt (cx * cx + cy * cy)
            for y in 0 .. h - 1 do
                for x in 0 .. w - 1 do
                    let i = y * w + x
                    if Pixel.a px.[i] <> 0uy then
                        let dx, dy = float x - cx, float y - cy
                        let d = sqrt (dx * dx + dy * dy) / maxd
                        let t = max 0.0 (min 1.0 ((d - radius) / (1.0 - radius)))
                        let f = 1.0 - amount * t * t
                        let c = Color.fromPremul (Pixel.toColor px.[i])
                        px.[i] <- Pixel.fromColorPremul { c with R = clampB (float c.R * f); G = clampB (float c.G * f); B = clampB (float c.B * f) }
            px

    /// monochrome film grain (deterministic seed so renders are reproducible).
    let private grainPass (amount: float) : PixelPass =
        fun _w _h px ->
            let rnd = System.Random(20260615)
            let mag = amount * 48.0
            for i in 0 .. px.Length - 1 do
                if Pixel.a px.[i] <> 0uy then
                    let n = (rnd.NextDouble() - 0.5) * 2.0 * mag
                    let c = Color.fromPremul (Pixel.toColor px.[i])
                    px.[i] <- Pixel.fromColorPremul { c with R = clampB (float c.R + n); G = clampB (float c.G + n); B = clampB (float c.B + n) }
            px

    /// duotone / gradient-map: map luminance onto a shadow→highlight colour ramp.
    let private duotonePass (lo: Color) (hi: Color) : PixelPass =
        fun _w _h px ->
            let mix (a: byte) (b: byte) (t: float) = clampB (float a + (float b - float a) * t)
            for i in 0 .. px.Length - 1 do
                if Pixel.a px.[i] <> 0uy then
                    let c = Color.fromPremul (Pixel.toColor px.[i])
                    let t = lum c / 255.0
                    px.[i] <- Pixel.fromColorPremul { c with R = mix lo.R hi.R t; G = mix lo.G hi.G t; B = mix lo.B hi.B t }
            px

    /// Map a document adjustment to a renderer filter. Unknown names lower to identity so a
    /// document never crashes the renderer.
    let lower (_frame: Frame) (ref: AdjustRef) : Filter =
        let ps = ref.Params
        match ref.Name with
        // ── reuse the renderer's built-in CPU filters ──
        | "blur" -> Blur (pint ps "radius" 8)
        | "saturate" | "saturation" -> Saturate (pfloat ps "amount" 1.4)
        | "brightness" -> Brightness (pfloat ps "amount" 1.15)
        | "contrast" -> Contrast (pfloat ps "amount" 1.2)
        | "grayscale" | "desaturate" -> Grayscale (pfloat ps "amount" 1.0)
        | "sepia" -> Sepia (pfloat ps "amount" 1.0)
        | "invert" -> Invert (pfloat ps "amount" 1.0)
        | "hue" | "hueRotate" -> HueRotate (pfloat ps "degrees" 0.0 * System.Math.PI / 180.0)
        | "opacity" -> OpacityFilter (pfloat ps "alpha" 1.0)
        // ── Photoshop-specific passes, supplied directly as callbacks ──
        | "levels" ->
            let lut = levelsLut (pfloat ps "black" 0.0) (pfloat ps "white" 255.0) (pfloat ps "gamma" 1.0)
            Custom (lut3 lut lut lut)
        | "curves" ->
            let pts = pfloats ps "points" [ 0.0; 0.0; 255.0; 255.0 ] |> List.chunkBySize 2
                      |> List.choose (function [ a; b ] -> Some(a, b) | _ -> None)
            let lut = curvesLut pts
            Custom (lut3 lut lut lut)
        | "sharpen" -> Custom (sharpenPass (pfloat ps "amount" 0.6))
        | "threshold" -> Custom (thresholdPass (pfloat ps "level" 128.0))
        | "posterize" -> let lut = posterizeLut (pint ps "levels" 4) in Custom (lut3 lut lut lut)
        | "vignette" -> Custom (vignettePass (pfloat ps "amount" 0.6) (pfloat ps "radius" 0.55))
        | "grain" | "noise" -> Custom (grainPass (pfloat ps "amount" 0.5))
        | "temperature" | "temp" ->
            let amt = pfloat ps "amount" 0.0   // -1 (cool) .. +1 (warm)
            let scale s = Array.init 256 (fun v -> clampB (float v * s))
            Custom (lut3 (scale (1.0 + 0.3 * amt)) (Array.init 256 byte) (scale (1.0 - 0.3 * amt)))
        | "duotone" | "gradientmap" ->
            Custom (duotonePass (pcolor ps "shadow" (Color.rgb 20uy 14uy 60uy)) (pcolor ps "highlight" (Color.rgb 255uy 220uy 150uy)))
        | _ -> Custom (fun _ _ px -> px)

    /// Names the document/LLM can use (the catalog — analog of VideoEdit's `Fx.catalog`).
    let names =
        [ "blur"; "saturate"; "brightness"; "contrast"; "grayscale"; "sepia"; "invert"; "hue"
          "opacity"; "levels"; "curves"; "sharpen"; "threshold"; "posterize"; "vignette"; "grain"
          "temperature"; "duotone" ]
