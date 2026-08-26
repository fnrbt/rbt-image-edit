namespace Rbt.ImageEdit

open System
open System.IO
open System.Diagnostics
open Rbt.Renderer

/// Headless rendering of a layer tree to a framebuffer, and writing a framebuffer to PNG via the
/// ffmpeg CLI (the pattern the video editor uses for headless --shot frames). Uses the software
/// rasterizer, so `Filter.Custom` adjustment passes apply with no GL context required.
module Export =

    let private straightBgraBytes (fb: Framebuffer) =
        let n = fb.W * fb.H
        let bytes = Array.zeroCreate<byte> (n * 4)
        let span = fb.Px.Span
        for i in 0 .. n - 1 do
            let c = Color.fromPremul (Pixel.toColor span.[i])
            let o = i * 4
            bytes.[o] <- c.B; bytes.[o + 1] <- c.G; bytes.[o + 2] <- c.R; bytes.[o + 3] <- c.A
        bytes

    /// Composite the document to a premultiplied-BGRA framebuffer at its canvas size.
    let renderCanvas (cfg: Sexp.ImageConfig) (root: Layer) : Framebuffer =
        let frame = { W = float cfg.W; H = float cfg.H }
        let backend = Raster.create ()
        Rbt.Renderer.Scene.render backend cfg.W cfg.H (Clear cfg.Bg :: eval frame root)

    /// Write a framebuffer to a PNG (or any ffmpeg-supported still). Un-premultiplies alpha so the
    /// file stores straight colour (correct for semi-transparent / transparent pixels).
    let writePng (fb: Framebuffer) (outPath: string) : unit =
        let dir = Path.GetDirectoryName outPath
        if not (String.IsNullOrEmpty dir) then Directory.CreateDirectory dir |> ignore
        let bytes = straightBgraBytes fb
        let psi = ProcessStartInfo("ffmpeg")
        psi.UseShellExecute <- false
        psi.RedirectStandardInput <- true
        psi.RedirectStandardError <- true
        [ "-y"; "-f"; "rawvideo"; "-pixel_format"; "bgra"; "-video_size"; sprintf "%dx%d" fb.W fb.H
          "-i"; "pipe:0"; "-frames:v"; "1"; outPath ] |> List.iter psi.ArgumentList.Add
        let p = Process.Start psi
        p.StandardInput.BaseStream.Write(bytes, 0, bytes.Length)
        p.StandardInput.BaseStream.Flush()
        p.StandardInput.Close()
        let err = p.StandardError.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode <> 0 then failwithf "ffmpeg PNG encode failed: %s" err

    /// Encode a framebuffer as PNG bytes for callers that own persistence.
    let toPngBytes (fb: Framebuffer) : byte[] =
        let bytes = straightBgraBytes fb
        let psi = ProcessStartInfo("ffmpeg")
        psi.UseShellExecute <- false
        psi.RedirectStandardInput <- true
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        [ "-v"; "error"; "-f"; "rawvideo"; "-pixel_format"; "bgra"; "-video_size"; sprintf "%dx%d" fb.W fb.H
          "-i"; "pipe:0"; "-frames:v"; "1"; "-f"; "image2pipe"; "-vcodec"; "png"; "pipe:1" ]
        |> List.iter psi.ArgumentList.Add
        let p = Process.Start psi
        use output = new MemoryStream()
        let outputTask = p.StandardOutput.BaseStream.CopyToAsync(output)
        let errorTask = p.StandardError.ReadToEndAsync()
        p.StandardInput.BaseStream.Write(bytes, 0, bytes.Length)
        p.StandardInput.Close()
        outputTask.GetAwaiter().GetResult()
        let err = errorTask.GetAwaiter().GetResult()
        p.WaitForExit()
        if p.ExitCode <> 0 then failwithf "ffmpeg PNG encode failed: %s" err
        output.ToArray()

    /// Render an .imed document to its configured `:out` path (or an override). Returns the path.
    let renderToFile (cfg: Sexp.ImageConfig) (root: Layer) (outOverride: string option) : string =
        let outp = defaultArg outOverride cfg.Out
        writePng (renderCanvas cfg root) outp
        outp
