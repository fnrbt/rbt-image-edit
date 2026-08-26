namespace Rbt.ImageEdit

open System
open System.IO
open System.Diagnostics
open Rbt.Renderer

/// Decoded raster sources. Decoding shells out to the ffmpeg/ffprobe CLI (symmetric with PNG
/// export) and premultiplies alpha so pixels are ready for the renderer's premultiplied
/// compositing. Cached by path so a re-load / re-render does not re-decode.
module Image =

    let private probe (path: string) : int * int =
        let psi = ProcessStartInfo("ffprobe")
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        [ "-v"; "error"; "-select_streams"; "v:0"; "-show_entries"; "stream=width,height"
          "-of"; "csv=p=0:s=x"; path ] |> List.iter psi.ArgumentList.Add
        let p = Process.Start psi
        let outp = p.StandardOutput.ReadToEnd().Trim()
        p.WaitForExit()
        match outp.Split('x') with
        | [| w; h |] -> int w, int h
        | _ -> failwithf "ffprobe could not read dimensions of %s (got '%s')" path outp

    let private framebufferFromBgra (label: string) w h (bytes: byte[]) =
        let n = w * h
        if bytes.Length < n * 4 then failwithf "short decode for %s: %d bytes (need %d)" label bytes.Length (n * 4)
        let px = Array.zeroCreate<uint32> n
        for i in 0 .. n - 1 do
            let o = i * 4
            px.[i] <- Pixel.fromColorPremul (Color.rgba bytes.[o + 2] bytes.[o + 1] bytes.[o] bytes.[o + 3])
        { W = w; H = h; Px = Memory(px) }

    let decode (path: string) : Framebuffer =
        if not (File.Exists path) then failwithf "image not found: %s" path
        let w, h = probe path
        let psi = ProcessStartInfo("ffmpeg")
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        [ "-v"; "error"; "-i"; path; "-frames:v"; "1"; "-f"; "rawvideo"; "-pix_fmt"; "bgra"; "pipe:1" ]
        |> List.iter psi.ArgumentList.Add
        let p = Process.Start psi
        use ms = new MemoryStream()
        p.StandardOutput.BaseStream.CopyTo ms
        let err = p.StandardError.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode <> 0 then failwithf "ffmpeg decode failed for %s: %s" path err
        let bytes = ms.ToArray()
        // ffmpeg "bgra" is straight (un-premultiplied) B,G,R,A per pixel → premultiply for the renderer.
        framebufferFromBgra path w h bytes

    /// Decode an in-memory image without granting the document filesystem access.
    let decodeBytes (label: string) (data: byte[]) : Framebuffer =
        if isNull data || data.Length = 0 then invalidArg "data" "image data is empty"
        let probePsi = ProcessStartInfo("ffprobe")
        probePsi.UseShellExecute <- false
        probePsi.RedirectStandardInput <- true
        probePsi.RedirectStandardOutput <- true
        probePsi.RedirectStandardError <- true
        [ "-v"; "error"; "-select_streams"; "v:0"; "-show_entries"; "stream=width,height"
          "-of"; "csv=p=0:s=x"; "pipe:0" ] |> List.iter probePsi.ArgumentList.Add
        let probeProcess = Process.Start probePsi
        let probeOutputTask = probeProcess.StandardOutput.ReadToEndAsync()
        let probeErrorTask = probeProcess.StandardError.ReadToEndAsync()
        probeProcess.StandardInput.BaseStream.Write(data, 0, data.Length)
        probeProcess.StandardInput.Close()
        let dimensions = probeOutputTask.GetAwaiter().GetResult().Trim()
        let probeError = probeErrorTask.GetAwaiter().GetResult()
        probeProcess.WaitForExit()
        let w, h =
            match dimensions.Split('x') with
            | [| width; height |] -> int width, int height
            | _ -> failwithf "ffprobe could not read dimensions of %s (got '%s'): %s" label dimensions probeError

        let psi = ProcessStartInfo("ffmpeg")
        psi.UseShellExecute <- false
        psi.RedirectStandardInput <- true
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        [ "-v"; "error"; "-i"; "pipe:0"; "-frames:v"; "1"; "-f"; "rawvideo"; "-pix_fmt"; "bgra"; "pipe:1" ]
        |> List.iter psi.ArgumentList.Add
        let p = Process.Start psi
        use ms = new MemoryStream()
        let outputTask = p.StandardOutput.BaseStream.CopyToAsync(ms)
        let errorTask = p.StandardError.ReadToEndAsync()
        p.StandardInput.BaseStream.Write(data, 0, data.Length)
        p.StandardInput.Close()
        outputTask.GetAwaiter().GetResult()
        let err = errorTask.GetAwaiter().GetResult()
        p.WaitForExit()
        if p.ExitCode <> 0 then failwithf "ffmpeg decode failed for %s: %s" label err
        framebufferFromBgra label w h (ms.ToArray())

    let private cache = System.Collections.Generic.Dictionary<string, Framebuffer>()
    let loadCached (path: string) : Framebuffer =
        match cache.TryGetValue path with
        | true, v -> v
        | _ -> let v = decode path in cache.[path] <- v; v
