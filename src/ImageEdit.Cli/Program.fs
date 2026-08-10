module ImageEdit.Cli.Program

// Headless front-end for the image engine:
//   imageedit <doc.imed> [--out file.png]   render a document to PNG (default: its :out)
//   imageedit --diff a.imed b.imed           print the structural AST diff between two docs
//   imageedit --catalog                      list the adjustment vocabulary (for the LLM/chat)

open System
open System.IO
open ImageEdit

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | "--diff" :: a :: b :: _ ->
        let _, la = Sexp.loadDoc (File.ReadAllText a)
        let _, lb = Sexp.loadDoc (File.ReadAllText b)
        match diff la lb with
        | [] -> printfn "(no changes)"
        | lines -> lines |> List.iter (printfn "%s")
        0
    | "--catalog" :: _ ->
        printfn "adjustments: %s" (String.concat ", " Adjustments.names)
        0
    | docPath :: rest ->
        let outOverride = match rest with "--out" :: o :: _ -> Some o | _ -> None
        let cfg, root = Sexp.loadDoc (File.ReadAllText docPath)
        let sw = Diagnostics.Stopwatch.StartNew()
        let outp = Export.renderToFile cfg root outOverride
        printfn "wrote %s — %dx%d in %.2fs" outp cfg.W cfg.H sw.Elapsed.TotalSeconds
        0
    | [] ->
        eprintfn "usage: imageedit <doc.imed> [--out file.png]"
        eprintfn "       imageedit --diff a.imed b.imed"
        eprintfn "       imageedit --catalog"
        1
