namespace ImageEdit

/// A structural diff over the layer AST (mirrors `VideoEdit.Diff`). Names + params + structure diff
/// into human-readable change lines; opaque leaves (raster pixels, mask shapes) compare by presence
/// / identity. This is what makes "the LLM rewrites the program, we diff the resulting AST" read
/// like the edits a person would describe — it is what each assistant chat turn shows.
/// Deliberately does NOT `open Renderer` (keeps Cmd cases from shadowing layer cases).
[<AutoOpen>]
module Diff =

    let private caseName (l: Layer) : string =
        match l with
        | Empty -> "empty" | SolidFill _ -> "solid" | GradientFill _ -> "gradient"
        | Bitmap r -> "raster:" + System.IO.Path.GetFileName r.Path
        | Shape _ -> "shape" | Caption t -> "text:" + t.Text
        | Stack _ -> "stack" | Folder (n, _, _, _) -> "folder:" + n
        | Blended (m, _) -> "blend:" + sprintf "%A" m
        | Placed _ -> "placed" | Masked _ -> "masked" | Clipped _ -> "clipped"
        | Adjust (a, _) -> "adjust:" + a.Name | AdjustBelow (a, _) -> "adjust-below:" + a.Name

    let diff (before: Layer) (after: Layer) : string list =
        let out = ResizeArray<string>()
        let pdiff (path: string) (pa: Params) (pb: Params) =
            let keys = Set.ofSeq (Seq.append (Map.toSeq pa |> Seq.map fst) (Map.toSeq pb |> Seq.map fst))
            for k in Set.toList keys do
                match Map.tryFind k pa, Map.tryFind k pb with
                | Some va, Some vb -> if showParam va <> showParam vb then out.Add(sprintf "~ %s.%s: %s → %s" path k (showParam va) (showParam vb))
                | None, Some vb -> out.Add(sprintf "+ %s.%s = %s" path k (showParam vb))
                | Some va, None -> out.Add(sprintf "- %s.%s (was %s)" path k (showParam va))
                | None, None -> ()
        let rec go (path: string) (a: Layer) (b: Layer) =
            match a, b with
            | Adjust (ra, ca), Adjust (rb, cb) ->
                if ra.Name <> rb.Name then out.Add(sprintf "~ %s: adjust %s → %s" path ra.Name rb.Name)
                else pdiff (sprintf "%s.%s" path ra.Name) ra.Params rb.Params
                go path ca cb
            | AdjustBelow (ra, _), AdjustBelow (rb, _) ->
                if ra.Name <> rb.Name then out.Add(sprintf "~ %s: adjust-below %s → %s" path ra.Name rb.Name)
                else pdiff (sprintf "%s.%s" path ra.Name) ra.Params rb.Params
            | Stack xs, Stack ys ->
                if List.length xs <> List.length ys then out.Add(sprintf "~ %s: %d → %d layers" path (List.length xs) (List.length ys))
                let n = min (List.length xs) (List.length ys)
                List.iteri2 (fun i x y -> go (sprintf "%s[%d]" path i) x y) (List.truncate n xs) (List.truncate n ys)
                ys |> List.skip n |> List.iteri (fun i y -> out.Add(sprintf "+ %s[%d]: %s" path (n + i) (caseName y)))
                xs |> List.skip n |> List.iteri (fun i x -> out.Add(sprintf "- %s[%d]: %s" path (n + i) (caseName x)))
            | Folder (na, va, oa, ca), Folder (nb, vb, ob, cb) ->
                if na <> nb then out.Add(sprintf "~ %s: folder %s → %s" path na nb)
                if va <> vb then out.Add(sprintf "~ %s '%s': %s" path nb (if vb then "shown" else "hidden"))
                if oa <> ob then out.Add(sprintf "~ %s '%s' opacity: %.2g → %.2g" path nb oa ob)
                go (sprintf "%s.%s" path nb) ca cb
            | Blended (ma, ca), Blended (mb, cb) ->
                if ma <> mb then out.Add(sprintf "~ %s blend: %A → %A" path ma mb)
                go path ca cb
            | Placed (ca, _), Placed (cb, _) -> go path ca cb
            | Masked (ca, _), Masked (cb, _) -> go path ca cb
            | Clipped (_, ca), Clipped (_, cb) -> go path ca cb
            | SolidFill ca, SolidFill cb ->
                if showParam (PColor ca) <> showParam (PColor cb) then
                    out.Add(sprintf "~ %s solid: %s → %s" path (showParam (PColor ca)) (showParam (PColor cb)))
            | Caption ta, Caption tb ->
                if ta.Text <> tb.Text then out.Add(sprintf "~ %s text: \"%s\" → \"%s\"" path ta.Text tb.Text)
                if ta.Size <> tb.Size then out.Add(sprintf "~ %s text.size: %.4g → %.4g" path ta.Size tb.Size)
            | _ -> if caseName a <> caseName b then out.Add(sprintf "~ %s: %s → %s" path (caseName a) (caseName b))
        go "root" before after
        List.ofSeq out
