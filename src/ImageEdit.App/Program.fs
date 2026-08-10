module ImageEdit.App.Program

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open Renderer
open ImageEdit
open Wisp.Reactive
open Wisp.Layout
open Wisp.Widgets
open Wisp.Theme
open ImageEdit.App
open ImageEdit.App.History

// The atrium chrome now lives in the Wisp library (shared with the PDF viewer); `Ui` aliases it so the
// existing `Ui.*` references resolve unchanged.
module Ui = Wisp.Atrium

// ── adjustment catalogue (drives the toolbar + dialogs) ──
type ParamSpec = { Key: string; Label: string; Min: float; Max: float; Default: float }
type AdjKind = KSliders of ParamSpec list | KCurves | KLevels
type AdjSpec = { Name: string; Title: string; Kind: AdjKind }

let private ps key label mn mx def = { Key = key; Label = label; Min = mn; Max = mx; Default = def }
let adjustments =
    [ { Name = "brightness";  Title = "Brightness";  Kind = KSliders [ ps "amount" "Amount" 0.0 2.0 1.2 ] }
      { Name = "contrast";    Title = "Contrast";    Kind = KSliders [ ps "amount" "Amount" 0.0 2.0 1.3 ] }
      { Name = "saturate";    Title = "Saturation";  Kind = KSliders [ ps "amount" "Amount" 0.0 3.0 1.5 ] }
      { Name = "temperature"; Title = "Temperature"; Kind = KSliders [ ps "amount" "Warm ↔ Cool" -1.0 1.0 0.3 ] }
      { Name = "hue";         Title = "Hue";         Kind = KSliders [ ps "degrees" "Degrees" -180.0 180.0 0.0 ] }
      { Name = "grayscale";   Title = "Grayscale";   Kind = KSliders [ ps "amount" "Amount" 0.0 1.0 1.0 ] }
      { Name = "sharpen";     Title = "Sharpen";     Kind = KSliders [ ps "amount" "Amount" 0.0 2.0 0.8 ] }
      { Name = "blur";        Title = "Blur";        Kind = KSliders [ ps "radius" "Radius" 0.0 30.0 8.0 ] }
      { Name = "vignette";    Title = "Vignette";    Kind = KSliders [ ps "amount" "Amount" 0.0 1.0 0.6; ps "radius" "Radius" 0.0 1.0 0.5 ] }
      { Name = "curves";      Title = "Curves";      Kind = KCurves }
      { Name = "levels";      Title = "Levels";      Kind = KLevels } ]

type Tool = HandTool | MoveTool | TextTool | ShapeTool | GradientTool | PickTool

/// A chat entry's role — mirrors chatkit's EntryKind (User/Assistant/Tool/Status), the subset the
/// local mini-agent produces. Tool entries render as a collapsible tool-call card.
type ChatRole = RYou | RAssistant | RTool | RStatus

/// A chat entry. A Tool entry carries a status (`Ok`), the param/diff `Details` (collapsible), and —
/// for edits — the resulting doc `Snapshot` (+ AST diff), so the conversation can optionally restore
/// that document state, independent of undo (dual time-travel).
type ChatEntry =
    { Role: ChatRole; Title: string; Text: string; Ok: bool; Details: string list; Snapshot: Layer option }

type DialogState =
    { LayerIdx: int
      Spec: AdjSpec
      Params: Dictionary<string, Signal<float>>
      Points: Signal<(float * float) list> }

let private buildRef (ds: DialogState) : AdjustRef =
    match ds.Spec.Kind with
    | KCurves ->
        let flat = ds.Points.Peek() |> List.collect (fun (a, b) -> [ PFloat a; PFloat b ])
        { Name = "curves"; Params = pmap [ "points", PList flat ] }
    | _ ->
        { Name = ds.Spec.Name; Params = pmap [ for kv in ds.Params -> kv.Key, PFloat(kv.Value.Peek()) ] }

/// Renders the latest requested document on a background thread, coalescing to the most recent
/// request (intermediate drag ticks are skipped) and posting the framebuffer back to the UI thread.
type private RenderService(render: Layer -> int -> Framebuffer, onFb: Framebuffer -> unit) =
    let gate = obj ()
    let mutable pending: (Layer * int) option = None
    let mutable busy = false
    let rec pump () =
        let work = lock gate (fun () -> let p = pending in pending <- None; (if p.IsNone then busy <- false); p)
        match work with
        | Some(doc, dim) ->
            let fb = render doc dim
            Wisp.Host.uiPost.Value (fun () -> onFb fb)
            pump ()
        | None -> ()
    member _.Request(doc: Layer, dim: int) =
        lock gate (fun () ->
            pending <- Some(doc, dim)
            if not busy then busy <- true; System.Threading.Tasks.Task.Run(pump) |> ignore)

// ── application state ────────────────────────────────────
type AppState(cfg: Sexp.ImageConfig, initialDoc: Layer, headless: bool) =
    let history = History(initialDoc)
    let docSig = refSignal initialDoc
    let currentFb = refSignal (None: Framebuffer option)
    let previewSig = refSignal (None: Layer option)   // live dialog preview overrides docSig
    let dialogSig = refSignal (None: DialogState option)
    let mutable dialogDisposer: unit -> unit = ignore
    let histTick = signal 0
    let zoom = signal 1.0
    let panX = signal 0.0
    let panY = signal 0.0
    let selected = signal -1
    let currentTool = signal HandTool
    let pickedColor = signal Color.white
    let shapeColor = signal (Color.rgb 232uy 88uy 88uy)
    // tool-option state (driven by the per-tool options bar)
    let shapeKind = signal 0                                   // 0 rect · 1 ellipse
    let gradStart = signal (Color.rgb 74uy 132uy 224uy)
    let gradEnd = signal (Color.rgb 244uy 246uy 252uy)
    let gradDir = signal 0                                     // 0 horizontal · 1 vertical
    let textValue = signal "Text"
    let textSize = signal 72.0
    let textBold = signal true
    let textColor = signal Color.white
    let chatLog =
        refSignal
            [ { Role = RAssistant; Title = ""
                Text = "Hi - tell me how to edit the image. Try 'make it vintage', 'boost saturation', or 'add a vignette'."
                Ok = true; Details = []; Snapshot = None } ]
    let chatInput = signal ""
    let menuOpen = signal -1

    // Render the whole scene scaled into a `maxDim`-capped framebuffer (display res, not full res).
    let renderInto (doc: Layer) (maxDim: int) : Framebuffer =
        let s = min 1.0 (float maxDim / float (max cfg.W cfg.H))
        let w' = max 1 (int (float cfg.W * s))
        let h' = max 1 (int (float cfg.H * s))
        let frame : Frame = { W = float cfg.W; H = float cfg.H }
        Renderer.Scene.render (Raster.create ()) w' h'
            (Clear cfg.Bg :: [ Group(Matrix3x2.scale s s, 1.0, eval frame doc) ])

    let service = RenderService(renderInto, (fun fb -> currentFb.Value <- Some fb; Scheduler.requestFrame ()))

    // initial synchronous render so the first frame (and --shot) has content
    do currentFb.Value <- Some(renderInto initialDoc 1100)
    // on doc/preview change: headless renders synchronously; live mode renders async (coalesced).
    let mutable firstRun = true
    do effect (fun () ->
        let d = match previewSig.Value with Some p -> p | None -> docSig.Value
        if firstRun then firstRun <- false
        elif headless then currentFb.Value <- Some(renderInto d 1400)
        else service.Request(d, (if previewSig.Peek().IsSome then 760 else 1100))) |> ignore

    let bump () = histTick.Value <- histTick.Peek() + 1
    let closeDialog () =
        dialogDisposer (); dialogDisposer <- ignore
        previewSig.Value <- None
        dialogSig.Value <- None

    member _.Cfg = cfg
    member _.History = history
    member _.HistTick = histTick
    member _.Doc = docSig
    member _.Fb = currentFb
    member _.Zoom = zoom
    member _.PanX = panX
    member _.PanY = panY
    member _.Selected = selected
    member _.DialogSig = dialogSig
    member _.CurrentTool = currentTool
    member _.PickedColor = pickedColor
    member _.ShapeColor = shapeColor
    member _.ShapeKind = shapeKind
    member _.GradStart = gradStart
    member _.GradEnd = gradEnd
    member _.GradDir = gradDir
    member _.TextValue = textValue
    member _.TextSize = textSize
    member _.TextBold = textBold
    member _.TextColor = textColor

    /// Sample a pixel colour from the cached framebuffer at a document-space point (for eyedropper).
    member _.SamplePixel(docX: float, docY: float) =
        match currentFb.Peek() with
        | Some f ->
            let px = int (docX / float cfg.W * float f.W)
            let py = int (docY / float cfg.H * float f.H)
            if px >= 0 && px < f.W && py >= 0 && py < f.H then
                let c = Color.fromPremul (Pixel.toColor f.Px.Span.[py * f.W + px])
                pickedColor.Value <- { c with A = 255uy }
        | None -> ()

    member _.Apply(f: Layer -> Layer, label: string) =
        let d = f (docSig.Peek()) in history.Push(d, label); docSig.Value <- d; bump ()
    member _.ApplyLive(f: Layer -> Layer, label: string) =
        let d = f (docSig.Peek()) in history.ReplaceOrPush(d, label); docSig.Value <- d; bump ()
    member _.Undo() = if history.Undo() then docSig.Value <- history.CurrentDoc; bump ()
    member _.Redo() = if history.Redo() then docSig.Value <- history.CurrentDoc; bump ()
    member _.JumpTo(id) = history.JumpTo id; docSig.Value <- history.CurrentDoc; bump ()
    member _.Export() = Export.renderToFile cfg (docSig.Peek()) (Some "out/editor-export.png") |> ignore

    /// Open an adjustment dialog on a layer; a live-preview effect updates the canvas as params change.
    member _.OpenDialog(spec: AdjSpec, layerIdx: int) =
        closeDialog ()
        let prms = Dictionary<string, Signal<float>>()
        (match spec.Kind with
         | KSliders specs -> for p in specs do prms.[p.Key] <- signal p.Default
         | KLevels -> prms.["black"] <- signal 0.0; prms.["white"] <- signal 255.0; prms.["gamma"] <- signal 1.0
         | KCurves -> ())
        let points = signal [ 0.0, 0.0; 255.0, 255.0 ]
        let ds = { LayerIdx = layerIdx; Spec = spec; Params = prms; Points = points }
        dialogDisposer <-
            effect (fun () ->
                for kv in prms do kv.Value.Value |> ignore   // subscribe
                points.Value |> ignore
                previewSig.Value <- Some(Doc.applyAdjust layerIdx (buildRef ds) (docSig.Peek())))
        dialogSig.Value <- Some ds

    member _.CloseDialog() = closeDialog ()
    member _.ApplyDialog() =
        match dialogSig.Peek() with
        | Some ds ->
            let d = Doc.applyAdjust ds.LayerIdx (buildRef ds) (docSig.Peek())
            history.Push(d, ds.Spec.Name); docSig.Value <- d
            closeDialog (); bump ()
        | None -> ()

    /// 256-bin luminance histogram of the current framebuffer (sampled), normalized to 0..1.
    member _.HistogramBins() : float[] =
        match currentFb.Peek() with
        | Some f ->
            let bins = Array.zeroCreate 256
            let span = f.Px.Span
            let stride = max 1 (f.Px.Length / 40000)
            let mutable k = 0
            while k < f.Px.Length do
                let c = Color.fromPremul (Pixel.toColor span.[k])
                let lum = int (0.299 * float c.R + 0.587 * float c.G + 0.114 * float c.B)
                bins.[max 0 (min 255 lum)] <- bins.[max 0 (min 255 lum)] + 1.0
                k <- k + stride
            let mx = Array.max bins
            if mx > 0.0 then bins |> Array.map (fun v -> v / mx) else bins
        | None -> Array.zeroCreate 256

    member _.ChatLog = chatLog
    member _.ChatInput = chatInput
    member _.MenuOpen = menuOpen
    /// Send a chat instruction: the mini-agent edits the doc, the turn records the resulting snapshot
    /// (+ AST diff), and the edit is committed to the undo history.
    member _.SendChat() =
        let txt = chatInput.Peek().Trim()
        if txt <> "" then
            let before = docSig.Peek()
            let after, reply, tool = Chat.interpret txt before
            let changed = not (System.Object.ReferenceEquals(before, after))
            let dlines = if changed then Diff.diff before after else []
            let you = { Role = RYou; Title = ""; Text = txt; Ok = true; Details = []; Snapshot = None }
            let toolCard =
                if changed then
                    [ { Role = RTool; Title = tool
                        Text = sprintf "%d node%s changed" dlines.Length (if dlines.Length = 1 then "" else "s")
                        Ok = true; Details = dlines; Snapshot = Some after } ]
                else []
            let assistant = { Role = RAssistant; Title = ""; Text = reply; Ok = true; Details = []; Snapshot = None }
            chatLog.Value <- chatLog.Peek() @ [ you ] @ toolCard @ [ assistant ]
            chatInput.Value <- ""
            if changed then history.Push(after, "chat: " + txt); docSig.Value <- after; bump ()
    /// Dual time-travel: restore the document to a past chat turn's snapshot (records an undo node),
    /// WITHOUT altering the conversation — the chat and document histories are decoupled.
    member _.RestoreChat(snap: Layer) = history.Push(snap, "revert to chat"); docSig.Value <- snap; bump ()

    member this.OnKey (kc: int) (shift: bool) (ctrl: bool) (_alt: bool) =
        match kc with
        | 9 -> this.CloseDialog()                                      // Escape
        | 52 when ctrl -> if shift then this.Redo() else this.Undo()   // Ctrl(+Shift)+Z
        | 29 when ctrl -> this.Redo()                                  // Ctrl+Y
        | _ -> ()

// ── UI (Atrium · flat blue-glass surface, hairline-divided sections) ──
let private colorChip (sg: Signal<Color>) (onClick: unit -> unit) : Element =
    clickable onClick (canvas 24.0 16.0 (fun x y w h -> Ui.wellCmds x y w h 3.0 @ [ FillRoundedRect(rect (x + 2.0) (y + 2.0) (w - 4.0) (h - 4.0), 2.0<px>, sg.Peek()) ]))

let private shapePalette =
    [ Color.rgb 232uy 88uy 88uy; Color.rgb 80uy 184uy 120uy; Color.rgb 74uy 132uy 224uy
      Color.rgb 240uy 196uy 72uy; Color.white; Color.rgb 40uy 44uy 54uy ]

/// Cycle a colour signal through the palette (click-to-change for the inline colour chips).
let private cycleColor (sg: Signal<Color>) =
    let i = shapePalette |> List.tryFindIndex (fun c -> c = sg.Peek()) |> Option.defaultValue -1
    sg.Value <- shapePalette.[(i + 1) % shapePalette.Length]

// ── menu bar (Atrium) ────────────────────────────────────
let private menuDefs (s: AppState) : (string * (string * (unit -> unit)) list) list =
    let sel () = if s.Selected.Peek() >= 0 then s.Selected.Peek() else 0
    [ "File",   [ "Export PNG", (fun () -> s.Export()) ]
      "Edit",   [ "Undo", (fun () -> s.Undo()); "Redo", (fun () -> s.Redo())
                  "Duplicate layer", (fun () -> let i = sel () in if i >= 0 then s.Apply(Doc.duplicateLayer i, "duplicate layer")) ]
      "Filter", [ for spec in adjustments -> spec.Title, (fun () -> s.OpenDialog(spec, sel ())) ]
      "View",   [ "Zoom in",  (fun () -> s.Zoom.Value <- min 8.0 (s.Zoom.Peek() * 1.25))
                  "Zoom out", (fun () -> s.Zoom.Value <- max 0.1 (s.Zoom.Peek() / 1.25))
                  "Fit",      (fun () -> s.Zoom.Value <- 1.0; s.PanX.Value <- 0.0; s.PanY.Value <- 0.0) ] ]

let private menuTitleW (title: string) : float = Text.measureWidth (Font.regular ()) 13.0 title + 18.0  // padXY 9

let private menuBar (s: AppState) : Element =
    let ms = menuDefs s
    heightBox 28.0
        (padXY (Ui.pad - 9.0) 0.0
            (rowA MStart CCenter 0.0
                [ for i in 0 .. ms.Length - 1 ->
                    let title = fst ms.[i]
                    let hov = signal false
                    hoverable (fun e -> hov.Value <- e; if e && s.MenuOpen.Peek() >= 0 then s.MenuOpen.Value <- i)
                        (clickable (fun () -> s.MenuOpen.Value <- (if s.MenuOpen.Peek() = i then -1 else i))
                            (Ui.surfaceBox
                                (fun x y w h ->
                                    if s.MenuOpen.Peek() = i then Ui.controlCmds x y w h Ui.radius
                                    elif hov.Peek() then [ FillRoundedRect(rect x y w h, Ui.radius * 1.0<px>, Ui.P.sel) ]
                                    else [])
                                (padXY 9.0 4.0 (text 13.0 Ui.P.text title)))) ]))

let private menuOverlay (s: AppState) : Element =
    dyn (fun () -> s.MenuOpen.Value)
        (fun () ->
            let i = s.MenuOpen.Peek()
            if i < 0 then Ui.blank ()
            else
                let ms = menuDefs s
                let _, items = ms.[i]
                let xoff = (Ui.pad - 9.0) + List.sumBy menuTitleW [ for j in 0 .. i - 1 -> fst ms.[j] ]
                stack [
                    pointerRegion (fun k _ _ -> match k with Down -> s.MenuOpen.Value <- -1 | _ -> ()) (fill (Color.rgba 0uy 0uy 0uy 0uy))
                    offset xoff 28.0
                        (fixedWidth 196.0
                            (Ui.surfaceBox (fun x y w h -> Ui.modalCmds x y w h Ui.radius)
                                (padding 5.0
                                    (columnA MStart CStretch 1.0
                                        [ for (lbl, act) in items ->
                                            let hov = signal false
                                            hoverable (fun e -> hov.Value <- e)
                                                (clickable (fun () -> act (); s.MenuOpen.Value <- -1)
                                                    (Ui.surfaceBox (fun x y w h -> if hov.Peek() then [ FillRoundedRect(rect x y w h, Ui.radius * 1.0<px>, Ui.P.sel) ] else [])
                                                        (padXY 9.0 5.0 (text 12.5 Ui.P.text lbl)))) ]))))
                ])

/// Top bar: title, the tool row, and primary actions — flush on the surface, a hairline below.
let private topBar (s: AppState) : Element =
    let tools =
        [ MoveTool, Ui.Glyph.move; HandTool, Ui.Glyph.hand; TextTool, Ui.Glyph.text2
          ShapeTool, Ui.Glyph.shape; GradientTool, Ui.Glyph.gradient; PickTool, Ui.Glyph.dropper ]
    heightBox 44.0
        (padXY Ui.pad 0.0
            (rowA MStart CCenter Ui.gap
                ([ textBold 15.0 Ui.P.text "ImageEdit"
                   Ui.vdivider ()
                   yield! [ for (t, g) in tools -> Ui.toolBtn g (fun () -> s.CurrentTool.Peek() = t) (fun () -> s.CurrentTool.Value <- t) ]
                   expanded 1 (Ui.blank ())
                   Ui.btn "Undo" (fun () -> s.Undo())
                   Ui.btn "Redo" (fun () -> s.Redo())
                   Ui.btn "Fit" (fun () -> s.Zoom.Value <- 1.0; s.PanX.Value <- 0.0; s.PanY.Value <- 0.0)
                   Ui.btnPrimary "Export" (fun () -> s.Export()) ])))

let private canvasView (s: AppState) : Element =
    let vp = ref (0.0, 0.0, 1.0)                  // localLeft, localTop, scale
    let cvSize = ref (0.0, 0.0)                    // canvas viewport size (for pan clamping)
    let toDoc lx ly = let l, t, sc = vp.Value in (lx - l) / sc, (ly - t) / sc
    let panLast = ref (0.0, 0.0)
    // Clamp pan to the image's overflow past the viewport: at fit the image can't move; zoomed in
    // you can only pan within the image (no dragging it off / no empty gaps).
    let clampPan () =
        let cw, ch = cvSize.Value
        let scale = (min (cw / float s.Cfg.W) (ch / float s.Cfg.H)) * s.Zoom.Peek()
        let limX = max 0.0 ((float s.Cfg.W * scale - cw) / 2.0)
        let limY = max 0.0 ((float s.Cfg.H * scale - ch) / 2.0)
        s.PanX.Value <- max -limX (min limX (s.PanX.Peek()))
        s.PanY.Value <- max -limY (min limY (s.PanY.Peek()))
    let moveBase = ref (0.0, 0.0, 0.0, 0.0, -1)   // startDocX, startDocY, baseTx, baseTy, layerIdx
    let shapeStart = ref (0.0, 0.0, 0.0, 0.0)     // startDocX, startDocY, startLx, startLy
    let band = ref (None: (float * float * float * float) option)   // rubber-band local rect
    let handle kind lx ly =
        let docX, docY = toDoc lx ly
        match s.CurrentTool.Peek() with
        | HandTool ->
            match kind with
            | Down -> panLast.Value <- (lx, ly)
            | Move ->
                let px, py = panLast.Value
                s.PanX.Value <- s.PanX.Peek() + (lx - px)
                s.PanY.Value <- s.PanY.Peek() + (ly - py)
                clampPan ()
                panLast.Value <- (lx, ly)
            | Up -> ()
        | MoveTool ->
            match kind with
            | Down -> let idx = s.Selected.Peek() in let bx, by = Doc.getTranslate idx (s.Doc.Peek()) in moveBase.Value <- (docX, docY, bx, by, idx)
            | Move -> let sx, sy, bx, by, idx = moveBase.Value in if idx >= 0 then s.ApplyLive(Doc.setTranslate idx (bx + docX - sx) (by + docY - sy), "move")
            | Up -> ()
        | TextTool ->
            (match kind with
             | Down -> s.Apply(Doc.appendLayer "text" (Doc.textLayerEx docX docY (s.TextValue.Peek()) (s.TextSize.Peek()) (s.TextColor.Peek()) (s.TextBold.Peek())), "add text")
             | _ -> ())
        | ShapeTool | GradientTool ->
            match kind with
            | Down -> shapeStart.Value <- (docX, docY, lx, ly); band.Value <- Some(lx, ly, 0.0, 0.0)
            | Move -> let _, _, slx, sly = shapeStart.Value in band.Value <- Some(min slx lx, min sly ly, abs (lx - slx), abs (ly - sly))
            | Up ->
                let sdx, sdy, _, _ = shapeStart.Value
                (if abs (docX - sdx) > 3.0 && abs (docY - sdy) > 3.0 then
                    let r = rect (min sdx docX) (min sdy docY) (abs (docX - sdx)) (abs (docY - sdy))
                    let layer, name =
                        match s.CurrentTool.Peek() with
                        | GradientTool -> Doc.gradientLayer r (s.GradStart.Peek()) (s.GradEnd.Peek()) (s.GradDir.Peek() = 0), "gradient"
                        | _ when s.ShapeKind.Peek() = 1 -> Doc.shapeEllipseLayer r (s.ShapeColor.Peek()), "ellipse"
                        | _ -> Doc.shapeRectLayer r (s.ShapeColor.Peek()), "shape"
                    s.Apply(Doc.appendLayer name layer, "add " + name))
                band.Value <- None
        | PickTool -> (match kind with Down -> s.SamplePixel(docX, docY) | _ -> ())
    let el =
        pointerRegion handle
            (canvasFill (fun x y w h ->
                let scale = (min (w / float s.Cfg.W) (h / float s.Cfg.H)) * s.Zoom.Peek()
                let dw, dh = float s.Cfg.W * scale, float s.Cfg.H * scale
                let ll = (w - dw) / 2.0 + s.PanX.Peek()
                let tt = (h - dh) / 2.0 + s.PanY.Peek()
                vp.Value <- (ll, tt, scale)
                cvSize.Value <- (w, h)
                let bg = [ FillRect(rect x y w h, Ui.P.canvasBg) ]
                let img =
                    match s.Fb.Peek() with
                    | Some f -> [ DrawImage(f, rect 0.0 0.0 (float f.W) (float f.H), rect (x + ll) (y + tt) dw dh, true) ]
                    | None -> []
                let rb =
                    match band.Value with
                    | Some(bx, by, bw, bh) -> [ StrokeRect(rect (x + bx) (y + by) bw bh, Ui.P.accent, 1.5<px>) ]
                    | None -> []
                bg @ img @ rb))
    el.OnScroll <- Some(fun dy ->
        s.Zoom.Value <- max 0.1 (min 8.0 (s.Zoom.Peek() * (if dy > 0.0 then 1.1 else 1.0 / 1.1)))
        clampPan ())
    el

// Rows are reused by index across edits, so every control reads LIVE state (by index) rather than a
// captured snapshot — otherwise toggling a value would leave the reused row showing the stale one.
let private layerRow (s: AppState) (idx: int) : Element =
    let info () = Doc.layers (s.Doc.Value) |> List.tryFind (fun l -> l.Index = idx)
    let vis () = match info () with Some l -> l.Visible | None -> true
    let name () = match info () with Some l -> (if l.Name.Length > 18 then l.Name.[..16] + "…" else l.Name) | None -> ""
    let blend () = match info () with Some l -> l.Blend | None -> "normal"
    let opac () = match info () with Some l -> l.Opacity | None -> 1.0
    clickable (fun () -> s.Selected.Value <- idx)
        (Ui.surfaceBox (fun x y w h -> if s.Selected.Peek() = idx then [ FillRoundedRect(rect x y w h, Ui.radius * 1.0<px>, Ui.P.sel) ] else [])
            (padXY 8.0 4.0
                (rowA MStart CCenter Ui.gap [
                    Ui.checkbox vis (fun () -> s.Apply(Doc.setVisible idx (not (vis ())), "visibility"))
                    expanded 1 (textf 12.5 Ui.P.text false name)
                    Ui.selectChip blend (fun () -> s.Apply(Doc.cycleBlend idx, "blend"))
                    Ui.slider 52.0 0.0 1.0 opac (fun v -> s.ApplyLive(Doc.setOpacity idx v, "opacity"))
                ])))

let private layersList (s: AppState) : Element =
    scrollView (signal 0.0)
        (forEachCol Ui.rowGap CStretch
            (fun () -> s.HistTick.Value |> ignore; s.Selected.Value |> ignore; Doc.layers (s.Doc.Peek()))
            (fun li -> li.Index)
            (fun li -> layerRow s li.Index))

let private adjustGrid (s: AppState) : Element =
    let onSel () = if s.Selected.Peek() >= 0 then s.Selected.Peek() else 0
    let short (spec: AdjSpec) = if spec.Name = "temperature" then "Temp" else spec.Title
    columnA MStart CStretch 5.0
        [ for chunk in adjustments |> List.chunkBySize 3 ->
            rowA MStart CCenter 5.0
                ([ for spec in chunk -> expanded 1 (Ui.btnSm (short spec) (fun () -> s.OpenDialog(spec, onSel ())))
                   for _ in 1 .. (3 - chunk.Length) -> expanded 1 (Ui.blank ()) ]) ]

let private historyList (s: AppState) : Element =
    scrollView (signal 0.0)
        (forEachCol Ui.rowGap CStretch
            (fun () -> s.HistTick.Value |> ignore; s.History.Nodes)
            (fun n -> n.Id)
            (fun n ->
                clickable (fun () -> s.JumpTo n.Id)
                    (Ui.surfaceBox (fun x y w h -> if s.History.CurrentId = n.Id then [ FillRoundedRect(rect x y w h, Ui.radius * 1.0<px>, Ui.P.sel) ] else [])
                        (padXY 8.0 5.0
                            (textf 12.0 Ui.P.text false
                                (fun () -> sprintf "%s%d · %s" (String.replicate (s.History.Depth n.Id) "  ") n.Id n.Label))))))

// ── layers section (with layer-op buttons in the header) ──
let private layersSection (s: AppState) : Element =
    let sel () = s.Selected.Peek()
    let count () = Doc.layerCount (s.Doc.Peek())
    padding Ui.pad
        (columnA MStart CStretch Ui.gap [
            rowA MStart CCenter 3.0 [
                expanded 1 (Ui.sectionLabel "Layers")
                Ui.iconBtn Ui.Glyph.plus (fun () -> s.Apply(Doc.appendLayer "layer" (Doc.shapeRectLayer (rect 120.0 120.0 (float s.Cfg.W * 0.4) (float s.Cfg.H * 0.4)) (s.ShapeColor.Peek())), "add layer"))
                Ui.iconBtn Ui.Glyph.up (fun () -> let i = sel () in if i >= 0 && i < count () - 1 then (s.Apply(Doc.moveLayer i (i + 1), "move up"); s.Selected.Value <- i + 1))
                Ui.iconBtn Ui.Glyph.down (fun () -> let i = sel () in if i > 0 then (s.Apply(Doc.moveLayer i (i - 1), "move down"); s.Selected.Value <- i - 1))
                Ui.iconBtn Ui.Glyph.trash (fun () -> let i = sel () in if i >= 0 && count () > 1 then (s.Apply(Doc.removeLayer i, "delete layer"); s.Selected.Value <- -1))
            ]
            expanded 1 (layersList s)
        ])

// ── chat / assistant pane (chatkit entry model rendered in atrium) ──
/// One chat entry. User = right-aligned accent bubble; Assistant = control bubble; Status = dim line;
/// Tool = a collapsible tool-call card (status dot + name + a disclosure to its diff, plus the
/// dual-time-travel "restore" affordance).
let private chatBubble (s: AppState) (e: ChatEntry) : Element =
    match e.Role with
    | RStatus -> rowA MCenter CCenter 0.0 [ text 11.0 Ui.P.mute e.Text ]
    | RYou ->
        rowA MEnd CCenter 0.0
            [ Ui.surfaceBox (fun x y w h -> Ui.accentCmds x y w h Ui.radius) (padXY 9.0 6.0 (Ui.wrapText 12.5 Ui.P.accentText e.Text)) ]
    | RAssistant ->
        Ui.surfaceBox (fun x y w h -> Ui.controlCmds x y w h Ui.radius) (padXY 9.0 6.0 (Ui.wrapText 12.5 Ui.P.text e.Text))
    | RTool ->
        let opened = signal false
        Ui.surfaceBox (fun x y w h -> Ui.controlCmds x y w h Ui.radius)
            (padding 8.0
                (columnA MStart CStretch 4.0
                    [ clickable (fun () -> opened.Value <- not (opened.Peek()))
                        (rowA MStart CStart 6.0
                            [ padXY 0.0 3.0 (Ui.statusDot (fun () -> if e.Ok then Ui.P.ok else Ui.P.warn))
                              expanded 1 (Ui.wrapText 12.0 Ui.P.text e.Title)
                              padXY 0.0 2.0 (canvas 12.0 12.0 (fun x y _ _ -> [ Group(Matrix3x2.translate x y, 1.0, (if opened.Peek() then Ui.Glyph.chevronD else Ui.Glyph.chevronR) 12.0 Ui.P.dim) ])) ])
                      text 10.5 Ui.P.mute e.Text
                      show (fun () -> opened.Value)
                        (fun () -> Ui.well (padXY 8.0 6.0 (columnA MStart CStretch 2.0 [ for d in e.Details -> Ui.wrapText 11.0 Ui.P.dim d ])))
                      (match e.Snapshot with
                       | Some snap -> clickable (fun () -> s.RestoreChat snap) (padXY 1.0 1.0 (text 11.0 Ui.P.accent "Restore this version"))
                       | None -> Ui.blank ()) ]))

let private chatHeader () : Element =
    heightBox 32.0
        (padXY Ui.pad 0.0
            (rowA MStart CCenter 7.0
                [ Ui.statusDot (fun () -> Ui.P.ok)
                  text 12.5 Ui.P.text "Local Agent"
                  expanded 1 (Ui.blank ())
                  text 11.0 Ui.P.mute "mini" ]))

let private assistantPanel (s: AppState) : Element =
    widthBox 264.0
        (columnA MStart CStretch 0.0
            [ chatHeader ()
              Ui.divider ()
              expanded 1
                (padding Ui.pad
                    (scrollView (signal 0.0)
                        (forEachCol Ui.gap CStretch
                            (fun () -> s.ChatLog.Value |> List.indexed)
                            (fun (i, _) -> i)
                            (fun (_, e) -> chatBubble s e))))
              Ui.divider ()
              padding Ui.pad
                (rowA MStart CCenter 6.0
                    [ expanded 1 (Ui.textField s.ChatInput "Ask…" (fun () -> s.SendChat()))
                      Ui.iconBtn Ui.Glyph.send (fun () -> s.SendChat()) ]) ])

let private sidebar (s: AppState) : Element =
    widthBox 312.0
        (columnA MStart CStretch 0.0 [
            expanded 2 (layersSection s)
            Ui.divider ()
            Ui.section "Adjustments" (adjustGrid s)
            Ui.divider ()
            expanded 2 (Ui.sectionFill "History" (historyList s))
        ])

/// The canvas viewport — the image on a neutral light backdrop, inset from the surface.
let private canvasArea (s: AppState) : Element = padding Ui.pad (canvasView s)

let private statusBar (s: AppState) : Element =
    heightBox 26.0
        (padXY Ui.pad 0.0
            (rowA MStart CCenter Ui.pad [
                Ui.labelDim (sprintf "%d × %d" s.Cfg.W s.Cfg.H)
                textf 12.0 Ui.P.dim false (fun () -> sprintf "%.0f%%" (s.Zoom.Value * 100.0))
                textf 12.0 Ui.P.dim false (fun () -> sprintf "%d layers" (Doc.layerCount (s.Doc.Value)))
                expanded 1 (Ui.blank ())
                Ui.labelDim (sprintf "%A" (s.CurrentTool.Peek()))
            ]))

let private sliderRow (lbl: string) (sg: Signal<float>) (mn: float) (mx: float) : Element =
    rowA MStart CCenter Ui.gap [
        sizedBox 64.0 16.0 (Ui.labelDim lbl)
        Ui.slider 150.0 mn mx (fun () -> sg.Peek()) (fun v -> sg.Value <- v)
        sizedBox 44.0 16.0 (textf 12.0 Ui.P.text false (fun () -> sprintf "%.2f" sg.Value))
    ]
let private centerRow (el: Element) : Element = rowA MCenter CCenter 0.0 [ el ]

let private dialogView (s: AppState) : Element =
    match s.DialogSig.Peek() with
    | None -> empty
    | Some ds ->
        let body =
            match ds.Spec.Kind with
            | KCurves -> centerRow (curveEditor 240.0 ds.Points)
            | KLevels ->
                columnA MStart CStretch Ui.gap [
                    centerRow (histogram 240.0 70.0 (fun () -> s.HistogramBins()))
                    sliderRow "Black" ds.Params.["black"] 0.0 255.0
                    sliderRow "White" ds.Params.["white"] 0.0 255.0
                    sliderRow "Gamma" ds.Params.["gamma"] 0.1 3.0
                ]
            | KSliders specs -> columnA MStart CStretch Ui.gap [ for p in specs -> sliderRow p.Label ds.Params.[p.Key] p.Min p.Max ]
        let footer = rowA MEnd CCenter Ui.gap [ Ui.btn "Cancel" (fun () -> s.CloseDialog()); Ui.btnPrimary "Apply" (fun () -> s.ApplyDialog()) ]
        modal (fun () -> s.CloseDialog())
            (fixedWidth 360.0
                (Ui.surfaceBox (fun x y w h -> Ui.modalCmds x y w h 8.0)
                    (padding 18.0
                        (columnA MStart CStretch 14.0 [ textBold 14.0 Ui.P.text ("Adjustment · " + ds.Spec.Title); Ui.divider (); body; footer ]))))

/// A thin per-tool options bar (Photoshop-style): the controls switch with the active tool.
let private toolOptions (s: AppState) : Element =
    let zoomLbl () = sprintf "%.0f%%" (s.Zoom.Value * 100.0)
    heightBox 34.0
        (padXY Ui.pad 0.0
            (dyn (fun () -> s.CurrentTool.Value)
                (fun () ->
                    let r els = rowA MStart CCenter Ui.gap els
                    match s.CurrentTool.Peek() with
                    | HandTool ->
                        r [ Ui.labelDim "Zoom"
                            Ui.iconBtn Ui.Glyph.down (fun () -> s.Zoom.Value <- max 0.1 (s.Zoom.Peek() / 1.25))
                            sizedBox 46.0 16.0 (textf 12.0 Ui.P.text false zoomLbl)
                            Ui.iconBtn Ui.Glyph.up (fun () -> s.Zoom.Value <- min 8.0 (s.Zoom.Peek() * 1.25))
                            Ui.btn "Fit" (fun () -> s.Zoom.Value <- 1.0; s.PanX.Value <- 0.0; s.PanY.Value <- 0.0) ]
                    | MoveTool ->
                        r [ Ui.labelDim "Move layer"; Ui.vdivider ()
                            Ui.btn "Reset position" (fun () -> let i = s.Selected.Peek() in if i >= 0 then s.Apply(Doc.setTranslate i 0.0 0.0, "reset position")) ]
                    | TextTool ->
                        r [ sizedBox 132.0 26.0 (Ui.textField s.TextValue "Text…" ignore)
                            Ui.labelDim "Size"
                            Ui.slider 92.0 8.0 200.0 (fun () -> s.TextSize.Peek()) (fun v -> s.TextSize.Value <- v)
                            Ui.toggleBtn "Bold" (fun () -> s.TextBold.Peek()) (fun () -> s.TextBold.Value <- not (s.TextBold.Peek()))
                            colorChip s.TextColor (fun () -> cycleColor s.TextColor) ]
                    | ShapeTool ->
                        r [ Ui.labelDim "Shape"
                            Ui.segmented [ "Rect"; "Ellipse" ] (fun () -> s.ShapeKind.Peek()) (fun i -> s.ShapeKind.Value <- i)
                            Ui.labelDim "Fill"; colorChip s.ShapeColor (fun () -> cycleColor s.ShapeColor) ]
                    | GradientTool ->
                        r [ Ui.labelDim "Gradient"
                            colorChip s.GradStart (fun () -> cycleColor s.GradStart)
                            colorChip s.GradEnd (fun () -> cycleColor s.GradEnd)
                            Ui.segmented [ "Horiz"; "Vert" ] (fun () -> s.GradDir.Peek()) (fun i -> s.GradDir.Value <- i) ]
                    | PickTool ->
                        r [ Ui.labelDim "Picked"; colorChip s.PickedColor ignore
                            textf 12.0 Ui.P.text false (fun () -> let cc = s.PickedColor.Value in sprintf "#%02X%02X%02X" cc.R cc.G cc.B) ])))

let view (s: AppState) : Element =
    stack [
        Ui.windowSurface
            (columnA MStart CStretch 0.0 [
                Ui.frostPane 190 (menuBar s)
                Ui.divider ()
                Ui.frostPane 182 (topBar s)
                Ui.divider ()
                Ui.frostPane 178 (toolOptions s)
                Ui.divider ()
                expanded 1
                    (rowA MStart CStretch 0.0 [
                        Ui.frostPane 150 (assistantPanel s)
                        Ui.vdivider ()
                        expanded 1 (canvasArea s)
                        Ui.vdivider ()
                        Ui.frostPane 158 (sidebar s)
                    ])
                Ui.divider ()
                Ui.frostPane 196 (statusBar s)
            ])
        menuOverlay s
        show (fun () -> s.DialogSig.Value.IsSome) (fun () -> dialogView s)
    ]

// ── headless verification of edit + history logic ────────
let private selfTest (loaded: Layer) : int =
    let mutable ok = 0
    let mutable bad = 0
    let check name c = if c then ok <- ok + 1; printfn "  ok   %s" name else bad <- bad + 1; printfn "  FAIL %s" name
    let h = History(Doc.normalize loaded)
    let top () = List.head (Doc.layers h.CurrentDoc)
    let v0 = (top ()).Visible
    check "doc has layers" (not (List.isEmpty (Doc.layers h.CurrentDoc)))
    h.Push(Doc.setVisible (top ()).Index (not v0) h.CurrentDoc, "visibility")
    check "visibility edit applied" ((top ()).Visible = not v0)
    h.Undo() |> ignore
    check "undo reverts" ((top ()).Visible = v0)
    h.Redo() |> ignore
    check "redo re-applies" ((top ()).Visible = not v0)
    let n0 = Doc.layerCount h.CurrentDoc
    h.Push(Doc.applyAdjust (top ()).Index { Name = "curves"; Params = pmap [ "points", PList [ PFloat 0.0; PFloat 0.0; PFloat 255.0; PFloat 255.0 ] ] } h.CurrentDoc, "curves")
    check "adjustment applied (layer count stable)" (Doc.layerCount h.CurrentDoc = n0)
    h.Push(Doc.appendLayer "text" (Doc.textLayer 100.0 100.0 "Hi") h.CurrentDoc, "add text")
    check "tool: appendLayer grows stack" (Doc.layerCount h.CurrentDoc = n0 + 1)
    h.Push(Doc.setTranslate 0 50.0 25.0 h.CurrentDoc, "move")
    let tx, ty = Doc.getTranslate 0 h.CurrentDoc
    check "tool: translate round-trips" (abs (tx - 50.0) < 1e-6 && abs (ty - 25.0) < 1e-6)
    // chat mini-agent + dual time-travel
    let cbefore = Doc.normalize loaded
    let cafter, creply, ctool = Chat.interpret "make it vintage" cbefore
    check "chat: interpret edits the doc" (not (System.Object.ReferenceEquals(cbefore, cafter)))
    check "chat: reply is non-empty" (creply.Length > 0)
    check "chat: tool label is set" (ctool <> "")
    check "chat: edit yields an AST diff" (not (List.isEmpty (Diff.diff cbefore cafter)))
    let cnoop, _, _ = Chat.interpret "asdf" cbefore
    check "chat: unknown command is a no-op" (System.Object.ReferenceEquals(cbefore, cnoop))
    h.JumpTo 0
    check "time-travel to root restores initial" ((top ()).Visible = v0)
    printfn ""
    printfn "ImageEdit.App: %d passed, %d failed" ok bad
    if bad = 0 then 0 else 1

// Headless GL render (EGL pbuffer, llvmpipe) → PNG, so --shotgl shows the real ShaderFill + frost.
let private writePng (fb: Framebuffer) (outPath: string) =
    let n = fb.W * fb.H
    let bytes = Array.zeroCreate<byte> (n * 4)
    let span = fb.Px.Span
    for i in 0 .. n - 1 do
        let cc = Color.fromPremul (Pixel.toColor span.[i])
        let o = i * 4
        bytes.[o] <- cc.B; bytes.[o + 1] <- cc.G; bytes.[o + 2] <- cc.R; bytes.[o + 3] <- cc.A
    let psi = ProcessStartInfo("ffmpeg")
    psi.UseShellExecute <- false; psi.RedirectStandardInput <- true; psi.RedirectStandardError <- true
    [ "-y"; "-f"; "rawvideo"; "-pixel_format"; "bgra"; "-video_size"; sprintf "%dx%d" fb.W fb.H; "-i"; "pipe:0"; "-frames:v"; "1"; outPath ]
    |> List.iter psi.ArgumentList.Add
    let p = Process.Start psi
    p.StandardInput.BaseStream.Write(bytes, 0, bytes.Length); p.StandardInput.BaseStream.Flush(); p.StandardInput.Close()
    p.StandardError.ReadToEnd() |> ignore; p.WaitForExit()

let private shotGl (w: int) (h: int) (root: unit -> Element) (outPath: string) =
    Scheduler.requestFrame <- ignore
    let mutable rootEl = Unchecked.defaultof<Element>
    createRoot (fun _ -> rootEl <- root ()) |> ignore
    rootEl.Layout(BoxConstraints.tightWH (float w) (float h))
    flushLayout ()
    let cmds = Clear Color.black :: rootEl.Paint(0.0, 0.0)
    let fb = Renderer.Scene.render (GlBackend.create ()) w h cmds
    writePng fb outPath

[<EntryPoint>]
let main argv =
    let args = Array.toList argv
    let docPath = args |> List.tryFind (fun (a: string) -> a.EndsWith ".imed") |> Option.defaultValue "docs/compose.imed"
    let cfg, loaded = Sexp.loadDoc (File.ReadAllText docPath)
    if List.contains "--selftest" args then
        selfTest loaded
    else
        let state = AppState(cfg, Doc.normalize loaded, List.contains "--shot" args || List.contains "--shotgl" args)
        // stage the editor for a screenshot (works for both --shot and --shotgl): set a tool, open a
        // menu / dialog, or run a chat turn from the 3rd arg.
        (match args with
         | _ :: _ :: "menu" :: _ -> state.MenuOpen.Value <- 2
         | _ :: _ :: "chat" :: _ -> state.ChatInput.Value <- "make it vintage"; state.SendChat()
         | _ :: _ :: "shape" :: _ -> state.CurrentTool.Value <- ShapeTool
         | _ :: _ :: "text" :: _ -> state.CurrentTool.Value <- TextTool
         | _ :: _ :: "gradient" :: _ -> state.CurrentTool.Value <- GradientTool
         | _ :: _ :: "pick" :: _ -> state.CurrentTool.Value <- PickTool; state.SamplePixel(float cfg.W * 0.5, float cfg.H * 0.4)
         | _ :: _ :: which :: _ -> adjustments |> List.tryFind (fun a -> a.Name = which) |> Option.iter (fun spec -> state.OpenDialog(spec, 0))
         | _ -> ())
        match args with
        | "--shotgl" :: out :: _ ->
            shotGl 1400 900 (fun () -> view state) out
            printfn "wrote %s (GL)" out
            0
        | "--shot" :: out :: _ ->
            Wisp.Host.shot 1400 900 (fun () -> view state) out
            printfn "wrote %s" out
            0
        | _ ->
            Wisp.Host.keyHandler.Value <- state.OnKey
            Wisp.Host.run "ImageEdit" 1400 900 (fun () -> view state)
            0
