module Rbt.ImageEdit.App.Chat

// A tiny local "agent": maps a chat instruction to a non-destructive document edit (an adjustment
// on the base layer). Stands in for a live LLM (which would rewrite the .imed program) so the chat +
// dual-time-travel flow is demonstrable without an API. Returns (newDoc, assistantReply).

open Rbt.Renderer
open Rbt.ImageEdit

let private adj name pars (doc: Layer) = Doc.applyAdjust 0 { Name = name; Params = pmap pars } doc

/// Interpret a chat instruction. Returns (newDoc, assistantReply, toolLabel). `toolLabel` names the
/// adjustment(s) the "tool call" performed (empty when nothing was applied), so the chat panel can
/// render a tool-call card — mirroring chatkit's Tool entry kind.
let interpret (text: string) (doc: Layer) : Layer * string * string =
    let t = text.ToLowerInvariant()
    let has (s: string) = t.Contains s
    if has "vintage" then
        doc
        |> adj "saturate" [ "amount", PFloat 0.85 ]
        |> adj "temperature" [ "amount", PFloat 0.3 ]
        |> adj "vignette" [ "amount", PFloat 0.5; "radius", PFloat 0.5 ],
        "Applied a vintage look — warm tone, lower saturation, vignette.", "saturate · temperature · vignette"
    elif has "bright" then adj "brightness" [ "amount", PFloat 1.3 ] doc, "Brightened the image.", "brightness"
    elif has "dark" then adj "brightness" [ "amount", PFloat 0.7 ] doc, "Darkened the image.", "brightness"
    elif has "gray" || has "grey" || has "black and white" || has "b&w" then
        adj "grayscale" [ "amount", PFloat 1.0 ] doc, "Converted to black & white.", "grayscale"
    elif has "saturat" then adj "saturate" [ "amount", PFloat 1.6 ] doc, "Boosted the saturation.", "saturate"
    elif has "vignette" then adj "vignette" [ "amount", PFloat 0.6; "radius", PFloat 0.5 ] doc, "Added a vignette.", "vignette"
    elif has "blur" then adj "blur" [ "radius", PFloat 10.0 ] doc, "Blurred the image.", "blur"
    elif has "warm" then adj "temperature" [ "amount", PFloat 0.35 ] doc, "Warmed the colours.", "temperature"
    elif has "cool" then adj "temperature" [ "amount", PFloat -0.35 ] doc, "Cooled the colours.", "temperature"
    elif has "sharp" then adj "sharpen" [ "amount", PFloat 1.0 ] doc, "Sharpened the image.", "sharpen"
    elif has "contrast" then adj "contrast" [ "amount", PFloat 1.3 ] doc, "Increased the contrast.", "contrast"
    else doc, "Try: brighten, darken, b&w, saturate, vignette, blur, warm, cool, sharpen, contrast, vintage.", ""
