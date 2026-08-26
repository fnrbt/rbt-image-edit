module Rbt.ImageEdit.App.History

open Rbt.ImageEdit

/// One immutable document version in the time-travel tree.
type Snapshot = { Id: int; Parent: int option; Doc: Layer; Label: string }

/// A tree of document snapshots with a `current` pointer. Undo = move to parent, redo = newest
/// child, jumpTo = time-travel to any node (a subsequent edit branches). Immutable `Layer` docs
/// make every snapshot cheap (structural sharing). Phase 6 will link chat nodes to snapshot ids.
type History(initial: Layer) =
    let nodes = ResizeArray<Snapshot>([ { Id = 0; Parent = None; Doc = initial; Label = "open" } ])
    let mutable cur = 0
    member _.Nodes = List.ofSeq nodes
    member _.CurrentId = cur
    member _.CurrentDoc = nodes.[cur].Doc
    member _.CanUndo = nodes.[cur].Parent.IsSome
    member _.CanRedo = nodes |> Seq.exists (fun n -> n.Parent = Some cur)
    /// Append a new version as a child of the current node and move to it.
    member _.Push(doc: Layer, label: string) =
        let id = nodes.Count
        nodes.Add { Id = id; Parent = Some cur; Doc = doc; Label = label }
        cur <- id
    /// For live drags: if the current node has the same label, amend it in place; else push a new
    /// node. Coalesces a slider drag into a single history entry.
    member _.ReplaceOrPush(doc: Layer, label: string) =
        if cur <> 0 && nodes.[cur].Label = label then
            nodes.[cur] <- { nodes.[cur] with Doc = doc }
        else
            let id = nodes.Count
            nodes.Add { Id = id; Parent = Some cur; Doc = doc; Label = label }
            cur <- id
    member _.Undo() = match nodes.[cur].Parent with Some p -> cur <- p; true | None -> false
    member _.Redo() =
        match nodes |> Seq.filter (fun n -> n.Parent = Some cur) |> Seq.tryLast with
        | Some child -> cur <- child.Id; true
        | None -> false
    member _.JumpTo(id: int) = if id >= 0 && id < nodes.Count then cur <- id
    /// Depth of a node (for indented display of the tree).
    member _.Depth(id: int) =
        let mutable d = 0
        let mutable n = id
        while nodes.[n].Parent.IsSome do n <- nodes.[n].Parent.Value; d <- d + 1
        d
