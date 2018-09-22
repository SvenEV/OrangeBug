namespace OrangeBug

open System.Collections.Generic

type 'a Graph when 'a: comparison = {
    inEdges: Map<'a, 'a Set>
    outEdges: Map<'a, 'a Set>
}

module Graph =
    let empty = { inEdges = Map.empty; outEdges = Map.empty }

    let addEdge source target graph = {
        inEdges =
            match graph.inEdges.TryFind target with
            | Some set -> set.Add source
            | None -> Set.singleton source
            |> fun set -> graph.inEdges.Add(target, set)
        outEdges =
            match graph.outEdges.TryFind source with
            | Some set -> set.Add target
            | None -> Set.singleton target
            |> fun set -> graph.outEdges.Add(source, set)
    }

    let removeEdge source target graph = {
        inEdges =
            match graph.inEdges.TryFind target with
            | Some set -> graph.inEdges.Add(target, set.Remove source)
            | None -> graph.inEdges
        outEdges =
            match graph.outEdges.TryFind source with
            | Some set -> graph.outEdges.Add(source, set.Remove target)
            | None -> graph.outEdges
    }

    let removeInEdges node graph = {
        inEdges = graph.inEdges.Remove node
        outEdges = graph.outEdges |> Map.map (fun _ targets -> targets.Remove node)
    }

    let removeOutEdges node graph = {
        inEdges = graph.inEdges |> Map.map (fun _ sources -> sources.Remove node)
        outEdges = graph.outEdges.Remove node
    }

    let removeEdges node =
        removeInEdges node >> removeOutEdges node

    let seqFromDfs root graph =
        let visited = HashSet()
        let rec iter node = seq {
            if not (visited.Contains node) then
                visited.Add node |> ignore
                yield node
                yield! graph.outEdges.[node] |> Seq.collect iter
        }
        iter root
    
    let seqFromBfs root graph = seq {
        let queue = Queue()
        let visited = HashSet()
        queue.Enqueue root
        while queue.Count > 0 do
            let node = queue.Dequeue()
            if not (visited.Contains node) then
                visited.Add node |> ignore
                yield node
                match graph.outEdges.TryFind node with
                | Some neighbors -> neighbors |> Seq.iter queue.Enqueue
                | _ -> ()
    }

    /// <summary>
    /// Finds leaf nodes in the subgraph consisting of <paramref="nodes" />
    /// and the edges from <paramref="graph" />.
    /// </summary>
    let findLeafs nodes graph =
        nodes
        |> Seq.filter (fun p ->
            match graph.outEdges.TryFind p with
            | None -> true
            | Some neighbors -> (Set.intersect neighbors nodes).IsEmpty)

type 'a Graph when 'a: comparison with
    member this.AddEdge source target = Graph.addEdge source target this
    member this.RemoveEdge source target = Graph.removeEdge source target this
    member this.RemoveInEdges node = Graph.removeInEdges node this
    member this.RemoveOutEdges node = Graph.removeOutEdges node this
