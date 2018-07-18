namespace OrangeBug

type DependencyGraph = {
    inEdges: Map<Point, Point Set> // e.g. for a button, all gates triggered by that button
    outEdges: Map<Point, Point Set> // e.g. for a gate, the button that triggers the gate
}

module DependencyGraph =
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

    let removeInEdges position graph = {
        inEdges = graph.inEdges.Remove position
        outEdges = graph.outEdges |> Map.map (fun _ targets -> targets.Remove position)
    }

    let removeOutEdges position graph = {
        inEdges = graph.inEdges |> Map.map (fun _ sources -> sources.Remove position)
        outEdges = graph.outEdges.Remove position
    }

    /// <summary>
    /// Finds leaf nodes in the subgraph consisting of <paramref="points" />
    /// and the edges from <paramref="graph" />.
    /// </summary>
    let findLeafs points graph =
        points
        |> Seq.filter (fun p ->
            match graph.outEdges.TryFind p with
            | None -> true
            | Some otherPoints -> (Set.intersect otherPoints points).IsEmpty)

type DependencyGraph with
    static member empty = { inEdges = Map.empty; outEdges = Map.empty }
    member this.addEdge source target = DependencyGraph.addEdge source target this
    member this.removeEdge source target = DependencyGraph.removeEdge source target this
    member this.removeInEdges position = DependencyGraph.removeInEdges position this
    member this.removeOutEdges position = DependencyGraph.removeOutEdges position this
