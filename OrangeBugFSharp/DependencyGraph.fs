namespace OrangeBug

module DependencyGraph =
    
    type DependencyGraph = {
        inEdges: Map<Point, Point Set> // e.g. for a button, all gates triggered by that button
        outEdges: Map<Point, Point Set> // e.g. for a gate, the button that triggers the gate
    }

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

    type DependencyGraph with
        static member empty = { inEdges = Map.empty; outEdges = Map.empty }
        member this.addEdge source target = addEdge source target this
        member this.removeEdge source target = removeEdge source target this
        member this.removeInEdges position = removeInEdges position this
        member this.removeOutEdges position = removeOutEdges position this
