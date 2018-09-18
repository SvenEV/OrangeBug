namespace OrangeBug.DesktopClient

type 'a Tree = { value: 'a; children: 'a Tree list }

type TreeTraverseEvent<'state, 'a> =
    | Enter of 'state * 'a
    | Leave of 'state * 'a

module Tree =
    let create value children = { value = value; children = children }
    let empty value = create value []

    /// Traverses the tree, applying 'f' to every node while accumulating a 'state' value.
    let rec iter f state tree =
        let newState = f tree.value state
        tree.children |> Seq.iter (iter f newState)

    // Computes a result value by traversing the tree, applying 'f' every time a node
    // is entered (going down the tree) or left (going up).
    let rec foldEvents f initialState tree =
        let rec iterCore state node =
            let state = f (Enter(state, node.value))
            let state = Seq.fold iterCore state node.children
            f (Leave(state, node.value))
        iterCore initialState tree
    
    /// Computes a result value by traversing the tree, applying 'f' to every node.
    /// Going down the tree, a state value is threaded down to the leaves via 'f'.
    /// At the leaves, the state is transformed into a result value via 'stateToResult'.
    /// Going up, the results from children are merged with 'mergeResults'.
    let rec fold f stateToResult mergeResults initialState tree =
        let rec iterCore state node =
            let nextState = f state node.value
            match node.children with
            | [] -> stateToResult nextState
            | list -> list |> Seq.map (iterCore nextState) |> mergeResults
        iterCore initialState tree
    
    let rec asSeq tree = seq {
        yield tree.value
        yield! tree.children |> Seq.collect asSeq
    }

    type private 'a Removal = Keep of 'a Tree * 'a seq | Remove of 'a

    let removeWhere predicate tree =
        let rec remove node =
            if predicate node.value then
                Remove node.value
            else
                let results = node.children |> List.map remove
                let childrenToKeep = results |> List.choose (function Keep (c, _) -> Some c | Remove _ -> None)
                let removed = results |> Seq.collect (function Keep (_, r) -> r | Remove v -> Seq.singleton v)
                Keep ({ node with children = childrenToKeep }, removed)
        match remove tree with
        | Keep (node, removed) -> node, removed
        | Remove _ -> failwithf "Cannot remove root node"

    let rec replaceWhere predicate replacer tree =
        if predicate tree.value
        then replacer tree
        else { tree with children = tree.children |> List.map (replaceWhere predicate replacer) }
