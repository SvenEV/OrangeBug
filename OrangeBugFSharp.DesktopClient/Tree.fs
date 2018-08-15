namespace OrangeBug.DesktopClient

type 'a Tree = { value: 'a; children: 'a Tree list }

module Tree =
    let create value children = { value = value; children = children }
    let empty value = create value []

    let rec iter f state tree =
        let newState = f tree.value state
        tree.children |> Seq.iter (iter f newState)
    
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

type SceneNodeId = string

type SceneNode = {
    id: SceneNodeId
    components: obj list
}

module SceneNode =
    let private isExactly<'a> (o: obj) = o.GetType() = typedefof<'a>
    let private removeOfType<'a> = List.filter (isExactly<'a> >> not)

    let hasComponent<'a> node =
        node.components |> List.exists isExactly<'a>

    let tryGetComponent<'a> node =
        node.components
        |> List.tryFind isExactly<'a>
        |> Option.map (fun c -> c :?> 'a)
    
    let removeComponent<'a> node =
        { node with components = node.components |> removeOfType<'a> }
    
    let addOrReplaceComponent<'a> comp node =
        { node with components = comp :: (node.components |> removeOfType<'a>) }


type SceneGraph = {
    nodes: Map<SceneNodeId, SceneNode>
    tree: SceneNode Tree
}

module SceneGraph =
    let private failAlreadyExists id = failwithf "SceneGraph already contains a node with ID '%A'" id
    let private failNotExists id = failwithf "SceneGraph does not contain a node with ID '%A'" id
    let private failWrongType id expected = failwithf "Node with ID '%s' is not of expected type '%A'" id expected

    let rootId = ""
    let private rootNode = { id = rootId; components = [] }

    let empty = {
        nodes = Map.empty |> Map.add rootId rootNode
        tree = Tree.empty rootNode
    }

    /// Traverses the scene graph, applying 'f' to every scene node while accumulating a 'state' value.
    let rec iter f state graph =
        let rec iterCore state node =
            let nextState = f state node.value
            node.children |> Seq.iter (iterCore nextState)
        iterCore state graph.tree
    
    /// Computes a result value by traversing the scene graph, applying 'f' to every scene node.
    /// Going down the tree, a state value is threaded down to the leaves via 'f'.
    /// At the leaves, the state is transformed into a result value via 'stateToResult'.
    /// Going up, the results from children are merged with 'mergeResults'.
    let rec fold f stateToResult mergeResults initialState graph =
        let rec iterCore state node =
            let nextState = f state node.value
            match node.children with
            | [] -> stateToResult nextState
            | list -> list |> Seq.map (iterCore nextState) |> mergeResults
        iterCore initialState graph.tree

    /// Traverses the scene graph, applying 'f' to every component of a given type while accumulating a 'state' value.
    let rec iterComponents<'a, 'c> f (state: 'a) graph =
        let rec iterCore state node =
            let nextState =
                match node.value |> SceneNode.tryGetComponent<'c> with
                | Some comp -> f state node.value comp
                | None -> state
            node.children |> Seq.iter (iterCore nextState)
        iterCore state graph.tree
    
    let get id graph = graph.nodes |> Map.find id
    let tryGet id graph = graph.nodes |> Map.tryFind id
    let tryGetComponent<'a> id graph = tryGet id graph |> Option.bind SceneNode.tryGetComponent<'a>

    /// Removes a node and the entire subtree below it from a scene graph.
    let remove id graph =
        if id = rootId then failwithf "Cannot remove root node"
        let tree, removed = graph.tree |> Tree.removeWhere (fun n -> n.id = id)
        let nodes = removed |> Seq.fold (fun map n -> Map.remove n.id map) graph.nodes
        { nodes = nodes; tree = tree }

    /// Adds an entire new subtree to a scene graph. Throws if parts of the subtree are already part of the graph.
    let addSubtree parentId subtree graph =
        let subtreeNodes = subtree |> Tree.asSeq

        if not (graph.nodes.ContainsKey parentId) then
            failwithf "The desired parent node (ID: '%s') does not exist" parentId

        if subtreeNodes |> Seq.exists (fun n -> graph.nodes.ContainsKey n.id) then
            failwithf "At least one of the nodes in the subtree is already part of the scene graph"
        
        {
            nodes = subtreeNodes |> Seq.fold (fun map n -> Map.add n.id n map) graph.nodes;
            tree = graph.tree |> Tree.replaceWhere (fun n -> n.id = parentId) (fun n -> Tree.create n.value (subtree :: n.children))
        }

    /// Adds a node with the given ID and components or replaces an existing node, keeping the subtree below intact.
    let addOrReplace parentId node graph =
        if parentId = node.id then failwithf "A node cannot be its own parent"
        let nodes = graph.nodes |> Map.add node.id node
        let tree =
            if graph.nodes.ContainsKey node.id
            then graph.tree |> Tree.replaceWhere (fun n -> n.id = node.id) (fun n -> Tree.create node n.children)
            else graph.tree |> Tree.replaceWhere (fun n -> n.id = parentId) (fun n -> Tree.create n.value (Tree.empty node :: n.children))
        { nodes = nodes; tree = tree }