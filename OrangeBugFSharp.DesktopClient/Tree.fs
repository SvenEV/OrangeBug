namespace OrangeBug.DesktopClient

type 'a Tree =
    | EmptyTree
    | TreeNode of 'a * 'a Tree list

module Tree =
    let rec iter f temp =
        function
        | EmptyTree -> ()
        | TreeNode (value, children) ->
            let newTemp = f value temp
            children |> Seq.iter (iter f newTemp)
    
    let rec asSeq tree = seq {
        match tree with
        | EmptyTree -> ()
        | TreeNode (value, children) ->
            yield value
            yield! children |> Seq.collect asSeq
    }

type SceneNodeId = string

type SceneNode = {
    id: SceneNodeId
    mutable state: obj
}

type SceneGraph = SceneNode Tree

module SceneGraph =
    let private failAlreadyExists id = failwithf "SceneGraph already contains a node with ID '%A'" id
    let private failNotExists id = failwithf "SceneGraph does not contain a node with ID '%A'" id
    let private failWrongType id expected = failwithf "Node with ID '%s' is not of expected type '%A'" id expected

    let rootNode = { id = ""; state = () }

    let rec iterAndUpdate f temp (tree: SceneGraph) =
        match tree with
        | EmptyTree -> ()
        | TreeNode (node, children) ->
            let newTemp =
                match node.state with
                | :? 'state as state ->
                    let newState, newTemp = f node.id state temp
                    node.state <- newState
                    newTemp
                | _ -> temp
            children |> Seq.iter (iterAndUpdate f newTemp)
    
    let rec iter f = iterAndUpdate (fun id (state: 'state) temp -> state, f id state temp)

    let get id tree =
        tree |> Tree.asSeq |> Seq.find (fun n -> n.id = id)

    let getAs<'a> id tree =
        let node = get id tree
        match node.state with
        | :? 'a as state -> state
        | _ -> failWrongType id typedefof<'a>

    let update<'a> id updater tree =
        match tree |> Tree.asSeq |> Seq.tryFind (fun n -> n.id = id) with
        | Some node ->
            match node.state with
            | :? 'a as state -> node.state <- updater state
            | _ -> failWrongType id typedefof<'a>
        | None -> failNotExists id

    let set id state tree =
        update id (fun _ -> state) tree

    let rec remove id =
        function
        | EmptyTree -> EmptyTree
        | TreeNode ({ id = i }, _) when i = id -> EmptyTree
        | TreeNode _ as node -> remove id node

    let rec add parentId id state =
        function
        | EmptyTree -> EmptyTree
        | TreeNode ({ id = i} as node, children) when i = parentId ->
            let newChild = TreeNode ({ id = id; state = state }, [])
            TreeNode (node, newChild :: children)
        | TreeNode _ as node -> add parentId id state node