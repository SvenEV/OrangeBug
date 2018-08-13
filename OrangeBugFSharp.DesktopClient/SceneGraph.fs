namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open System.Collections.Generic
open OrangeBug

type SceneObjectId = string

type SceneObject = {
    id: SceneObjectId
    mutable state: obj
}

type Scene = {
    objects: Dictionary<SceneObjectId, SceneObject>
    mutable hierarchy: SceneObjectId Graph // with edges to children
}

module Scene =

    let rootId = ""

    let private failAlreadyExists id = failwithf "Scene already contains a SceneObject with ID '%A'" id
    let private failNotExists id = failwithf "Scene does not contain a SceneObject with ID '%A'" id

    let tryGet id scene =
        match scene.objects.TryGetValue id with
        | true, o -> Some o.state
        | _ -> None

    let get id scene =
        match scene.objects.TryGetValue id with
        | true, o -> o.state
        | _ -> failNotExists id

    let update id state scene =
        match scene.objects.TryGetValue id with
        | true, o -> o.state <- state
        | _ -> failNotExists id

    let add id parentId initialState scene =
        if scene.objects.ContainsKey id then
            failAlreadyExists id
        else
            scene.objects.Add(id, { id = id; state = initialState})
            scene.hierarchy <- scene.hierarchy.AddEdge parentId id
    
    /// Removes a SceneObject and all its descendants    
    let remove id scene =
        let subtreeIds = scene.hierarchy |> Graph.seqFromBfs id |> Set.ofSeq
        subtreeIds |> Seq.iter (scene.objects.Remove >> ignore)
        scene.hierarchy <- subtreeIds |> Seq.fold (fun g id -> Graph.removeEdges id g) scene.hierarchy

    let create() =
        let scene = { objects = Dictionary(); hierarchy = Graph.empty }
        scene |> add rootId rootId ()
        scene

    let parentId id scene =
        match scene.hierarchy.inEdges.TryFind id with
        | None -> failNotExists id
        | Some parents -> parents.MinimumElement

    let parent id scene =
        let parentId = parentId id scene
        match scene.objects.TryGetValue(parentId) with
        | true, parent -> parent
        | _ -> failwithf "Internal error: SceneObject '%A' has parent '%A' which does not exist" id parentId

    let childrenIds id scene =
        match scene.hierarchy.outEdges.TryFind id with
        | None -> if scene.objects.ContainsKey id then Seq.empty else failNotExists id
        | Some children -> children |> Seq.except [ rootId ]

    let children id scene =
        let childrenIds = childrenIds id scene
        childrenIds |> Seq.map (fun childId ->
            match scene.objects.TryGetValue childId with
            | true, child -> child
            | _ -> failwithf "Internal error: SceneObject '%A' has child '%A' which does not exist" id childId)
    
    let rec toSeqPreorder<'state> scene id = seq {
        match get id scene with
        | :? 'state as node -> yield node
        | _ -> ()
        yield! childrenIds id scene |> Seq.collect (toSeqPreorder scene)
    }

    let rec iterPreorder f temp id scene =
        let newTemp =
            match get id scene with
            | :? 'state as node -> f id node temp // TODO: Why doesn't this match all nodes if 'state = obj?
            | _ -> temp
        childrenIds id scene |> Seq.iter (fun child -> iterPreorder f newTemp child scene)
    
type Scene with
    member scene.Get id = Scene.get id scene
    member scene.Update id state = Scene.update id state scene
    member scene.Add id parentId initialState = Scene.add id parentId initialState scene
    member scene.Remove id = Scene.remove id scene
    member scene.ParentId id = Scene.parentId id scene
    member scene.ChildrenIds id = Scene.childrenIds id scene
    member scene.IterPreorder f temp id = Scene.iterPreorder f temp id scene