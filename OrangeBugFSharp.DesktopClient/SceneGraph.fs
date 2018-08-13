namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open System.Collections.Generic
open OrangeBug

type SceneObjectId = string

type SceneObject = {
    id: SceneObjectId
    update: GameTime -> unit
    draw: GraphicsDevice -> GameTime -> unit
}

type Scene = {
    objects: Dictionary<SceneObjectId, SceneObject>
    mutable hierarchy: SceneObjectId Graph // with edges to children
}

module Scene =
    let rootNode = {
        id = ""
        update = ignore
        draw = fun dev -> ignore
    }

    let private failAlreadyExists id = failwithf "Scene already contains a SceneObject with ID '%A'" id
    let private failNotExists id = failwithf "Scene does not contain a SceneObject with ID '%A'" id

    let tryGet id scene =
        match scene.objects.TryGetValue id with
        | true, o -> Some o
        | _ -> None

    let get id scene =
        match scene.objects.TryGetValue id with
        | true, o -> o
        | _ -> failNotExists id

    let add parent object scene =
        if scene.objects.ContainsKey object.id then
            failAlreadyExists object.id
        else
            scene.objects.Add(object.id, object)
            scene.hierarchy <- scene.hierarchy.AddEdge parent object.id
    
    let addUnderRoot = add rootNode.id

    /// Removes a SceneObject and all its descendants    
    let remove id scene =
        let subtreeIds = scene.hierarchy |> Graph.seqFromBfs id |> Set.ofSeq
        subtreeIds |> Seq.iter (scene.objects.Remove >> ignore)
        scene.hierarchy <- subtreeIds |> Seq.fold (fun g id -> Graph.removeEdges id g) scene.hierarchy

    let create() =
        let scene = { objects = Dictionary(); hierarchy = Graph.empty }
        scene |> add rootNode.id rootNode

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
        | None -> failNotExists id
        | Some children -> children

    let children id scene =
        let childrenIds = childrenIds id scene
        childrenIds |> Seq.map (fun childId ->
            match scene.objects.TryGetValue childId with
            | true, child -> child
            | _ -> failwithf "Internal error: SceneObject '%A' has child '%A' which does not exist" id childId)