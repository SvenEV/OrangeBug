namespace OrangeBug.Grid

open OrangeBug

// An immutable quadtree

// Coordinate system:
//
// ^ Y
// |
// -------
// |NW|NE|
// -------
// |SW|SE|
// 0-----------> X

type Cell = SW | SE | NW | NE

type 'a Node =
    | Empty
    | Leaf of 'a
    | InnerNode of 'a InnerNode
    
and 'a InnerNode =
    {
        sw: 'a Node
        se: 'a Node
        nw: 'a Node
        ne: 'a Node
    }
    static member Create child = { sw = child; se = child; nw = child; ne = child }
    member n.Cell c =
        match c with
        | SW -> n.sw
        | SE -> n.se
        | NW -> n.nw
        | NE -> n.ne

type 'a Grid = {
    size: Point
    root: 'a Node
}

module Grid =
    open System
    open System.Collections.Generic

    let empty size = {
        size = size
        root = Empty
    }

    let private log2 v = Math.Log(v, 2.0)
    let private treeSize size = max size.x size.y |> float |> log2 |> ceil |> (fun i -> 2.0 ** i) |> int

    let private classify p nodeSize =
        match sign (p.x - nodeSize / 2), sign (p.y - nodeSize / 2) with
        | -1, -1 -> SW
        | _, -1 -> SE
        | -1, _ -> NW
        | _, _ -> NE

    // Returns a new query point that is relative to the given child cell
    let private adjustPoint cell nodeSize p =
        match cell with
        | SW -> p
        | SE -> p - (nodeSize / 2) * Point.unitX
        | NW -> p - (nodeSize / 2) * Point.unitY
        | NE -> p - (nodeSize / 2) * Point.one

    let tryFind p grid =
        let rec findCore p node nodeSize =
            match node with
            | Empty -> None
            | Leaf v -> Some v
            | InnerNode node ->
                let cell = classify p nodeSize
                findCore (adjustPoint cell nodeSize p) (node.Cell cell) (nodeSize / 2)

        findCore p grid.root (treeSize grid.size)

    let find p grid =
        match tryFind p grid with
        | Some result -> result
        | None -> raise (KeyNotFoundException(sprintf "Key '%O' not found in Grid" p))
    
    let add p value grid =
        let rec makeNode p node nodeSize =
            match nodeSize with
            | 1 -> Leaf value
            | _ -> 
                let halfNodeSize = nodeSize / 2
                let cell = classify p nodeSize
                match node with
                | Empty ->
                    let newChild = makeNode (adjustPoint cell nodeSize p) Empty halfNodeSize
                    InnerNode
                        (match cell with
                        | SW -> { InnerNode.Create Empty with sw = newChild }
                        | SE -> { InnerNode.Create Empty with se = newChild }
                        | NW -> { InnerNode.Create Empty with nw = newChild }
                        | NE -> { InnerNode.Create Empty with ne = newChild })
                | InnerNode node ->
                    let newChild = makeNode (adjustPoint cell nodeSize p) (node.Cell cell) halfNodeSize
                    InnerNode
                        (match cell with
                        | SW -> { node with sw = newChild }
                        | SE -> { node with se = newChild }
                        | NW -> { node with nw = newChild }
                        | NE -> { node with ne = newChild })
                | Leaf _ ->
                    failwith "fatal Grid error: Reached leaf node at nodeSize > 1"

        let newRoot = makeNode p grid.root (treeSize grid.size)
        { size = grid.size; root = newRoot }

    let remove p grid =
        let simplify node =
            match node with
            | InnerNode { sw = Empty; se = Empty; nw = Empty; ne = Empty } -> Empty
            | _ -> node

        let rec mergeNode p node nodeSize =
            match nodeSize with
            | 1 -> Empty
            | _ ->
                match node with
                | Empty -> Empty
                | InnerNode node ->
                    let cell = classify p nodeSize
                    let newChild = mergeNode (adjustPoint cell nodeSize p) (node.Cell cell) (nodeSize / 2)
                    simplify (InnerNode 
                        (match cell with
                        | SW -> { node with sw = newChild }
                        | SE -> { node with se = newChild }
                        | NW -> { node with nw = newChild }
                        | NE -> { node with ne = newChild }))

                | Leaf _ ->
                    failwith "fatal Grid error: Reached leaf node at nodeSize > 1"

        let newRoot = mergeNode p grid.root (treeSize grid.size)
        { size = grid.size; root = newRoot }

    let init size initializer =
        Seq.allPairs [0 .. size.y - 1] [0 .. size.x - 1]
        |> Seq.fold
            (fun grid (y, x) ->
                let p = Point.create x y
                add p (initializer p) grid)
            (empty size)
    
    let asSeq grid =
        let rec enumerateCore p node nodeSize = seq {
            let halfNodeSize = nodeSize / 2
            match node with
            | Leaf v -> yield p, v
            | Empty -> ()
            | InnerNode node ->
                yield! enumerateCore (p) node.sw halfNodeSize
                yield! enumerateCore (p + halfNodeSize * Point.unitX) node.se halfNodeSize
                yield! enumerateCore (p + halfNodeSize * Point.unitY) node.nw halfNodeSize
                yield! enumerateCore (p + halfNodeSize * Point.one) node.ne halfNodeSize
        }

        enumerateCore Point.zero grid.root (treeSize grid.size)

    let asCharBitmap grid =
        let bitmapSize = (2 * (treeSize grid.size) + 1) * Point.one
        let bitmap = CharBitmap.create bitmapSize ' '

        bitmap |> CharBitmap.fillRectangle '.'
            (Point.create 0 (bitmapSize.y - 2 * grid.size.y))
            (2 * grid.size)

        let rec drawNode node nodeSize (offset: Point) =
            bitmap |> CharBitmap.drawBox (2 * offset) ((2 * nodeSize + 1) * Point.one)
            match node with
            | InnerNode inner ->
                let halfNodeSize = nodeSize / 2
                drawNode inner.nw halfNodeSize offset
                drawNode inner.ne halfNodeSize (offset + halfNodeSize * Point.unitX)
                drawNode inner.sw halfNodeSize (offset + halfNodeSize * Point.unitY)
                drawNode inner.se halfNodeSize (offset + halfNodeSize * Point.one)
            | Leaf value ->
                CharBitmap.setPixel (value.ToString().PadLeft(1, '?').[0]) bitmap (2 * offset + Point.one)
            | Empty -> ()

        drawNode grid.root (treeSize grid.size) Point.zero
        bitmap

type 'a Grid with
    member this.Add p value = Grid.add p value this
    member this.Remove p = Grid.remove p this
    member this.TryFind p = Grid.tryFind p this
    member this.Find p = Grid.find p this
    member this.AsSeq = Grid.asSeq this