open System
open System.Collections.Generic
open Utils.Pair
open Utils.SampleData
open Utils.String

type Node = { name: string; mutable connections: Set<string> }

let toNodes (data: (string * string) array) =
    let dic = Dictionary<string, Node>()

    for name, connectedTo in data do
        match dic.TryGetValue name with
        | true, node -> node.connections <- node.connections |> Set.add connectedTo
        | false, _ -> dic.Add(name, { name = name; connections = Set.singleton connectedTo })

        match dic.TryGetValue connectedTo with
        | true, node -> node.connections <- node.connections |> Set.add name
        | false, _ -> dic.Add(connectedTo, { name = connectedTo; connections = Set.singleton name })

    dic

let findTriangles (nodes: Node list) =
    let nodeMap = nodes |> Seq.map (fun n -> n.name, n) |> Map.ofSeq

    let connectionsOf name =
        match Map.tryFind name nodeMap with
        | Some node -> node.connections
        | None -> Set.empty

    nodes
    |> List.collect (fun u ->
        u.connections
        |> Set.toList
        |> List.collect (fun v ->
            if u.name < v then
                let vConnections = connectionsOf v
                let commonNeighbors = Set.intersect u.connections vConnections

                commonNeighbors
                |> Set.toList
                |> List.filter (fun w -> v < w)
                |> List.map (fun w -> (u.name, v, w))
            else
                []))

let findCliques (nodes: Node list) =
    let nodeMap = nodes |> Seq.map (fun n -> n.name, n) |> Map.ofSeq

    let connectionsOf name =
        match Map.tryFind name nodeMap with
        | Some node -> node.connections
        | None -> Set.empty

    let rec bronKerbosch (r: Set<string>) (p: Set<string>) (x: Set<string>) (cliques: Set<Set<string>>) =
        if Set.isEmpty p && Set.isEmpty x then
            Set.add r cliques
        else
            let pivot = Set.union p x |> Set.minElement

            let nonNeighbors =
                match Map.tryFind pivot nodeMap with
                | Some node -> Set.difference p node.connections
                | None -> p

            let mutable newCliques = cliques

            for v in Set.toList nonNeighbors do
                let vConnections = connectionsOf v
                let r' = Set.add v r
                let p' = Set.intersect p vConnections
                let x' = Set.intersect x vConnections
                newCliques <- bronKerbosch r' p' x' newCliques

            newCliques

    let allNodes = nodes |> List.map _.name |> Set.ofList
    bronKerbosch Set.empty allNodes Set.empty Set.empty

let data =
    loadSampleDataLines "Puzzle1.txt"
    |> Array.filter (fun x -> x.Length > 0)
    |> Array.map (split "-" >> fromArray)
    |> toNodes
    |> _.Values
    |> Seq.toList

let solution1 data =
    findTriangles data
    |> List.filter (fun (a, b, c) -> a.StartsWith("t") || b.StartsWith("t") || c.StartsWith("t"))
    |> List.length
    |> string

let solution2 data =
    findCliques data
    |> Set.toList
    |> List.maxBy Set.count
    |> Set.toList
    |> List.sort
    |> fun x -> String.Join(",", x)

run [ solution1; solution2 ] data
