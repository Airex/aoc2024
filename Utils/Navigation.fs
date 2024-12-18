namespace Utils

open System.Collections.Generic

/// Represents a node in the graph.
type Node<'T> =
    { Position: 'T
      GCost: float // Cost from start to this node
      HCost: float // Heuristic cost to the goal
      FCost: float // Total cost (G + H)
      Parent: Node<'T> option } // Parent node in the path

module Navigation =
    /// A* algorithm implementation
    let aStar
        (start: 'T)
        (goal: 'T)
        (goalReached: 'T -> 'T -> bool) // Function to check if the goal has been reached
        (neighbors: 'T -> ('T * float) list) // Function to get neighbors and their costs
        (heuristic: 'T -> 'T -> float) // Heuristic function
        : ('T list list * float) option = // Returns the paths as a list of nodes, or None if no paths exists

        let openSet = HashSet<Node<'T>>()
        let closedSet = HashSet<'T>()
        let nodeMap = Dictionary<'T, Node<'T>>()

        // Create the start node
        let startNode =
            { Position = start
              GCost = 0.0
              HCost = heuristic start goal
              FCost = heuristic start goal
              Parent = None }

        openSet.Add(startNode) |> ignore
        nodeMap[start] <- startNode

        let reconstructPath (node: Node<'T>) : 'T list =
            let rec f node lst =
                match node.Parent with
                | Some parent -> f parent (node.Position :: lst)
                | None -> List.rev (node.Position :: lst)

            f node []

        let rec loop () =
            if openSet.Count = 0 then
                None
            else
                // Get the node with the lowest F cost
                let currentNode = openSet |> Seq.minBy _.FCost

                // If we reached the goal, reconstruct and return the paths with same score
                if goalReached currentNode.Position goal then
                    let minCost = currentNode.FCost

                    let paths =
                        openSet
                        |> Seq.filter (fun x -> x.FCost = minCost)
                        |> Seq.map reconstructPath
                        |> List.ofSeq

                    Some(paths, currentNode.FCost)
                else
                    openSet.Remove(currentNode) |> ignore
                    closedSet.Add(currentNode.Position) |> ignore

                    // Process each neighbor
                    for neighborPos, cost in neighbors currentNode.Position do
                        if not (closedSet.Contains(neighborPos)) then
                            let tentativeGCost = currentNode.GCost + cost

                            match nodeMap.TryGetValue(neighborPos) with
                            | true, neighborNode when tentativeGCost < neighborNode.GCost ->
                                // Update the neighbor's costs and parent
                                let updatedNeighbor =
                                    { neighborNode with
                                        GCost = tentativeGCost
                                        FCost = tentativeGCost + neighborNode.HCost
                                        Parent = Some currentNode }

                                nodeMap[neighborPos] <- updatedNeighbor

                                if not (openSet.Contains(neighborNode)) then
                                    openSet.Add(updatedNeighbor) |> ignore
                            | _, _ ->
                                // Create a new neighbor node
                                let hCost = heuristic neighborPos goal

                                let newNode =
                                    { Position = neighborPos
                                      GCost = tentativeGCost
                                      HCost = hCost
                                      FCost = tentativeGCost + hCost
                                      Parent = Some currentNode }

                                nodeMap[neighborPos] <- newNode
                                openSet.Add(newNode) |> ignore

                    loop ()

        loop ()

    type BinarySide =
        | Left
        | Right

    let binarySearch min max check =
        let rec recurse lowerBound upperBound =
            if lowerBound > upperBound then
                lowerBound
            else
                let midPoint = (upperBound + lowerBound) / 2
                let midValue = check midPoint

                match midValue with
                | BinarySide.Left -> recurse lowerBound (midPoint - 1)
                | BinarySide.Right -> recurse (midPoint + 1) upperBound

        recurse min max
