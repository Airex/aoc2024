module Day20

open System.Collections.Generic
open Utils.Array2D
open Utils.Navigation
open Utils.Pair
open Utils.SampleData

let neigbors (x, y) =
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let aStarOnMap (grid: char array2d) start goal =
    let isValid ((x, y)) = inBounds grid x y && grid[x, y] <> '#'

    let neighbors (x, y) =
        (x, y)
        |> neigbors
        |> Seq.filter isValid
        |> Seq.map (fun pos -> pos, 1)
        |> List.ofSeq

    dijkstra neighbors start goal

let getReachablePoints (n: int) (start: int * int) : Set<int * int> =
    // Directions for movement (horizontal, vertical, diagonal)
    let directions = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] // Horizontal and vertical

    // BFS to explore all reachable points
    let visited = HashSet<int * int>()
    let queue = Queue<(int * int) * int>()

    // Start with the initial point
    queue.Enqueue((start, 0))
    visited.Add(start) |> ignore

    while queue.Count > 0 do
        let current, steps = queue.Dequeue()

        if steps < n then
            for dx, dy in directions do
                let nextPoint = fst current + dx, snd current + dy

                if not (visited.Contains(nextPoint)) then
                    visited.Add(nextPoint) |> ignore
                    queue.Enqueue((nextPoint, steps + 1))

    visited |> Set.ofSeq

let solve cheetLength minCheetWin data =
    let start = findIndex (fun c -> c = 'S') data |> Option.get
    let goal = findIndex (fun c -> c = 'E') data |> Option.get

    let race start = aStarOnMap data start goal
    let cache = race start |> Option.get |> List.mapi (fun i x -> x, i) |> Map

    let racePosition start =
        cache |> Map.tryFind start |> Option.defaultValue -1

    let getAvailableCheets n pos =
        pos
        |> getReachablePoints n
        |> Seq.filter (fun (x, y) -> inBounds data x y && data[x, y] <> '#')
        |> Seq.map (branch racePosition (distance pos))

    race start
    |> Option.get
    |> Seq.collect (fun (x, y) ->
        let i = racePosition (x, y)

        (x, y)
        |> getAvailableCheets cheetLength
        |> Seq.map (fun (r, d) -> r - i - d)
        |> Seq.filter (fun x -> x >= minCheetWin))
    |> Seq.length


let solution1 = solve 2 100
let solution2 = solve 20 100

let data =
    loadSampleDataLines "Puzzle2.txt" |> Array.filter (_.Length >> (<) 0) |> array2D

run [ solution1; solution2 ] data
