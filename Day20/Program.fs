module Day20

open System.Linq
open FSharp.Collections.ParallelSeq
open Utils.Array2D
open Utils.Combinators
open Utils.Navigation
open Utils.Pair
open Utils.SampleData

let inline neigbors (x, y) =
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let inline distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let findPath inBounds get start goal =
    let isValid = S (&&) inBounds (get >> (<>) '#')
    let neighbors = neigbors >> List.filter isValid >> List.map (fun pos -> pos, 1)
    dijkstra neighbors start goal

let inline getReachablePoints gridSize get n (start: int * int) =
    seq {
        let maxX, maxY = gridSize
        let startX, startY = start

        // Constrain dx to valid x-range
        for dx in max -n (-startX) .. min n (maxX - startX - 1) do
            let y = n - abs dx

            // Constrain dy to valid y-range
            for dy in max -y (-startY) .. min y (maxY - startY - 1) do
                let pos = add start (dx, dy)

                if get pos <> '#' then
                    let dist = distance start pos

                    if dist > 0 && dist <= n && get pos <> '#' then
                        yield pos, dist
    }

let solve cheetLength minCheetWin data =
    let inBounds, get = data |> api
    let size = size data
    let start = findIndex (fun c -> c = 'S') data |> Option.get
    let goal = findIndex (fun c -> c = 'E') data |> Option.get

    let race start = findPath inBounds get start goal
    let path = race start |> Option.get
    let cache = path |> Seq.mapi (flip pair) |> dict

    let inline racePosition start = cache[start]

    let inline getAvailableCheets n pos =
        (n, pos) ||> getReachablePoints size get |> Seq.map (fmap2 (racePosition, id))

    let inline countCheets pos =
        let i = racePosition pos
        let cheatWin = uncurry (-) >> flip (-) i

        pos
        |> getAvailableCheets cheetLength
        |> Seq.map cheatWin
        |> Seq.filter ((<=) minCheetWin)
        |> Seq.length

    path |> PSeq.sumBy countCheets

let solution1 = solve 2 100
let solution2 = solve 20 100

let data =
    loadSampleDataLines "Puzzle2.txt" |> Array.filter (_.Length >> (<) 0) |> array2D

run [ solution1; solution2 ] data
