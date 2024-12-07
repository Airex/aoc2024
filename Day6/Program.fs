module Day6

open FSharp.HashCollections
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open Utils
open Utils.Array2D
open Utils.Dir
open Utils.Pair
open Utils.SampleData

let inline step dir pos = pos |> add (dir |> dirToPair)
let inline stepBack dir pos = pos |> add (dir |> dirToPair |> opposite)
let inline isGuard c = c = '^' || c = 'v' || c = '<' || c = '>'

[<TailCall>]
let rec stepper (inBounds, get) pos dir visited cyclesCount =
    match visited |> HashSet.contains (pos, dir) with
    | true -> true, visited, cyclesCount // we found cycle, return
    | false ->
        match pos |> inBounds with
        | false -> false, visited, cyclesCount // we are out of bounds, return
        | true ->
            match pos |> get with
            | c when c = '.' || isGuard c -> // we are at empty cell
                let cycled =
                    let inline canDoCycle pos  = [Up; Down; Left; Right] |> List.exists (fun x -> visited |> HashSet.contains (pos, x)) |> not
                    match cyclesCount >= 0 && canDoCycle pos with
                    | true -> // we are allowed to calculate alternative path for the cycle
                        let getWithFakeObstacle p = if p = pos then 'O' else get p
                        let cycled, _, _ = stepper (inBounds, getWithFakeObstacle) (pos |> stepBack dir ) (dir |> rotateRight) visited -1
                        cycled
                    | _ -> false // we are not allowed to calculate alternative path for the cycle
                stepper (inBounds, get) (pos |> step dir) dir (visited |> HashSet.add (pos, dir)) (cyclesCount + (cycled |> zeroOne))
            | _ -> stepper (inBounds, get) (pos |> stepBack dir ) (dir |> rotateRight) visited cyclesCount

let solution1 data =
    let inBounds, get = data |> api
    let start = data |> findIndex isGuard |> Option.defaultValue (-1, -1)
    let dir = charToDir ((uncurry (Array2D.get data)) start)

    let _, path, _ = stepper (inBounds, get) start dir HashSet.empty -1
    let p1 = path |> HashSet.fold (fun acc x -> x |> fst |> flip HashSet.add acc) HashSet.empty |> HashSet.count
    p1

let solution2 data =
    let inBounds, get = data |> api
    let start = data |> findIndex isGuard |> Option.defaultValue (-1, -1)
    let dir = start |> get |> charToDir
    let _, _, cycled = stepper (inBounds, get) start dir HashSet.empty 0
    cycled


let data =
    loadSampleDataLines "Puzzle1.txt"
    |> Array.filter (_.Length >> (<) 0) // Rider adds extra empty line at the end of the file
    |> array2D


run [ solution1; solution2 ] data
