module Day6

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open Utils
open Utils.Array2D
open Utils.Pair
open Utils.SampleData

type Dir = Up | Down | Left | Right

let charToDir =
    function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> raise (System.Exception("Invalid direction"))

let rotateRight =
    function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let dirToPair =
    function
    | Up -> (-1, 0)
    | Down -> (1, 0)
    | Left -> (0, -1)
    | Right -> (0, 1)

let opposite = scale -1

let inline step dir pos = pos |> add (dir |> dirToPair)
let inline stepBack dir pos = pos |> add (dir |> dirToPair |> opposite)

let inline isGuard c =
    c = '^' || c = 'v' || c = '<' || c = '>'

let collectDirs (x, y) acc (p, t) =
        match (x, y) = p with
        | true ->
            match acc with
            | Some x when (x = char "|" || x = char " ") && (t = Up || t = Down) -> Some '|'
            | Some x when (x = char "-" || x = char " ") && (t = Left || t = Right) -> Some '-'
            | Some x when x = char "|" && (t = Left || t = Right) -> Some '+'
            | Some x when x = char "-" && (t = Up || t = Down) -> Some '+'
            | Some x -> Some x
            | None -> if t = Up || t = Down then Some '|' else Some '-'
        | _ -> acc

let pathPrinter path get (_: _ array2d) (x, y) =
        match (path |> Set.fold (collectDirs (x,y)) None) with
        | Some x -> x
        | _ -> get (x, y)


[<TailCall>]
let rec stepper (inBounds, get) pos dir visited cyclesCount =
    match visited |> Set.contains (pos, dir) with
    | true -> true, visited, cyclesCount
    | false ->
        match pos |> inBounds with
        | false -> false, visited, cyclesCount
        | true ->
            match pos |> get with
            | c when c = '.' || isGuard c ->
                let cycled =
                    match cyclesCount >= 0 && visited |> Set.exists (fst >> (=) pos) |> not with
                    | true ->
                        let getWithFakeObstacle p = if p = pos then 'O' else get p
                        let cycled, _, _ = stepper (inBounds, getWithFakeObstacle) (pos |> stepBack dir ) (dir |> rotateRight) visited -1
                        cycled
                    | _ -> false
                stepper (inBounds, get) (pos |> step dir) dir (visited |> Set.add (pos, dir)) (cyclesCount + (cycled |> zeroOne))
            | _ -> stepper (inBounds, get) (pos |> stepBack dir ) (dir |> rotateRight) visited cyclesCount

let solution1 data =
    let start = data |> findIndex isGuard |> Option.defaultValue (-1, -1)
    let dir = charToDir ((uncurry (Array2D.get data)) start)
    let inBounds = uncurry (Array2D.inBounds data)
    let get = uncurry (Array2D.get data)

    let _, path, _ = stepper (inBounds, get) start dir Set.empty -1
    let p1 = path |> Set.map fst |> Set.count
    p1

let solution2 data =
    let inBounds = uncurry (Array2D.inBounds data)
    let get = uncurry (Array2D.get data)
    let start = data |> findIndex isGuard |> Option.defaultValue (-1, -1)
    let dir = start |> get |> charToDir
    let _, _, cycled = stepper (inBounds, get) start dir Set.empty 0
    cycled


let data =
    loadSampleDataLines "E:\\Sources\\experiments\\advent of code\\2024\\AdventOfCode2024\\Day6\\Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0) // Rider adds extra empty line at the end of the file
    |> array2D


run [ solution1; solution2 ] data
