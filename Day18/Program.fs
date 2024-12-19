module Day16

open Utils
open Utils.Dir
open Utils.Navigation
open Utils.Pair
open Utils.SampleData
open Utils.String

let aStarOnMap (size: int * int) (obstacles: Set<int * int>) start goal goalReached =
    let isValid (x, y) =
        size |> Box.fromPair |> Box.contains (x, y)
        && not (obstacles |> Set.contains (x, y))

    let neighbors (x, y) =
        [ Dir.Right; Dir.Left; Dir.Down; Dir.Up ]
        |> List.map (dirToPair >> add (x, y))
        |> List.filter isValid
        |> List.map (fun pos -> pos, 1.0)

    let manhattanDistance (a: int * int) (b: int * int) =
        abs (fst a - fst b) + abs (snd a - snd b) |> float

    aStar start goal goalReached neighbors manhattanDistance

let solve processResult data =
    let start = (0, 0)
    let goal = (70, 70)
    let goalReached = (=)

    match aStarOnMap goal data start goal goalReached with
    | Some paths -> Some(paths |> processResult)
    | None -> None

let solution1 data =
    data |> Array.take 1024 |> Set.ofArray |> solve (snd >> string) |> Option.get

let solution2 data =
    let checkPath bytes =
        match data |> Array.take bytes |> Set.ofArray |> solve id with
        | Some _ -> Right
        | None -> Left

    binarySearch 0 (data |> Array.length) checkPath
    |> flip Array.item data
    |> string


loadSampleDataLines "Puzzle2.txt"
|> Array.filter (_.Length >> (<) 0)
|> Array.map (split "," >> Array.map int >> fromArray)
|> run [ solution1; solution2 ]
