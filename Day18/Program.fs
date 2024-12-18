module Day16

open Utils
open Utils.Dir
open Utils.Navigation
open Utils.Pair
open Utils.SampleData
open Utils.String

let aStarOnMap (size: int*int) (obstacles: Set<int*int>) start goal goalReached =
    let isValid (x, y) = x >= 0 && x <= fst size && y >= 0 && y <= snd size && not (obstacles |> Set.contains (x, y))

    let neighbors (x, y) =
        [ Dir.Right; Dir.Left; Dir.Down; Dir.Up ]
        |> List.map (dirToPair >> add (x, y))
        |> List.filter isValid
        |> List.map (fun (x,y) -> (x,y), 1.0)

    let manhattanDistance (a: int * int) (b: int * int) =
        abs (fst a - fst b) + abs (snd a - snd b) |> float

    aStar start goal goalReached neighbors manhattanDistance

let solve processResult data =
    let start = (0,0)
    let goal = (70,70)
    let goalReached = (=)

    match aStarOnMap goal data start goal goalReached with
    | Some paths -> Some (paths |> processResult)
    | None -> None

let solution1 data =
    let obstacles =
        data
        |> Array.take 1024
        |> Set.ofArray
    solve (snd >> string) obstacles |> Option.get


let solution2 data =
    let rec f bytes =
        let obstacles =
            data
            |> Array.take bytes
            |> Set.ofArray
        let result = solve (fst >> string) obstacles
        match result with
        | Some _ -> data |> Array.take (bytes + 1) |> Array.last |> string
        | None -> f (bytes - 1)
    f (data |> Array.length)


loadSampleDataLines "Puzzle2.txt"
|> Array.filter (_.Length >> (<) 0)
|> Array.map (split "," >> Array.map int >> fromArray)
|> run [ solution1; solution2 ]
