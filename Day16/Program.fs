module Day16

open Utils
open Utils.Array2D
open Utils.Combinators
open Utils.Dir
open Utils.Navigation
open Utils.Pair
open Utils.SampleData

let start c = c = 'S'
let finish c = c = 'E'
let wall c = c = '#'

let aStarOnMap (map: char array2d) start goal goalReached =
    let isValid (x, y, _) = not (wall map[x, y])

    let neighbors (x, y, dir) =
        [ Dir.Right; Dir.Left; Dir.Down; Dir.Up ]
        |> List.map (S expand id (dirToPair >> add (x, y)))
        |> List.filter isValid
        |> List.map (fun (x, y, d) -> (x, y, d), if d = dir then 1.0 else 1001.0)

    let noHeuristics _ _ = 0.0
    aStar start goal goalReached neighbors noHeuristics

let solve processResult data =
    let start =
        data |> findIndex start |> Option.defaultValue (-1, -1) |> expand Dir.Right

    let goal =
        data |> findIndex finish |> Option.defaultValue (-1, -1) |> expand Dir.Unknown

    let goalReached (x, y, _) (gx, gy, _) = x = gx && y = gy

    match aStarOnMap data start goal goalReached with
    | Some paths -> paths |> processResult
    | None -> failwith "No paths found"

let solution1 = solve (snd >> int)

let solution2 =
    solve (
        fst
        >> List.collect id
        >> List.map (fun (x, y, _) -> x, y)
        >> List.distinct
        >> List.length
    )

loadSampleDataLines "Puzzle2.txt"
|> Array.filter (_.Length >> (<) 0)
|> array2D
|> run [ solution1; solution2 ]
