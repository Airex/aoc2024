module Day16

open Utils
open Utils.Array2D
open Utils.Dir
open Utils.Navigation
open Utils.Pair
open Utils.SampleData

let start c = c = 'S'
let finish c = c = 'E'
let wall c = c = '#'
let empty c = c = '.'

let (|Wall|Empty|Finish|)  = function
    | c when wall c -> Wall
    | c when empty c -> Empty
    | c when start c -> Empty
    | c when finish c -> Finish
    | _ -> failwith "Invalid character"

let aStarOnMap (map: char array2d) start goal goalReached =
    let isValid (x, y, _) = map.[x, y] <> '#'

    let neighbors (x, y, dir) =
        [ Dir.Right; Dir.Left; Dir.Down; Dir.Up ]
        |> List.map (fun d ->
            let pos = d |> dirToPair |> add (x, y)
            fst pos, snd pos, d
        ) // Uniform cost of 1.0
        |> List.filter isValid
        |> List.map (fun (x,y,d) -> (x,y,d), if d = dir then 1.0 else 1001.0) // Uniform cost of 1.0

    let noHeuristics _ _ = 0.0
    aStar start goal goalReached neighbors noHeuristics

let solve processResult data =
    let start = data |> findIndex start |> Option.defaultValue (-1, -1)
    let finish = data |> findIndex finish |> Option.defaultValue (-1, -1)

    let startWithDir = (fst start, snd start, Dir.Right)
    let goal = (fst finish, snd finish, Dir.Unknown)
    let goalReached (x, y, _) (gx, gy, _) = x = gx && y = gy

    match aStarOnMap data startWithDir goal goalReached with
    | Some paths -> paths |> processResult
    | None -> failwith "No paths found"

let solution1 =  solve (snd >> int)
let solution2  = solve (fst >> List.collect id >> List.map (fun (x, y, _) -> x, y) >> List.distinct >> List.length)

loadSampleDataLines "Puzzle2.txt"
|> Array.filter (_.Length >> (<) 0)
|> array2D
|> run [ solution1; solution2 ]
