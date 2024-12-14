module Day14

open Utils.Pair
open Utils.SampleData
open Utils.String

let parsePair = split "=" >> Array.last >> split "," >> fromArray >> map int
let step x m n x0 = ((x0 + x*n) % m + m) % m

type Robot = {
    Pos: int*int
    Dir: int*int
}
with
    static member Load line =
        line
        |> split " "
        |> fromArray
        |> fmap2 (makePair parsePair)
        |> fun (pos, dir) -> { Pos = pos; Dir = dir }

    member this.DoSteps steps field =
        (makePair step) <*> this.Dir <*> field <*> (makePair steps) <*> this.Pos

let calculateSteps steps field data =
    data
    |> Array.map (fun (r: Robot) -> r.DoSteps steps field)

let solution1 data =
    let field = (101, 103)
    let steps = 100
    let quadrant (x, y) = (x * 2 / ((fst field) + 1), y * 2 / ((snd field) + 1))
    let notMidleLines (x, y) = x <> (fst field) / 2 && y <> (snd field) / 2

    data
    |> calculateSteps steps field
    |> Array.filter notMidleLines
    |> Array.groupBy quadrant
    |> Array.map (snd >> Array.length)
    |> Array.fold (*) 1

let solution2 data =
    let field = (101, 103)
    let shift = 10
    let middleSquare (x, y) = abs (x - (fst field) / 2) < shift && abs (y - (snd field) / 2) < shift

    let rec f step =
        match data  |> calculateSteps step field |> Array.filter middleSquare  |> Array.length with
        | x when x >= shift * shift -> step
        | _ -> f (step + 1)
    f 0

let solution2_2 data =
    let field = (101, 103)
    let robotsCount = data |> Array.length

    let rec f step =
        match data |> calculateSteps step field |> Array.distinct |> Array.length with
        | x when x = robotsCount -> step
        | _ -> f (step + 1)
    f 0

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0)
    |> Array.map Robot.Load


run [solution1; solution2; solution2_2] data
