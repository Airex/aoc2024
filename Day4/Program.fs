module Day4

open Utils
open Utils.Array2D
open Utils.SampleData

let readLines =
    String.splitLines
    >> Array.filter (_.Length >> (<) 0)
    >> array2D

let dirs = [|(1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1)|]

let checkMask word (inBounds, get) pos dir =
    let step i c =
        let x, y = pos |> Pair.add (dir |> Pair.scale i)
        match inBounds (x, y) with
        | false -> false
        | true -> get (x, y) = c

    word
    |> Seq.mapi step
    |> Seq.forall id

let countDirections api pos =
    dirs
    |> Array.sumBy ((checkMask "XMAS" api pos) >> zeroOne)

let checkMas api pos =
    [
        checkMask "MAS" api pos (1, 1);
        checkMask "SAM" api pos (1, 1);
        checkMask "MAS" api (pos |> Pair.fmap2 ((+) 2, id)) (-1, 1);
        checkMask "SAM" api (pos |> Pair.fmap2 ((+) 2, id)) (-1, 1)
    ]
    |> List.sumBy zeroOne
    |> (=) 2

let solution1  data =
    let api = data |> api
    data
    |> Array2D.mapi (fun x y _ -> countDirections api (x, y))
    |> sum

let solution2 data =
    let api = data |> api
    data
    |> windowFold (fun s (_, x1, y1, _, _) ->
       (x1 ,y1) |> checkMas api |> zeroOne |> (+) s
    ) 0 (3, 3)

let data =
    loadSampleData "Puzzle2.txt"
    |> readLines

run [solution1; solution2] data
