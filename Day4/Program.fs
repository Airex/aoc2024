module Day4

open Utils
open Utils.SampleData

let readLines =
    String.splitLines
    >> Array.filter (_.Length >> (<) 0)
    >> array2D

let dirs = [|(1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1)|]

let checkMask word (grid: char array2d) pos dir =
    let step i c =
        let x, y = pos |> Pair.add (dir |> Pair.scale i)
        match Array2D.inBounds grid x y with
        | false -> false
        | true -> grid.[x, y] = c

    word
    |> Seq.mapi step
    |> Seq.forall id

let countDirections grid pos =
    dirs
    |> Array.sumBy ((checkMask "XMAS" grid pos) >> zeroOne)

let checkMas grid pos =
    [
        checkMask "MAS" grid pos (1, 1);
        checkMask "SAM" grid pos (1, 1);
        checkMask "MAS" grid (pos |> Pair.fmap2 ((+) 2, id)) (-1, 1);
        checkMask "SAM" grid (pos |> Pair.fmap2 ((+) 2, id)) (-1, 1)
    ]
    |> List.sumBy zeroOne
    |> (=) 2

let solution1  data =
    data
    |> Array2D.mapi (fun x y _ -> countDirections data (x, y))
    |> Array2D.sum

let solution2 data =
    data
    |> Array2D.windowFold (fun s (grid, x1, y1, _, _) ->
       (x1 ,y1) |> checkMas grid |> zeroOne |> (+) s
    ) 0 (3, 3)

let data =
    loadSampleData "E:\\Sources\\experiments\\advent of code\\2024\\AdventOfCode2024\\Day4\\Puzzle2.txt"
    |> readLines

run [solution1; solution2] data
