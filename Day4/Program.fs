module Day4

open Utils
open Utils.SampleData

let readLines =
    String.splitLines
    >> Array.filter (_.Length >> (<) 0)
    >> array2D

let dirs = [|(1,0); (1,1); (0,1); (-1,1); (-1,0); (-1,-1); (0,-1); (1,-1)|]

let checkMask word (field: char array2d) pos dir =
    let step i c =
        let x, y = ((fst pos) + ((fst dir) * i), (snd pos) + ((snd dir) * i))
        match Array2D.inBounds field x y with
        | false -> false
        | true -> field.[x, y] = c

    word
    |> Seq.mapi step
    |> Seq.forall id

let countDirections field pos =
    dirs
    |> Array.sumBy ((checkMask "XMAS" field pos) >> zeroOne)

let checkMas field pos =
    [
        checkMask "MAS" field pos (1, 1);
        checkMask "SAM" field pos (1, 1);
        checkMask "MAS" field (pos |> Pair.fmap2 ((+) 2, id) (-1, 1);
        checkMask "SAM" field (fst pos + 2, snd pos) (-1, 1)
    ]
    |> List.sumBy zeroOne

let solution1  data =
    data
    |> Array2D.mapi (fun x y _ -> countDirections data (x, y))
    |> Array2D.sum

let solution2 data =
    data
    |> Array2D.windowFold (fun (s, (arr, x1, y1, _, _)) ->
        let count = checkMas arr (x1, y1)
        s + (count = 2 |> zeroOne)
    ) 0 (2, 2)

let data =
    loadSampleData "E:\\Sources\\experiments\\advent of code\\2024\\AdventOfCode2024\\Day4\\Puzzle2.txt"
    |> readLines

let stopwatch = System.Diagnostics.Stopwatch.StartNew()
let result1 = solution1 data
stopwatch.Stop()
printfn $"Result 1: {result1}, Elapsed: {stopwatch.ElapsedTicks}ticks"

stopwatch.Restart()
let result2 = solution2 data
stopwatch.Stop()
printfn $"Result 2: {result2}, Elapsed: {stopwatch.ElapsedTicks}ticks"
