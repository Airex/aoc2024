module Day1

open FSharpPlus
open Utils.Combinators
open Utils.SampleData
open Utils

let parseLine =
    String.words
    >> Array.map int64
    >> Pair.fromArray

let readLines =
    String.splitLines
    >> Array.filter (_.Length >> (<) 0)
    >> Array.map parseLine
    >> Array.unzip


let solution1 data =
    data
    |> Pair.map Array.sort
    ||> Array.zip
    |> Array.sumBy ((<||) (-) >> abs)


let solution2 data=
    let inline findOrZero y x =
        y
        |> Map.tryFind x
        |> Option.defaultValue 0
        |> int64

    let inline frequencyMul m = S (*) id (findOrZero m)

    data
    |> Pair.fmap2 (id, Array.countBy id >> Map >> frequencyMul)
    ||> flip Array.sumBy

let data =
    loadSampleData "E:\\Sources\\experiments\\advent of code\\2024\\AdventOfCode2024\\Day1\\Puzzle1.txt"
    |> readLines

let stopwatch = System.Diagnostics.Stopwatch.StartNew()
let result1 = solution1 data
stopwatch.Stop()

printfn $"Result 1: {result1}, Elapsed: {stopwatch.ElapsedMilliseconds}ms"

stopwatch.Restart()
let result2 = solution2 data
stopwatch.Stop()
printfn $"Result 2: {result2}, Elapsed: {stopwatch.ElapsedMilliseconds}ms"
