module Day1

open FSharpPlus
open Utils.Combinators
open Utils.SampleData
open Utils

let parseLine = String.words >> Array.map int64 >> Pair.fromArray

let readLines =
    String.splitLines
    >> Array.filter (_.Length >> (<) 0)
    >> Array.map parseLine
    >> Array.unzip


let solution1 data =
    data |> Pair.map Array.sort ||> Array.zip |> Array.sumBy ((<||) (-) >> abs)


let solution2 data =
    let inline findOrZero y x =
        y |> Map.tryFind x |> Option.defaultValue 0 |> int64

    let inline frequencyMul m = S (*) id (findOrZero m)

    data
    |> Pair.fmap2 (id, Array.countBy id >> Map >> frequencyMul)
    ||> flip Array.sumBy

let data = loadSampleData "Puzzle1.txt" |> readLines

run [ solution1; solution2 ] data
