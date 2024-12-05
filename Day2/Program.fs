module Day2

open Utils
open Utils.Pair
open Utils.SampleData

let parseLine = String.words >> Array.map int64

let readLines =
    String.splitLines
    >> Array.filter (_.Length >> (<) 0) // Rider adds extra empty line at the end of the file
    >> Array.map parseLine

let safeCondition arr =
    let folder (withinRange, positiveCount, negativeCount) x =
        let inRange = withinRange && abs x >= 1L && abs x <= 3L
        let pos = if x > 0L then positiveCount + 1 else positiveCount
        let neg = if x < 0L then negativeCount + 1 else negativeCount
        (inRange, pos, neg)

    let withinRange, positiveCount, negativeCount =
        Array.fold folder (true, 0, 0) arr

    withinRange && (positiveCount = 0 || negativeCount = 0)

let pairwiseDiff = Array.pairwise >> Array.map (uncurry (-))
let conditionMatcher = pairwiseDiff >> safeCondition

let solution1 data =
    data |> Array.sumBy (conditionMatcher >> zeroOne)

let solution2 data =
    let flippedRemoveAt = flip Array.removeAt

    let withOneErrorConditionMatcher x =
        x
        |> Array.indexed
        |> Array.exists ( fst >> flippedRemoveAt x >> conditionMatcher)

    data
    |> Array.sumBy (withOneErrorConditionMatcher >> zeroOne)


let data =
    loadSampleData "E:\\Sources\\experiments\\advent of code\\2024\\AdventOfCode2024\\Day2\\Puzzle1.txt"
    |> readLines

run [solution1; solution2] data
