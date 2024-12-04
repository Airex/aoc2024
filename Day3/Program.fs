module Day3

open System.Text.RegularExpressions

open Utils
open Utils.SampleData

let parseLine = id

let readLines =
    String.splitLines
    >> Array.filter (_.Length >> (<) 0) // Rider adds extra empty line at the end of the file
    >> Array.map parseLine

let extractOps = String.matches "(do|don't|mul)(?:\\(\\)|\\((\\d+),(\\d+)\\))"

let (|DO|DONT|MUL|) (value: Match)=
    match value.Groups[1].Value with
    | "do" -> DO
    | "don't" -> DONT
    | "mul" when value.Groups.Count > 2 -> MUL (int64 value.Groups[2].Value, int64 value.Groups[3].Value)
    | _ -> raise <| System.Exception("Invalid operation")

let solve doDontActive =
    let folder (enabled, value) = function
    | DO -> (true, value)
    | DONT -> (false, value)
    | MUL (x, y) when not doDontActive || enabled -> (enabled, x*y + value)
    | _ -> (enabled, value)

    extractOps >> Seq.fold folder (true, 0L) >> snd

let solution1 = solve false
let solution2 = solve true

let data =
    loadSampleData "E:\\Sources\\experiments\\advent of code\\2024\\AdventOfCode2024\\Day3\\Puzzle1.txt"


let stopwatch = System.Diagnostics.Stopwatch.StartNew()
let result1 = solution1 data
stopwatch.Stop()

printfn $"Result 1: {result1}, Elapsed: {stopwatch.ElapsedTicks}ticks"

stopwatch.Restart()
let result2 = solution2 data
stopwatch.Stop()
printfn $"Result 2: {result2}, Elapsed: {stopwatch.ElapsedTicks}ticks"
