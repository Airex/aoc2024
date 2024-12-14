module Day7

open Utils.Combinators
open Utils.SampleData
open Utils.String

let checkops ops (target, parts) =
    let rec tryOperators' parts acc =
        match parts with
        | _ when acc > target -> false
        | [] -> target = acc
        | x :: xs -> ops |> List.exists ( (|>) acc >> (|>) x >> tryOperators' xs)

    match parts with
    | [] -> false
    | x :: xs -> tryOperators' xs x

let basicOps = [ (+); (*); ]

let solution1 data =
    data |> Array.sumBy ( S (*) fst (checkops basicOps >> zeroOne >> int64) )

let solution2 data =
    let (<||>) x y =
        let yDigits = if y = 0L then 1 else int (log10 (float y))  + 1
        x * int64 (pown 10L yDigits) + y
    data |> Array.sumBy ( S (*) fst (checkops ((<||>) :: basicOps) >> zeroOne >> int64) )

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0) // Rider adds extra empty line at the end of the file
    |> Array.map (split ":" >> Array.splitAt 1 >> fmap2 (Array.head >> int64, Array.head >> split " " >> Array.map int64 >> List.ofArray))

run [solution1; solution2 ] data
