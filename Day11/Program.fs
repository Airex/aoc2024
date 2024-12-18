open System.Collections.Generic
open Utils.SampleData
open Utils.String

let splitNumber (n: int64) =
    let digits = string n
    let length = digits.Length
    let mid = length / 2
    let left = digits.Substring(0, mid)
    let right = digits.Substring(mid)
    (int64 left, int64 right)

let countStonesAfterBlinks blinks initialStones =
    let stoneCounts = Dictionary<int64 * int, int64>()

    let rec calculateStoneCount (stone: int64) (remaining: int) =
        match remaining with
        | 0 -> 1L
        | _ ->
            let key = (stone, remaining)

            match stoneCounts.TryGetValue key with
            | true, count -> count
            | _ ->
                let count =
                    match stone with
                    | 0L -> calculateStoneCount 1L (remaining - 1)
                    | _ when (stone |> string |> _.Length) % 2 = 0 ->
                        let left, right = splitNumber stone

                        calculateStoneCount left (remaining - 1)
                        + calculateStoneCount right (remaining - 1)
                    | _ -> calculateStoneCount (stone * 2024L) (remaining - 1)

                stoneCounts.[key] <- count
                count

    initialStones |> List.sumBy (flip calculateStoneCount blinks)

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Seq.head
    |> split " "
    |> List.ofArray
    |> List.map int64

let solution1 = countStonesAfterBlinks 25
let solution2 = countStonesAfterBlinks 75

run [ solution1; solution2 ] data
