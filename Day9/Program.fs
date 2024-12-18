﻿module Day9

open Utils.SampleData

type Disk =
    | File of int * int
    | Empty of int

let format arr =
    arr
    |> Seq.map (function
        | File(fsize, index) -> String.replicate fsize (string index)
        | Empty esize -> String.replicate esize ".")
    |> String.concat ""
    |> dump

let fillGaps input =
    let mutable arr = input
    let mutable left = 0
    let mutable right = Array.length arr - 1

    while right <> left do
        match arr[right] with
        | Empty _ -> right <- right - 1
        | File(fsize, index) ->
            match arr[left] with
            | Empty esize ->
                arr[right] <-
                    if fsize - esize > 0 then
                        File(fsize - esize, index)
                    else
                        Empty 0

                arr.[left] <- File(min esize fsize, index)
                left <- if fsize - esize >= 0 then left + 1 else left
                right <- if fsize - esize > 0 then right else right - 1

                if fsize - esize < 0 then
                    arr <- arr |> Array.insertAt (left + 1) (Empty(esize - fsize)) // Insert the gap
            | _ -> left <- left + 1 // Move left

    arr

let fillGaps2 (input: Disk array) =
    let rec fill left right lst =
        // format lst |> ignore
        match left > right with
        | true -> List.rev lst
        | false ->
            match input[left] with
            | File(fsize, index) -> fill (left + 1) right (File(fsize, index) :: lst)
            | Empty esize ->
                match input[right] with
                | Empty _ -> fill left (right - 1) lst
                | File(fsize, index) ->
                    match fsize <= esize with
                    | true ->
                        input.[left] <- Empty(esize - fsize)
                        fill left (right - 1) (File(fsize, index) :: lst)
                    | false ->
                        input[right] <- File(fsize - esize, index)
                        fill (left + 1) right (File(esize, index) :: lst)


    fill 0 ((Array.length input) - 1) []


let fillGapsClusters input =
    let mutable arr = input
    let mutable left = 0
    let mutable right = Array.length arr - 1

    while right >= 0 do
        match arr[right] with
        | Empty _ -> right <- right - 1 // Skip empty clusters
        | File(fsize, index) -> // Fill gaps in clusters
            match arr[left] with
            | Empty esize when esize >= fsize -> // Found a gap enough to fill with right cluster
                arr.[left] <- File(fsize, index) // Fill the gap
                arr.[right] <- Empty fsize // Remove the cluster

                if (esize - fsize) > 0 then // If there is a gap left
                    arr <- arr |> Array.insertAt (left + 1) (Empty(esize - fsize)) // Insert the gap

                left <- 0 // Reset left
                right <- right - 1 // Move right
            | _ when left + 1 >= right -> // No gap found, move right
                right <- right - 1
                left <- 0
            | _ -> left <- left + 1 // Move left

    arr

let solve gapFiller data =
    data
    |> Array.ofSeq
    |> Array.map (string >> int)
    |> Array.mapi (fun i c -> if i % 2 = 0 then File(c, (i / 2)) else Empty c)
    |> gapFiller
    |> Seq.collect (fun d ->
        match d with
        | File(fsize, index) -> Seq.init fsize (fun _ -> index)
        | Empty esize -> Seq.init esize (fun _ -> 0))
    |> Seq.indexed
    |> Seq.sumBy (fun (i, c) -> int64 i * int64 c)

let solution1 = solve fillGaps
let solution1_2 = solve fillGaps2
let solution2 = solve fillGapsClusters

let data = loadSampleData "Puzzle1.txt" |> _.Trim()


run [ solution1; solution1_2; solution2 ] data


// [<SimpleJob(RuntimeMoniker.Net90)>]
// [<MemoryDiagnoser>]
// type Benchmarks() =
//
//     member val data = loadSampleData "Puzzle2.txt" |> _.Trim() with get, set
//
//     [<Benchmark>]
//     member this.Solution1() = solution1 this.data
//
//     [<Benchmark>]
//     member this.Solution1_2() = solution1_2 this.data
//
//     [<Benchmark>]
//     member this.Solution2() = solution2 this.data
//
// let defaultSwitch () = BenchmarkSwitcher [| typeof<Benchmarks>  |]
//
//
// [<EntryPoint>]
// let Main args =
//     defaultSwitch().Run args |> ignore
//     0
