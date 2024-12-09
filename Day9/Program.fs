module Day9

open Utils.SampleData

type Disk = | File of int*int | Empty of int

let format arr =
    arr
    |> Array.map (function | File (fsize, index) -> String.replicate fsize (string index) | Empty esize -> String.replicate esize ".")
    |> String.concat ""
    |> dump

let fillGaps arr =
    let mutable left = 0
    let mutable right = Array.length arr - 1

    while left <> right do
        match arr[left] with
        | Some _ -> left <- left + 1
        | None ->
            match arr[right] with
            | None -> right <- right - 1
            | Some r ->
                arr.[left] <- Some r
                arr.[right] <- None
                left <- left + 1
                right <- right - 1
    arr

let solution1 data =
    data
    |> Array.ofSeq
    |> Array.map (string >>int)
    |> Array.indexed
    |> Array.collect (fun (i, c) -> Array.init c (fun _ -> if i%2 = 0 then Some (i/2) else None ))
    |> fillGaps
    |> Seq.choose id
    |> Seq.indexed
    |> Seq.sumBy (fun (i, c) -> int64 i * int64 c)

let fillGapsClusters input =
    let mutable arr = input
    let mutable left = 0
    let mutable right = Array.length arr - 1

    while right >= 0 do
        match arr[right] with
        | Empty _ -> right <- right - 1 // Skip empty clusters
        | File (fsize, index) -> // Fill gaps in clusters
            match arr[left] with
            | Empty esize when esize >= fsize -> // Found a gap enough to fill with right cluster
                arr.[left] <- File (fsize, index) // Fill the gap
                arr.[right] <- Empty fsize // Remove the cluster
                if (esize-fsize) > 0 then // If there is a gap left
                    arr <- arr  |> Array.insertAt (left + 1) (Empty (esize-fsize)) // Insert the gap
                left <- 0 // Reset left
                right <- right - 1 // Move right
            | _ when left + 1 >= right  -> // No gap found, move right
                right <- right - 1
                left <- 0
            | _ -> left <- left + 1 // Move left
    arr

let solution2 data =
    data
    |> Array.ofSeq
    |> Array.map (string >> int)
    |> Array.mapi (fun i c ->  if i % 2 = 0 then File (c, (i/2)) else Empty c )
    |> fillGapsClusters
    |> Seq.collect (fun d ->
        match d with
        | File (fsize, index) -> Seq.init fsize (fun _ -> index)
        | Empty esize -> Seq.init esize (fun _ -> 0))
    |> Seq.indexed
    |> Seq.sumBy (fun (i, c) -> int64 i * int64 c)

let data =
    loadSampleData "Puzzle2.txt"
    |> _.Trim()


run [solution1; solution2; ] data
