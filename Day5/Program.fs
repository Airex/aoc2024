module Day5

open Utils.Pair
open Utils.SampleData
open Utils.String


let sorter rules a b =
    rules |> List.contains (b, a) |> boolToint 1 -1

let solution1 data =
    let maps = data |> fst |> List.map (split "|" >> fromArray >> swap)

    let cont = flip List.exists maps

    data
    |> snd
    |> List.map (split ",")
    |> List.filter (fun v ->
        v
        |> Array.fold
            (fun acc x ->
                let lst = acc |> fst

                let valid = lst |> List.exists (fun y -> cont ((=) (y, x))) |> not

                (x :: lst, snd acc && valid))
            (List.empty<string>, true)
        |> snd)
    |> List.sumBy ((fun x -> Array.get x (Array.length x / 2)) >> int)

let solution1_v2 data =

    let maps = data |> fst |> List.map (split "|" >> fromArray)

    data
    |> snd
    |> List.map (split ",")
    |> List.filter (fun v ->
        let idx = v |> Array.mapi (fun i x -> x, i) |> Map

        maps
        |> List.exists (fun (a, b) ->
            match Map.tryFind a idx, Map.tryFind b idx with
            | Some x, Some y -> x > y
            | _ -> false)
        |> not)
    |> List.sumBy ((fun x -> Array.get x (Array.length x / 2)) >> int)


let solution2 data =
    let maps = data |> fst |> List.map (split "|" >> fromArray)

    data
    |> snd
    |> List.map (split ",")
    |> List.map (fun v ->
        let mutable idx = v |> Array.mapi (fun i x -> x, i) |> Map
        let mutable changed = true
        let mutable changedOverall = false

        while changed do
            changed <-
                List.fold
                    (fun t (a, b) ->
                        match Map.tryFind a idx, Map.tryFind b idx with
                        | Some x, Some y when x > y ->
                            v.[x] <- b
                            v.[y] <- a

                            idx <- idx |> Map.change a (fun _ -> Some y) |> Map.change b (fun _ -> Some x)

                            t || true
                        | _ -> t || false)
                    false
                    maps

            changedOverall <- changedOverall || changed

        (changedOverall, v))

    |> List.filter fst
    |> List.sumBy (snd >> (fun x -> Array.get x (Array.length x / 2)) >> int)

let solution2_v2 data =
    // Parse the rules into pairs
    let maps = data |> fst |> List.map (split "|" >> fromArray)

    // Function to apply a single pass of swaps based on the rules
    let applySwaps rules idx v =
        rules
        |> List.fold
            (fun (updatedIdx, updatedV, hasChanged) (a, b) ->
                match Map.tryFind a updatedIdx, Map.tryFind b updatedIdx with
                | Some x, Some y when x > y ->
                    // Swap elements in the array and update indices
                    let newV = updatedV |> Array.copy
                    newV.[x] <- b
                    newV.[y] <- a

                    let newIdx =
                        updatedIdx |> Map.change a (fun _ -> Some y) |> Map.change b (fun _ -> Some x)

                    (newIdx, newV, true) // Indicate a change occurred
                | _ -> (updatedIdx, updatedV, hasChanged) // No change
            )
            (idx, v, false)

    // Recursively reorder the array until no changes occur
    let rec reorder rules idx v =
        let (newIdx, newV, hasChanged) = applySwaps rules idx v

        if hasChanged then reorder rules newIdx newV else newV

    // Process updates and directly calculate the sum of middle page numbers
    data
    |> snd
    |> List.fold
        (fun acc v ->
            let arr = split "," v
            let initialIdx = arr |> Array.mapi (fun i x -> x, i) |> Map
            let reordered = reorder maps initialIdx arr

            if reordered <> arr then
                acc + (reordered.[Array.length reordered / 2] |> int)
            else
                acc)
        0

let solution2_v3 data =
    let maps = data |> fst |> List.map (split "|" >> Array.map int >> fromArray)

    data
    |> snd
    |> List.sumBy (
        split ","
        >> Array.map int
        >> branch id (Array.sortWith (sorter maps))
        >> map2 (curry snd) (<>)
        >> fmap2 ((fun x -> x.[x.Length / 2]), zeroOne)
        >> fmap (*)
    )


let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.fold
        (fun acc v ->
            match v with
            | "" -> swap acc
            | value -> acc |> fmap2 (id, (fun x -> value :: x)))
        (List.empty<string>, List.empty<string>)

run [ solution1; solution1_v2; solution2; solution2_v2; solution2_v3 ] data
