module Day12

open Utils.Array2D
open Utils.Combinators
open Utils.Pair
open Utils.SampleData

let directions = [|(1,0); (-1,0); (0,1); (0,-1); |]
let leftOrRightSide dir = dir = 2 || dir = 3
let unpackBySide side v = v |> List.unzip |> snd |> List.unzip |> (if leftOrRightSide side then fst else snd)
let hasGap (a, b) = abs (a - b) > 1
let gapsCount lst = lst |> Seq.sort |> Seq.pairwise |> Seq.sumBy (hasGap >> zeroOne)

let calculateFencePrice discountFn data =
    let inBounds, get = data |> api
    let (|PlantOfDifferentKind|_|) plant x = if (inBounds x |> not) || get x <> plant then Some x else None

    let iterateDirs pos prev found next =
        directions |> Array.fold (fun acc x -> next (pos |> add x) prev acc) found

    let enumerateSidesNextToOtherPlants pos =
        let plant = get pos
        directions |> Array.indexed |> Array.choose (fun (i, dir) ->
            match (pos |> add dir) with
            | PlantOfDifferentKind plant _ -> Some (i, pos)
            | _ -> None)

    let rec findCluster pos plant (found, visited) =
        match visited |> Set.contains pos, inBounds pos with
        | true, _ | _, false -> (found, visited)
        | _ ->
            match pos |> get = plant with
            | true -> iterateDirs pos plant (found |> Set.add pos, visited |> Set.add pos) findCluster
            | _ -> (found,  visited |> Set.add pos)

    data |> windowFold (fun clusters (_, x, y, _, _) ->
        if  clusters |> List.exists (Set.contains (x, y)) then
            clusters
        else
            let cluster = findCluster (x, y) (get (x, y)) (Set.empty, Set.empty) |> fst
            (cluster :: clusters)
    ) [] (1, 1)
    |> List.sumBy (S (*) Set.count (Seq.collect enumerateSidesNextToOtherPlants >> discountFn))

let sidesDiscount sides =
    let sideOnSameRowOrCol (side, pos) = if leftOrRightSide side then (side, snd pos) else (side, fst pos)
    let numberOfConnectedSidesOnSameRowOrColumn ((side, _), v) = v |> unpackBySide side |> gapsCount |> (+) 1

    sides
    |> List.ofSeq
    |> List.groupBy sideOnSameRowOrCol
    |> List.sumBy numberOfConnectedSidesOnSameRowOrColumn

let noDiscount = Seq.length

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0)
    |> array2D

let solution1 = calculateFencePrice noDiscount
let solution2 = calculateFencePrice sidesDiscount

run [ solution1; solution2 ] data
