module Day12

open Utils.Array2D
open Utils.Combinators
open Utils.Pair
open Utils.SampleData

let directions = [|(1,0); (-1,0); (0,1); (0,-1); |]
let unpack v = v |> List.unzip |> snd |> List.unzip
let leftOrRightSide dir = dir = 2 || dir = 3

let calculateFencePrice discountFn data =
    let inBounds, get = data |> api
    let (|PlantOfSameKind|_|) plant x = if (inBounds x |> not) || get x <> plant then Some x else None

    let iterateDirs pos prev found next =
        directions |> Array.fold (fun acc x -> next (pos |> add x) prev acc) found

    let enumerateSides pos =
        let plant = get pos
        directions |> Array.indexed |> Array.choose (fun (i, dir) ->
            match (pos |> add dir) with
            | PlantOfSameKind plant _ -> Some (i, pos)
            | _ -> None)

    let rec findPath pos plant (found, visited) =
        match visited |> Set.contains pos with
        | true -> (found, visited)
        | _ ->
            match inBounds pos with
            | false -> (found, visited)
            | true ->
                match pos |> get = plant with
                | true -> iterateDirs pos plant (found |> Set.add pos, visited |> Set.add pos) findPath
                | _ -> (found,  visited |> Set.add pos)

    data |> windowFold (fun clusters (_, x, y, _, _) ->
        if  clusters |> List.exists (Set.contains (x, y)) then
            clusters
        else
            let cluster, _ = findPath (x, y) (get (x, y)) (Set.empty, Set.empty)
            ( cluster :: clusters)
    ) [] (1, 1)
    |> List.sumBy (S (*) Set.count (Seq.collect enumerateSides >> discountFn))

let sidesDiscount sides =
    let countGaps f lst =
        let sorted = lst |> unpack |> f |> List.sort
        let rec countGapsHelper acc = function
            | [] | [_] -> acc
            | x::y::xs -> countGapsHelper (acc + (abs(y-x) > 1 |> zeroOne )) (y::xs)
        (countGapsHelper 0 sorted) + 1

    let sidesOnSameRowOrCol (side, pos) = if leftOrRightSide side then (side, snd pos) else (side, fst pos)
    let numberOfGapsOnSameRowOrCol ((side, _), v) = countGaps (if leftOrRightSide side then fst else snd) v

    sides
    |> List.ofSeq
    |> List.groupBy sidesOnSameRowOrCol
    |> List.sumBy numberOfGapsOnSameRowOrCol

let noDiscount = Seq.length

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0)
    |> array2D

let solution1 = calculateFencePrice noDiscount
let solution2 = calculateFencePrice sidesDiscount

run [ solution1; solution2 ] data
