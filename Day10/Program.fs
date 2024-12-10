module Day10

open Utils.Array2D
open Utils.Pair
open Utils.SampleData

let directions = [|(1,0); (-1,0); (0,1); (0,-1); |]

let hike (data: _ array2d) x y =
    let inBounds, get = data |> api
    let iterateDirs pos prev found next =
        directions |> Array.fold (fun acc x -> next (pos |> add x) prev acc) found

    let rec findPath pos prev found =
        match inBounds pos with
        | false -> found
        | true ->
            match  pos |> get |> string |> int with
            | 9 when 9 - prev = 1 -> found |> Map.change pos (function | Some k -> Some (k + 1) | None -> Some 1)
            | t when t - prev = 1 -> iterateDirs pos t found findPath
            | _ -> found

    findPath (x, y) -1 Map.empty

let solve operation data =
    data |> windowFold (fun acc (g, x, y, _, _) ->
      acc + (hike g x y |> operation)
    ) 0 (1, 1)

let solution1 = solve (Map.values >> Seq.length)
let solution2 = solve (Map.values >> Seq.sum)

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0)
    |> array2D

run [solution1; solution2] data
