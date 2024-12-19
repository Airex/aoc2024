module Day19

open Utils.Pair
open Utils.SampleData
open Utils.String

let countDesignOptions cache (patterns: string list) (design: string) =
    let rec f cache design =
        match design with
        | "" -> (1L, cache)
        | _ ->
            match cache |> Map.tryFind design with
            | Some x -> (x, cache)
            | None ->
                let result =
                    patterns
                    |> Seq.filter design.StartsWith
                    |> Seq.fold
                        (fun (cnt, cache) p ->
                            let v, c = f cache (design.Substring(p.Length))
                            (cnt + v, c |> Map.add design (cnt + v)))
                        (0L, cache)

                result

    f cache design

let solve data =
    let towels, designs = data
    let cache = Map.empty

    designs
    |> Seq.fold
        (fun (lst, cache) x ->
            let v, c = countDesignOptions cache towels x
            (v :: lst, c))
        ([], cache)
    |> fst

let parsePatterns = Array.head >> split "," >> List.ofArray >> List.map _.Trim()
let parseDesigns = Array.skip 1

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0)
    |> branch parsePatterns parseDesigns

let solution1 = solve >> List.filter ((<) 0) >> List.length >> int64
let solution2 = solve >> List.sum

run [ solution1; solution2 ] data
