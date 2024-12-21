module Day21

open System
open System.Collections.Generic
open Utils.Array2D
open Utils.SampleData
open Utils.String

type Pad(value: char array2d) =
    let a = value |> findIndex ((=) 'A') |> Option.get
    let gap = value |> findIndex ((=) ' ') |> Option.get

    member this.A = a
    member this.Gap = gap

    member this.Button c =
        value |> findIndex ((=) c) |> Option.get

    member this.OptimalPaths (x0, y0) (x1, y1) =
        let dx = x1 - x0
        let dy = y1 - y0
        let ud = (if dx > 0 then "v" else "^") |> String.replicate (abs dx)
        let lr = (if dy > 0 then ">" else "<") |> String.replicate (abs dy)
        ud + lr + "A", lr + ud + "A"

    member this.CornersOnOptimalPaths start finish =
        (fst finish, snd start), (fst start, snd finish)

let max64 = Int64.MaxValue
let numpad = "789\n456\n123\n 0A" |> splitLines |> array2D
let dirpad = " ^A\n<v>" |> splitLines |> array2D

let numPad = Pad(numpad)
let dirPad = Pad(dirpad)

let solve layersCount input =
    let pads = [| yield numPad; for _ in 1..layersCount -> dirPad |]

    let cache = Array.init pads.Length Dictionary<String, int64>

    let iterate code layer find =
        let pad = pads[layer]

        let rec fn (code: char list) start len =
            match code with
            | [] -> len
            | c :: rest ->
                let finish = pad.Button c
                let s1, s2 = pad.OptimalPaths start finish
                // get corners to check if we passed trough the gap
                let c1, c2 = pad.CornersOnOptimalPaths start finish

                let best =
                    min
                        (if c1 <> pad.Gap then find s1 (layer + 1) else max64)
                        (if c2 <> pad.Gap then find s2 (layer + 1) else max64)

                fn rest finish (len + best)

        fn (code |> List.ofSeq) pad.A 0L

    let rec find code layer =
        if (layer = (layersCount + 1)) then
            code |> String.length |> int64
        else
            match cache[layer].TryGetValue(code) with
            | true, v -> v
            | _ ->
                let len = iterate code layer find
                cache[layer][code] <- len
                len

    let codeAsNumber (code: string) = code.TrimEnd('A') |> int64
    input |> List.sumBy (fun code -> find code 0 * codeAsNumber code)

let input =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (fun x -> x.Length > 0)
    |> List.ofArray

let solution1 = solve 2
let solution2 = solve 25

run [ solution1; solution2 ] input
