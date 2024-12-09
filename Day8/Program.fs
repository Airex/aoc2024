module Day8

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open Utils.Array2D
open Utils.Pair
open Utils.Permutations
open Utils.SampleData


let antinodes inBounds (x1, y1) (x2, y2) =
    let dx, dy = x1 - x2, y1 - y2
    seq {
        let a = (x1 + dx, y1 + dy)
        let b = (x2 - dx, y2 - dy)
        if inBounds a then yield a
        if inBounds b then yield b
    }

let infiniteAntinodes inBounds (x1, y1) (x2, y2) =
    let dx, dy = x1 - x2, y1 - y2
    let generatePoints (x, y) dx dy =
        Seq.initInfinite (fun i -> (x + i * dx, y + i * dy))
        |> Seq.takeWhile inBounds
    seq {
        yield! generatePoints (x1, y1) dx dy
        yield! generatePoints (x2, y2) (-dx) (-dy)
    }

let generateNodePairs lst =
    permutations 2 lst
    |> Seq.map fromArray
    |> Seq.filter (fmap (<>))

let solve antinodes data =
    let inBounds = uncurry (inBounds data)
    let generateAntinodes = uncurry (antinodes inBounds)

    data |> windowFold (fun acc (g, x,y,_,_) ->
        match g[x,y] with
        | '.' -> acc
        | value -> acc |> Map.change value (fun t ->
            match t with
            | Some v -> Some ((x,y) :: v)
            | None -> Some [(x,y)])
    ) Map.empty (1, 1)
    |> Map.values
    |> Seq.collect ( generateNodePairs >> Seq.collect generateAntinodes )
    |> Seq.distinct
    |> Seq.length

let solution1 = solve antinodes
let solution2 = solve infiniteAntinodes

let data =
    loadSampleDataLines "Puzzle1.txt"
    |> Array.filter (_.Length >> (<) 0) // Rider adds extra empty line at the end of the file
    |> array2D

run [solution1; solution2 ] data

// [<SimpleJob(RuntimeMoniker.Net90)>]
// type Benchmarks() =
//
//     member val data = loadSampleDataLines "Puzzle2.txt" |> Array.filter (_.Length >> (<) 0) |> array2D with get, set
//
//     [<Benchmark>]
//     member this.Solution1() = solution1 this.data
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
