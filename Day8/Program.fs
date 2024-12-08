module Day8

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
    // Helper function to generate points in one direction
    let generatePoints (x, y) dx dy =
        Seq.initInfinite (fun i -> (x + i * dx, y + i * dy))
        |> Seq.takeWhile inBounds
    seq {
        yield! generatePoints (x1, y1) dx dy
        yield! generatePoints (x2, y2) (-dx) (-dy)
    }

let nonSelfPermutations len lst =
    permutations len lst
    |> Seq.map fromArray
    |> Seq.filter (fmap (<>))

let solve antinodes data =
    let inBounds = uncurry (inBounds data)
    let result =
        data |> windowFold (fun acc (g, x,y,_,_) ->
            match g[x,y] with
            | '.' -> acc
            | value -> acc |> Map.change value (fun t ->
                match t with
                | Some v -> Some ((x,y) :: v)
                | None -> Some [(x,y)])
        ) Map.empty (1, 1)
        |> Map.values
        |> Seq.collect (fun positions ->
            positions
            |> nonSelfPermutations 2
            |> Seq.collect (uncurry (antinodes inBounds))
        )
        |> Seq.distinct
    result |> Seq.length

let solution1 = solve antinodes
let solution2 = solve infiniteAntinodes

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0) // Rider adds extra empty line at the end of the file
    |> array2D

run [solution1; solution2 ] data
