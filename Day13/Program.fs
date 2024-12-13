module Day13

open Utils.Combinators
open Utils.Pair
open Utils.SampleData
open Utils.String

type Machine =
    { A: int64 * (int64 * int64)
      B: int64 * (int64 * int64)
      Prize: int64 * int64 }

let play m =
    let a, (ax, ay) = m.A
    let b, (bx, by) = m.B
    let px, py = m.Prize
    let determinant = ax * by - bx * ay

    if determinant = 0L then
        // If the determinant is zero, the system has no unique solution
        None
    else
        let sa = (by * px - bx * py) / determinant
        let sb = (-ay * px + ax * py) / determinant

        if (sa * ax + sb * bx, sa * ay + sb * by) <> m.Prize then
            None
        else
            Some(a * sa + b * sb)

let parseLine delim input =
    input
    |> split ":"
    |> Array.item 1
    |> split ","
    |> Array.map (split delim >> Array.item 1 >> int64)
    |> fromArray

let parseMachine (arr: _ array) =
    { A = parseLine "+" (arr[0]) |> branch (K 3L) id
      B = parseLine "+" (arr[1]) |> branch (K 1L) id
      Prize = parseLine "=" (arr[2]) }

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.chunkBySize 4
    |> Array.map (Array.take 3)
    |> Array.map parseMachine

let solve patch (data: Machine array) =
    data
    |> Array.map (patch >> play)
    |> Array.sumBy (Option.defaultValue 0L)

let solution1 = solve id

let solution2 =
    solve (fun x -> { x with Prize = (10000000000000L + fst x.Prize, 10000000000000L + snd x.Prize) })

run [ solution1; solution2 ] data
