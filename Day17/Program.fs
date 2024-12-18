module Day17

open System
open Utils.SampleData
open Utils.String

type Device =
    { A: int64
      B: int64
      C: int64
      Pointer: int
      Input: int array
      Output: int list }
    static member Load(data: string array) =
        let a = data[0] |> split ":" |> Array.item 1 |> _.Trim() |> int64
        let b = data[1] |> split ":" |> Array.item 1 |> _.Trim() |> int64
        let c = data[2] |> split ":" |> Array.item 1 |> _.Trim() |> int64
        let input = data[4] |> split ":" |> Array.item 1 |> split "," |> Array.map (_.Trim() >> int)
        { A = a; B = b; C = c; Input = input; Pointer = 0; Output = [] }

let combo d =
    match d.Input[d.Pointer + 1] with
    | n when n <= 3 -> int64 (d.Input[d.Pointer + 1])
    | 4 -> d.A
    | 5 -> d.B
    | 6 -> d.C
    | _ -> failwith "Invalid combo"

let literal d = d.Input[d.Pointer + 1]

let max32 (v: int64) =
    if v > Int32.MaxValue then
        Int32.MaxValue
    else
        int v

let operation d =
    match d.Input[d.Pointer] with
    | 0 -> { d with A = d.A >>> (combo d |> int); Pointer = d.Pointer + 2 }
    | 1 -> { d with B = d.B ^^^ literal d; Pointer = d.Pointer + 2 }
    | 2 -> { d with B = int64 ((combo d) % 8L &&& 7L); Pointer = d.Pointer + 2 }
    | 3 -> { d with Pointer = if d.A = 0 then d.Pointer + 2 else literal d }
    | 4 -> { d with B = d.B ^^^ d.C; Pointer = d.Pointer + 2 }
    | 5 -> { d with Output = (((combo d) % 8L) |> int32) :: d.Output; Pointer = d.Pointer + 2 }
    | 6 -> { d with B = d.A >>> (combo d |> int); Pointer = d.Pointer + 2 }
    | 7 -> { d with C = d.A >>> (combo d |> int); Pointer = d.Pointer + 2 }
    | _ -> failwith "Invalid operation"

let rec runProgram d =
    if d.Pointer >= d.Input.Length then
        d.Output |> List.rev |> Array.ofList
    else
        runProgram (operation d)

let data = loadSampleDataLines "Puzzle2.txt"
let input = Device.Load(data)

let solution1 = runProgram >> fun x -> String.Join(",", x)
let solution2 input =
    let resultA a = runProgram { input with A = a }
    let rec smallest acc rem =
        match rem with
        | [] -> Some acc
        | target :: xs ->
            let results = Array.init 8 (fun i -> i, resultA (8L * acc + int64 i))
            let valid =
                results
                |> Array.filter (fun (_, r) -> r[0] = target)
                |> Array.map fst

            valid
            |> Array.tryPick (fun i -> smallest (8L * acc + int64 i) xs)

    let targets = input.Input |> List.ofArray |> List.rev

    smallest 0 targets |> string


run [solution1; solution2] input
