module Day15

open Utils.Array2D
open Utils.Dir
open Utils.Pair
open Utils.SampleData

let wall c = c = '#'
let openSpace c = c = '.'
let box x = x = 'O'
let largeBoxLeft x = x = '['
let largeBoxRight x = x = ']'
let largeBox x = largeBoxLeft x || largeBoxRight x
let robot x = x = '@'

let (|Wall|OpenSpace|Box|LargeBoxLeft|LargeBoxRight|) c =
    match c with
    | c when wall c -> Wall
    | c when openSpace c || robot c -> OpenSpace
    | c when box c -> Box
    | c when largeBoxLeft c -> LargeBoxLeft
    | c when largeBoxRight c -> LargeBoxRight
    | _ -> failwith "Invalid character"

let toMapAndMoves patch data =
    data
    |> fmap2 (
        patch >> array2D,
        Array.map Array.ofSeq
        >> Array.collect id
        >> List.ofArray
    )

let score t map =
    map
    |> windowFold
        (fun acc (_, x, y, _, _) ->
            match map[x, y] with
            | c when t c -> acc + (x * 100 + y)
            | _ -> acc)
        0
        (1, 1)

let specialSort dir arr =
    match dir with
    | 1, 0 -> arr |> List.sortBy (fun (x, _) -> -x)
    | -1, 0 -> arr |> List.sortBy fst
    | 0, 1 -> arr |> List.sortBy (fun (_, y) -> -y)
    | 0, -1 -> arr |> List.sortBy snd
    | _ -> arr

let collectBoxes get pos dir =
    let addBox pos boxes =
        match boxes with
        | Some x -> Some(pos :: x)
        | None -> Some [ pos ]

    let rec f pending boxes visited =
        match pending with
        | [] -> boxes
        | pos :: rest ->
            if visited |> Set.contains pos then
                f rest boxes visited
            else
                match get pos with
                | OpenSpace -> f rest boxes visited
                | Wall -> None
                | Box -> f ((pos |> add dir) :: rest) (addBox pos boxes) visited
                | LargeBoxRight ->
                    f ((pos |> add (0, -1)) :: (pos |> add dir) :: rest) (addBox pos boxes) (visited |> Set.add pos)
                | LargeBoxLeft ->
                    f ((pos |> add (0, 1)) :: (pos |> add dir) :: rest) (addBox pos boxes) (visited |> Set.add pos)

    f [ pos ] None Set.empty

let moveRobot pos moves (map: _ array2d) get =
    let moveBoxes dir boxes =
        boxes
        |> specialSort dir
        |> List.iter (fun (x, y) ->
            let nx, ny = (x, y) |> add dir
            map[nx, ny] <- map[x, y]
            map[x, y] <- '.')

    let rec f pos moves =
        match moves with
        | [] -> map
        | move :: rest ->
            let dir = move |> charToDir |> dirToPair
            let nextPos = pos |> add dir

            match get nextPos with
            | Wall -> f pos rest
            | OpenSpace -> f nextPos rest
            | Box
            | LargeBoxLeft
            | LargeBoxRight ->
                match collectBoxes get nextPos dir with
                | None -> f pos rest
                | Some boxes ->
                    moveBoxes dir boxes
                    f nextPos rest

    f pos moves

let solve mapPatch scorer data =
    let map, moves = data |> toMapAndMoves mapPatch
    let _, get = map |> api

    let start =
        map
        |> findIndex robot
        |> Option.defaultValue (-1, -1)

    map[fst start, snd start] <- '.'

    moveRobot start moves map get
    |> score scorer

let scaleMap line =
    line
    |> Seq.fold
        (fun acc c ->
            match c with
            | '@' -> '.' :: '@' :: acc
            | 'O' -> ']' :: '[' :: acc
            | v -> v :: v :: acc)
        []
    |> List.rev
    |> List.toArray

let solution1 = solve id box
let solution2 = solve (Array.map scaleMap) largeBoxLeft

loadSampleDataLines "Puzzle2.txt"
|> Array.fold
    (fun acc v ->
        match v with
        | "" -> swap acc
        | value -> acc |> fmap2 (id, (fun x -> value :: x)))
    (List.empty<string>, List.empty<string>)
|> map (List.rev >> Array.ofList)
|> run [ solution1; solution2 ]
