namespace Utils

open System
open System.Text.RegularExpressions

module Pair =
    let fromArray (a: 'a array) = (a.[0], a.[1])
    let fromList (a: 'a list) = (List.head a, List.head (List.tail a))
    let map (f: 'a -> 'b) (a: 'a * 'a) = (f (fst a), f (snd a))
    let map2 (f: 'a -> 'b -> 'c) (g: 'a -> 'b -> 'd) (a: 'a * 'b) = f (fst a) (snd a), g (fst a) (snd a)

    let fmap2 (a: ('a -> 'c) * ('b -> 'd)) (b: 'a * 'b) = ((fst a) (fst b), (snd a) (snd b))
    let fmap (f: 'a -> 'b -> 'c) (b: 'a * 'b) = f (fst b) (snd b)
    let swap (a: 'a * 'b) = (snd a, fst a)
    let uncurry (f: 'a -> 'b -> 'c) (a: 'a * 'b) = f (fst a) (snd a)
    let curry (f: 'a * 'b -> 'c) (a: 'a) (b: 'b) = f (a, b)
    let branch (f: 'a -> 'b) (g: 'a -> 'c) (a: 'a) = (f a, g a)
    let add (a, b) (c, d) = (a + c, b + d)
    let scale factor (a, b) = (a * factor, b * factor)
    let opposite = scale -1


module String =
    let split (d: string) (x: string) =
        x.Split(d, StringSplitOptions.RemoveEmptyEntries)

    let splitChars (d: char array) (x: string) = x.Split(d)

    let words = split " "

    let splitLines = splitChars [| '\n'; '\r' |]

    let matches patern input = Regex.Matches(input, patern)



module Combinators =
    let S f g h x = f (g x) (h x)
    let K x _ = x

type Dir =
    | Unknown
    | Up
    | Down
    | Left
    | Right

module Dir =
    let charToDir =
        function
        | '^' -> Up
        | 'v' -> Down
        | '<' -> Left
        | '>' -> Right
        | _ -> raise (System.Exception("Invalid direction"))

    let rotateRight =
        function
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    let dirToPair =
        function
        | Up -> (-1, 0)
        | Down -> (1, 0)
        | Left -> (0, -1)
        | Right -> (0, 1)


module Array2D =

    let sum (arr: _ array2d) =
        let rec sumHelper i j acc =
            if i >= Array2D.length1 arr then
                acc
            elif j >= Array2D.length2 arr then
                sumHelper (i + 1) 0 acc
            else
                sumHelper i (j + 1) (acc + arr.[i, j])

        sumHelper 0 0 0

    let findIndex (f: _ -> bool) (arr: _ array2d) =
        let rec findIndexHelper i j =
            if i >= Array2D.length1 arr then
                None
            elif j >= Array2D.length2 arr then
                findIndexHelper (i + 1) 0
            elif f arr.[i, j] then
                Some(i, j)
            else
                findIndexHelper i (j + 1)

        findIndexHelper 0 0

    let inBounds (arr: _ array2d) x y =
        x >= 0
        && y >= 0
        && x < Array2D.length1 arr
        && y < Array2D.length2 arr



    let windowFold (f: 's -> _ array2d * _ * _ * _ * _ -> 's) (state: 's) (window: _ * _) (arr: _ array2d) =
        let rec foldHelper i j s =
            if i > Array2D.length1 arr - fst window then
                s
            else if j > Array2D.length2 arr - snd window then
                foldHelper (i + 1) 0 s
            else
                foldHelper i (j + 1) (f s (arr, i, j, i + fst window - 1, j + snd window - 1))

        foldHelper 0 0 state

    let print (arr: _ array2d) get =
        let rec printHelper i j =
            if i >= Array2D.length1 arr then
                ()
            elif j >= Array2D.length2 arr then
                printfn ""
                printHelper (i + 1) 0
            else
                printf $"{(get arr (i, j))}"
                printHelper i (j + 1)

        printHelper 0 0

    let collectDirs (x, y) acc (p, t) =
        match (x, y) = p with
        | true ->
            match acc with
            | Some x when
                (x = char "|" || x = char " ")
                && (t = Up || t = Down)
                ->
                Some '|'
            | Some x when
                (x = char "-" || x = char " ")
                && (t = Left || t = Right)
                ->
                Some '-'
            | Some x when x = char "|" && (t = Left || t = Right) -> Some '+'
            | Some x when x = char "-" && (t = Up || t = Down) -> Some '+'
            | Some x -> Some x
            | None ->
                if t = Up || t = Down then
                    Some '|'
                else
                    Some '-'
        | _ -> acc

    let pathPrinter path get (_: _ array2d) (x, y) =
        match (path |> Set.fold (collectDirs (x, y)) None) with
        | Some x -> x
        | _ -> get (x, y)
