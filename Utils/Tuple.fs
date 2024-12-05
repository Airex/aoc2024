namespace Utils

open System
open System.Text.RegularExpressions

module Pair =
    let fromArray (a: 'a array) = (a.[0], a.[1])
    let fromList (a: 'a list) = (List.head a, List.head (List.tail a))
    let map (f: 'a -> 'b) (a: 'a * 'a) = (f (fst a), f (snd a))
    let map2 (f: 'a -> 'b -> 'c) (a: 'a * 'a) (b: 'b * 'b) = (f (fst a) (fst b), f (snd a) (snd b))

    let fmap2 (a: ('a -> 'c) * ('b -> 'd)) (b: 'a * 'b) = ((fst a) (fst b), (snd a) (snd b))

    let swap (a: 'a * 'b) = (snd a, fst a)

    let uncurry (f: 'a -> 'b -> 'c) (a: 'a * 'b) = f (fst a) (snd a)

    let add (a, b) (c, d) = (a + c, b + d)
    let scale factor (a, b) = (a * factor, b * factor)


module String =
    let split (d: string) (x: string) =
        x.Split(d, StringSplitOptions.RemoveEmptyEntries)

    let words = split " "

    let splitLines = split ("\n")

    let matches patern input = Regex.Matches(input, patern)



module Combinators =
    let S f g h x = f (g x) (h x)


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

    let inBounds (arr: _ array2d) x y =
        x >= 0
        && y >= 0
        && x < Array2D.length1 arr
        && y < Array2D.length2 arr



    let windowFold
        (f: 's -> _ array2d * _ * _ * _ * _ -> 's)
        (state: 's)
        (window: _ * _)
        (arr: _ array2d)
        =
        let rec foldHelper i j s =
            if i > Array2D.length1 arr - fst window then
                s
            else if j > Array2D.length2 arr - snd window then
                foldHelper (i + 1) 0 s
            else
                foldHelper i (j + 1) (f s (arr, i, j, i + fst window - 1, j + snd window - 1))

        foldHelper 0 0 state
