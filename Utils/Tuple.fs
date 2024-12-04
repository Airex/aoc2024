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
        let mutable sum = 0

        for i in 0 .. (Array2D.length1 arr - 1) do
            for j in 0 .. (Array2D.length2 arr - 1) do
                sum <- sum + arr[i, j]

        sum

    let inBounds (arr: _ array2d) x y =
        x >= 0
        && y >= 0
        && x < Array2D.length1 arr
        && y < Array2D.length2 arr

    let windowFold (f: 's * 'b -> 's) (state: 's) (window: int * int) (arr: _ array2d) =
        let mutable s = state

        for i in 0 .. (Array2D.length1 arr - 1 - fst window - 1) do
            for j in 0 .. (Array2D.length2 arr - 1 - snd window - 1) do
                s <- f (s, (arr, i, j, i + fst window, j + snd window))
        s
