module Day22

open System.Collections.Generic
open Utils.Pair
open Utils.SampleData

let inline div32 (x: int64) = x >>> 5
let inline mul64 (x: int64) = x <<< 6
let inline mul2048 (x: int64) = x <<< 11
let inline mix (x: int64) (y: int64) = x ^^^ y
let inline prune (x: int64) = x &&& 0xFFFFFF

let inline op1 x = mul64 x |> mix x |> prune
let inline op2 x = div32 x |> mix x |> prune
let inline op3 x = mul2048 x |> mix x |> prune
let secret = op1 >> op2 >> op3
let inline lastDigit (x: int64) = x % 10L |> int

let inline encode (lst: int list) =
    let shift x = x + 9
    let base19 = 19

    lst |> Seq.mapi (fun i x -> (shift x) * (pown base19 (3 - i))) |> Seq.sum

type SellSeq = int
type SellPrice = int
type Monkey = (SellSeq * SellPrice) list

let getSecretNumberAfterSteps steps initial =
    Seq.initInfinite (fun _ -> (+) 1L)
    |> Seq.take steps
    |> Seq.fold (fun x _ -> (secret x)) initial

let getSecretNumbersforSteps steps initial =
    Seq.initInfinite (fun _ -> (+) 1L)
    |> Seq.take steps
    |> Seq.fold
        (fun (x, list) _ ->
            let next = secret x
            next, (lastDigit next) :: list)
        (initial, [ lastDigit initial ])
    |> snd
    |> List.rev

let getDiffs list =
    let diffs = 0 :: list |> List.pairwise |> List.map (uncurry (-))
    diffs |> List.zip list

let splitIntoWindowsOf n lst =
    lst
    |> List.tail
    |> List.windowed n
    |> List.map ((List.map snd >> encode) -< (List.last >> fst))

let monkey steps wsize s =
    s |> getSecretNumbersforSteps steps |> getDiffs |> splitIntoWindowsOf wsize

let monkeyPricesFolder (dic: Dictionary<int, int>, visited: HashSet<int>) (key, price) =
    if (not <| visited.Contains(key)) then
        match dic.TryGetValue(key) with
        | true, value -> dic[key] <- value + price
        | _ -> dic[key] <- price

        visited.Add(key) |> ignore

    (dic, visited)

let monkeyBestPriceFolder (dic: Dictionary<int, int>) monkey =
    monkey |> Seq.fold monkeyPricesFolder (dic, HashSet<int>()) |> fst

let solution1 = Seq.sumBy (getSecretNumberAfterSteps 2000)

let solution2 data =
    data
    |> Seq.map (monkey 2000 4)
    |> Seq.fold monkeyBestPriceFolder (Dictionary<int, int>())
    |> _.Values
    |> Seq.max
    |> int64

let data =
    loadSampleDataLines "Puzzle2.txt"
    |> Array.filter (_.Length >> (<) 0)
    |> List.ofArray
    |> List.map int64

run [ solution1; solution2 ] data
