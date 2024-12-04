namespace Utils

open System.IO

module SampleData =

    let loadSampleData filePath = File.ReadAllText filePath

    let dump a =
        printfn $"{a}"
        a

    let flip f x y = f y x

    let zeroOne =
        function
        | false -> 0
        | true -> 1
