﻿namespace Utils

open System.IO
open System.Reflection

module SampleData =

    let loadSampleData filePath = File.ReadAllText filePath

    let loadSampleDataLines filePath =
        Path.Join(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), filePath)
        |> File.ReadAllLines

    let run solutions data =
        match solutions |> List.length with
        | 0 -> printfn "No solutions provided"
        | _ ->
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()

            let measureSolution data i solution =
                stopwatch.Restart()
                let result = solution data
                stopwatch.Stop()
                printfn $"Result {i + 1}: {result}, Elapsed: {stopwatch.ElapsedMilliseconds} ms"

            solutions |> List.iteri (measureSolution data)

    let dump a =
        printfn $"%A{a}"
        a

    let inline flip f x y = f y x


    let boolToint x y =
        function
        | true -> x
        | false -> y

    let zeroOne =
        function
        | false -> 0
        | true -> 1
