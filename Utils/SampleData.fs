namespace Utils

open System.IO

module SampleData =

    let loadSampleData filePath = File.ReadAllText filePath

    let loadSampleDataLines filePath = File.ReadAllLines filePath

    let run solutions data =
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        let measureSolution data i solution =
            stopwatch.Restart()
            let result = solution data
            stopwatch.Stop()
            printfn $"Result {i + 1}: {result}, Elapsed: {stopwatch.ElapsedTicks}ticks"

        solutions |> List.iteri (measureSolution data)

    let dump a =
        printfn $"%A{a}"
        a

    let flip f x y = f y x


    let boolToint x y =
        function
        | true -> x
        | false -> y

    let zeroOne =
        function
        | false -> 0
        | true -> 1
