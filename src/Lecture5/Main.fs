namespace TestProj2020

module Main =
    open Lecture5
    open Lecture5.PerfTests

    [<EntryPoint>]
    let main (argv: string array) =
        let config = new PerfConfig(10, 10, 20 * 10)
        Lecture5.PerfTests.perfTests config List.sort 2  "out1.csv" //"timingsSystemListSortOut.csv"
        Lecture5.PerfTests.perfTests config Sorts.qSort 2 "out2.csv" //"timingsCustomListSortOut.csv"
        0
