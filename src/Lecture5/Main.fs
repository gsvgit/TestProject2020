namespace TestProj2020

module Main =
    open Lecture5
    open Lecture5.PerfTests

    [<EntryPoint>]
    let main (argv: string array) =
        let config = new PerfConfig(10_000, 20_000, 200 * 10_000, false)
        Lecture5.PerfTests.perfTests config List.sort 5  "timingsSystemListSortOut5_release_GC_charging_2.csv"
        Lecture5.PerfTests.perfTests config Sorts.qSort 5 "timingsCustomListSortOut5_release_GC_charging_2.csv"
        0
