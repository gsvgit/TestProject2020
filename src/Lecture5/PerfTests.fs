module Lecture5.PerfTests

open System
open System
open System.Security.Cryptography
open System.Threading
open TestProj2020

[<Struct>]
type PerfConfig =
    val From: int
    val Step: int
    val To  : int
    val ForceGC: bool
    new (_from, _step, _to, forceGC) = {From = _from; Step = _step; To = _to; ForceGC = forceGC}

let genRandomList n =
    let rand = new System.Random()
    List.init n (fun _ -> rand.Next())

let time f =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let res = f()
    let time = timer.ElapsedMilliseconds
    res,time

let perfTests (perfConfig:PerfConfig) sortFun sycles file =
    let steps = [perfConfig.From .. perfConfig.Step .. perfConfig.To]
    let timings = Array.zeroCreate steps.Length
    let mutable j = 0
    for i in steps do
        let lst = genRandomList i
        let times = Array.zeroCreate sycles
        for j in 0 .. sycles - 1 do
            if perfConfig.ForceGC then GC.Collect()
            let res,time = time (fun _ -> sortFun lst)
            if perfConfig.ForceGC then GC.Collect()
            times.[j] <- string time
            printfn "Measured: len = %A, iteration = %A, time = %A" i j time
        timings.[j] <- (string i) + ","  + (String.concat ", " times)
        j <- j + 1

    System.IO.File.WriteAllLines (file, timings)
