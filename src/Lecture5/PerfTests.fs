module Lecture5.PerfTests

open System
open System.Security.Cryptography
open TestProj2020

[<Struct>]
type PerfConfig =
    val From: int
    val Step: int
    val To  : int
    new (_from, _step, _to) = {From = _from; Step = _step; To = _to}

let genRandomList n =
    let rand = new System.Random()
    List.init n (fun _ -> rand.Next())

let time f =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let res = f()
    let time = timer.ElapsedMilliseconds
    res,time

let perfTests (perfConfig:PerfConfig) sortFun sicles file =
    let steps = [perfConfig.From .. perfConfig.Step .. perfConfig.To]
    let timings = Array.zeroCreate steps.Length
    let mutable j = 0
    for i in steps do
        let lst = genRandomList i
        let times = Array.zeroCreate sicles
        for j in 0 .. sicles - 1 do
            let res,time = time (fun _ -> sortFun lst)
            times.[j] <- string time
            printfn "%A" time
        timings.[j] <- (string i) + ","  + (String.concat ", " times)
        j <- j + 1

    System.IO.File.WriteAllLines (file, timings)
