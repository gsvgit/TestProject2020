module ImgPorcessing.Matrices

open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions


let getCommandQueue platformName =
    let deviceType = DeviceType.Default

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with
        | ex -> failwith ex.Message

    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    provider, commandQueue

let getRandomMatrix n m =
    let rnd = System.Random()
    let res = Array2D.zeroCreate n m
    for i in 0 .. n - 1 do
        for j in 0 .. m - 1 do
            res.[i,j] <- rnd.NextDouble()
    res
let multiply (provider:ComputeProvider) (commandQueue:CommandQueue) localWorkSize (m1: float[,]) (m2: _[,]) =
    let m1Rows = m1.GetLength 0
    let m1Cols = m1.GetLength 1

    let m2Rows = m2.GetLength 0
    let m2Cols = m2.GetLength 1

    let resRows = m1Rows
    let resCols = m2Cols

    let mToArr m =
            [| for x in 0 .. Array2D.length1 m - 1 do
               yield! [| for y in 0 .. Array2D.length2 m - 1 -> m.[x, y] |]
            |]

    let m1Arr = mToArr m1
    let m2Arr = mToArr m2

    let command =
        <@
            fun (r:_2D) (m1:array<_>) (m2:array<_>) (res:array<_>) ->
                let row = r.GlobalID0
                let col = r.GlobalID1
                let mutable buf = res.[row * resCols + col]
                for k in 0 .. m2Rows - 1 do
                    //res.[row * resCols + col] <- res.[row * resCols + col] + (m1.[row * m1Cols + k] * m2.[k * m2Cols + col])
                    buf <- buf + (m1.[row * m1Cols + k] * m2.[k * m2Cols + col])
                res.[row * resCols + col] <- buf
        @>

    let kernel, kernelPrepare, kernelRun =
        provider.Compile command

    let d = _2D(resRows, resCols, localWorkSize)
    let result' = Array.zeroCreate (resRows * resCols)

    kernelPrepare d m1Arr m2Arr result'

    commandQueue.Add(kernelRun()) |> ignore
    let _ = commandQueue.Add(result'.ToHost provider).Finish()

    let result = Array2D.zeroCreate resRows resCols

    Array.Parallel.iteri (fun x v -> result.[x / resCols, x % resCols] <-  v) result'

    provider.CloseAllBuffers()

    result
