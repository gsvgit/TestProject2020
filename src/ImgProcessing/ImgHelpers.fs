module TestProj2020.ImgHelpers
open System.Drawing
open Brahma.FSharp.OpenCL.Translator.Common
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

let loadAs2DArray (file:string) =
    let img = new Bitmap(file)
    let res = Array2D.zeroCreate img.Height img.Width
    for i in 0.. img.Height - 1 do
        for j in 0 .. img.Width - 1 do
            res.[i,j] <- img.GetPixel(j,i)
    printfn "H=%A W=%A" img.Height img.Width
    res

let toGrayscale (img:Color[,]) =
    img |> Array2D.map (fun x -> (x.R + x.G + x.B) / 3uy)

let save2DByteArrayAsImage (imageData: byte[,]) file =
    let h = imageData.GetLength 0
    let w = imageData.GetLength 1
    printfn "H=%A W=%A" h w
    let img = new Bitmap(w,h)
    for i in 0.. h - 1 do
        for j in 0 .. w - 1 do
            let level = int imageData.[i,j]
            img.SetPixel(j,i,Color.FromArgb(level, level, level))

    img.Save file

let gaussianBlurKernel =
    [|
      [| 1; 4;  6;  4;  1|]
      [| 4; 16; 24; 16; 4|]
      [| 6; 24; 36; 24; 6|]
      [| 4; 16; 24; 16; 4|]
      [| 1; 4;  6;  4;  1|]
    |]
    |> Array.map (fun a -> Array.map (fun x -> (float32 x) / 256.0f) a)

let edgesKernel =
    [|
      [|0;  0; -1;  0;  0|]
      [|0;  0; -1;  0;  0|]
      [|0;  0;  2;  0;  0|]
      [|0;  0;  0;  0;  0|]
      [|0;  0;  0;  0;  0|]
    |]
    |> Array.map (fun x -> Array.map float32 x)

let applyFilter (filter: 't[][]) (img: byte[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1
    let filterD = (Array.length filter) / 2
    let filter = Array.concat filter
    let processPixel px py =
        let dataToHandle =
            [|
              for i in px - filterD .. px + filterD do
                for j in py - filterD .. py + filterD do
                    if i < 0 || i >= imgH || j < 0 || j >= imgW
                    then float img.[px,py]
                    else float img.[i,j]
            |]
        Array.fold2 (fun s x y -> s + x * y) 0.0 filter dataToHandle

    Array2D.mapi (fun x y _ -> byte (processPixel x y)) img

let applyFilter2 (filter: 't[][]) (img: byte[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1
    let img =
            [| for x in 0 .. Array2D.length1 img - 1 do
               yield [| for y in 0 .. Array2D.length2 img - 1 -> img.[x, y] |]
            |]
    let filterD = (Array.length filter) / 2
    let filter = Array.concat filter
    let processPixel px py =
        let dataToHandle =
            [|
              for i in px - filterD .. px + filterD do
                for j in py - filterD .. py + filterD do
                    if i < 0 || i >= imgH || j < 0 || j >= imgW
                    then float img.[px].[py]
                    else float img.[i].[j]
            |]
        Array.fold2 (fun s x y -> (s + x * y)) 0.0 filter dataToHandle
        |> byte

    Array.Parallel.mapi (fun x l -> Array.mapi (fun y _ ->  processPixel x y) l) img
    |> array2D


let applyFilter3 (filter: 't[][]) (img: byte[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1
    let img =
            [| for x in 0 .. Array2D.length1 img - 1 do
               yield! [| for y in 0 .. Array2D.length2 img - 1 -> img.[x, y] |]
            |]
    let filterD = (Array.length filter) / 2
    let filter = Array.concat filter

    let result = Array2D.zeroCreate imgH imgW

    let processPixel p =
        let pw = p % imgW
        let ph = p / imgW

        let mutable res = 0.0

        for i in ph - filterD .. ph + filterD do
            for j in pw - filterD .. pw + filterD do
                let d =
                    if i < 0 || i >= imgH || j < 0 || j >= imgW
                    then float img.[p]
                    else float img.[i * imgW + j]
                let f = filter.[(i - ph + filterD) * (2 * filterD + 1) + (j - pw + filterD)]
                res <- res + d * f
        byte res

    Array.Parallel.iteri (fun x v -> result.[x / imgW, x % imgW] <- processPixel x) img

    result

let getCommandQueue platformName =
    let deviceType = DeviceType.Default

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with
        | ex -> failwith ex.Message

    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    provider, commandQueue

let applyFilter4 (provider:ComputeProvider) (commandQueue:CommandQueue) localWorkSize (filter: float32[][]) (img: byte[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1
    let img =
            [| for x in 0 .. Array2D.length1 img - 1 do
               yield! [| for y in 0 .. Array2D.length2 img - 1 -> img.[x, y] |]
            |]
    let filterD = (Array.length filter) / 2
    let filterSize = filter.Length * filter.Length

    let result = Array2D.zeroCreate imgH imgW

    let command =
        <@
            fun (r:_1D) (img:array<_>) (filter:array<_>) (result:array<_>) ->
                let p = r.GlobalID0
                let localId = r.LocalID0

                let pw = p % imgW
                let ph = p / imgW

                let localFilter = localArray filterSize

                if localId < filterSize then localFilter.[localId] <- filter.[localId]

                barrier()

                let mutable res = 0.0f

                for i in ph - filterD .. ph + filterD do
                    for j in pw - filterD .. pw + filterD do
                        let mutable d = 0uy
                        if i < 0 || i >= imgH || j < 0 || j >= imgW
                        then d <- img.[p]
                        else d <- img.[i * imgW + j]
                        let f = localFilter.[(i - ph + filterD) * (2 * filterD + 1) + (j - pw + filterD)]
                        res <- res + (float32 d) * f
                result.[p] <- res
        @>

    let str = ref ""
    let kernel, kernelPrepare, kernelRun = provider.Compile command //(command, _options = CompileOptions.DisableOptimizations, _outCode = str, translatorOptions=[TranslatorOption.BoolAsBit])
    let d = _1D(imgH * imgW, localWorkSize)
    let result' = Array.zeroCreate (imgH * imgW)
    let filter = Array.concat filter

    kernelPrepare d img filter result'

    commandQueue.Add(kernelRun()) |> ignore
    let _ = commandQueue.Add(result'.ToHost provider).Finish()

    Array.Parallel.iteri (fun x v -> result.[x / imgW, x % imgW] <- byte v) result'

    provider.CloseAllBuffers()

    result


let applyFilters (provider:ComputeProvider) (commandQueue:CommandQueue) localWorkSize (filters: list<float32[][]>) (img: byte[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1
    let img =
            [| for x in 0 .. Array2D.length1 img - 1 do
               yield! [| for y in 0 .. Array2D.length2 img - 1 -> img.[x, y] |]
            |]

    let applyFilter img (filter:'t[][]) =
        let filterD = (Array.length filter) / 2
        let filter = Array.concat filter

        let filterSize = filter.Length

        let command =
            <@
                fun (r:_1D) (img:array<_>) (filter:array<_>) (result:array<_>) ->
                    let p = r.GlobalID0
                    //let localId = r.LocalID0

                    let pw = p % imgW
                    let ph = p / imgW

                    //let localFilter = localArray filterSize

                    //if localId < filterSize then localFilter.[localId] <- filter.[localId]

                    //barrier()

                    let mutable res = 0.0f

                    for i in ph - filterD .. ph + filterD do
                        for j in pw - filterD .. pw + filterD do
                            let mutable d = 0uy
                            if i < 0 || i >= imgH || j < 0 || j >= imgW
                            then d <- img.[p]
                            else d <- img.[i * imgW + j]
                            let f = filter.[(i - ph + filterD) * (2 * filterD + 1) + (j - pw + filterD)]
                            res <- res + (float32 d) * f
                    result.[p] <-  res
            @>

        let kernel, kernelPrepare, kernelRun = provider.Compile command
        let d = _1D(imgH * imgW, localWorkSize)
        let result' = Array.zeroCreate (imgH * imgW)
        kernelPrepare d img filter result'

        commandQueue.Add(kernelRun()) |> ignore
        let _ = commandQueue.Add(result'.ToHost provider).Finish()
        provider.CloseAllBuffers()
        result'

    let mutable res = img
    for filter in filters do
        res <- applyFilter res filter |> Array.Parallel.map (byte)
    let result = Array2D.zeroCreate imgH imgW
    Array.Parallel.iteri (fun x v -> result.[x / imgW, x % imgW] <- byte v) res

    provider.CloseAllBuffers()

    result
