namespace TestProj2020

open System.Drawing.Imaging
open ImgPorcessing

module Main =
    open Argu

    type CLIArguments =
        | TaskPow of int*int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | TaskPow _ -> "Run pow function"

    let time f =
        let start = System.DateTime.Now
        let r = f ()
        let final = (System.DateTime.Now - start).TotalMilliseconds / 1000.0
        r,final

    [<EntryPoint>]
    let main (argv: string array) =
        let img1 = "../../../Examples/sara-budhwani-h_P71-B8BPw-unsplash.jpg"
        let img2 = "../../../Examples/nick-gavrilov-F-rvSJl6qI0-unsplash.jpg"
        let inDir = "../../../Examples/input"
        let outDir = "../../../Examples/output"
        let filters = (List.init 4 (fun i -> ImgHelpers.gaussianBlurKernel) @ [ImgHelpers.edgesKernel])

        (*let bArray = ImgHelpers.loadAs2DArray img1
        let grey = ImgHelpers.toGrayscale bArray

        let platform, queue = ImgHelpers.getCommandQueue "*NVIDIA*"

        let filter4 = ImgHelpers.applyFilter4 platform queue 64

        let filter =
            filter4
            //ImgHelpers.applyFilter3
        let r1 () = ImgHelpers.applyFilters platform queue 64 filters grey
        let r () =
            grey
            |> filter ImgHelpers.gaussianBlurKernel
            |> filter ImgHelpers.gaussianBlurKernel
            |> filter ImgHelpers.gaussianBlurKernel
            |> filter ImgHelpers.gaussianBlurKernel
            |> filter ImgHelpers.edgesKernel
*)
        //let r,t = time r
        //let _do () = ImgPorcessing.Streaming.processAllFiles inDir outDir filters
        let _do () = ImgPorcessing.Streaming.processAllFilesAsync inDir outDir filters
        let t = time _do
        printfn "Execution time: %A seconds" t
        //ImgHelpers.save2DByteArrayAsImage r (img2 + "_out_blur_edg_10_gpu_2_br.jpg")
        0
