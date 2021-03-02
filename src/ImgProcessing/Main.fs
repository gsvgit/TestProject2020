namespace TestProj2020

open System.Drawing.Imaging

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

        let bArray = ImgHelpers.loadAs2DArray img1
        let grey = ImgHelpers.toGrayscale bArray

        let r () =
            ImgHelpers.applyFilter3 ImgHelpers.gaussianBlurKernel grey
            |> ImgHelpers.applyFilter2 ImgHelpers.gaussianBlurKernel
            |> ImgHelpers.applyFilter2 ImgHelpers.gaussianBlurKernel
            |> ImgHelpers.applyFilter2 ImgHelpers.gaussianBlurKernel
            |> ImgHelpers.applyFilter3 ImgHelpers.edgesKernel
            //|> ImgHelpers.applyFilter ImgHelpers.gaussianBlurKernel
        let r,t = time r
        printfn "Execution time: %A seconds" t
        ImgHelpers.save2DByteArrayAsImage r (img1 + "_out_blur_edg_4.jpg")
        0
