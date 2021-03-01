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

    [<EntryPoint>]
    let main (argv: string array) =
        let img1 = "../../../Examples/sara-budhwani-h_P71-B8BPw-unsplash.jpg"
        let img2 = "../../../Examples/nick-gavrilov-F-rvSJl6qI0-unsplash.jpg"

        let bArray = ImgHelpers.loadAs2DArray img2
        let grey = ImgHelpers.toGrayscale bArray
        let r =
            ImgHelpers.applyFilter2 ImgHelpers.gaussianBlurKernel grey
            |> ImgHelpers.applyFilter2 ImgHelpers.gaussianBlurKernel
            |> ImgHelpers.applyFilter2 ImgHelpers.gaussianBlurKernel
            |> ImgHelpers.applyFilter2 ImgHelpers.gaussianBlurKernel
            |> ImgHelpers.applyFilter2 ImgHelpers.edgesKernel
            //|> ImgHelpers.applyFilter ImgHelpers.gaussianBlurKernel
        ImgHelpers.save2DByteArrayAsImage r (img2 + "out_3.jpg")
        0
