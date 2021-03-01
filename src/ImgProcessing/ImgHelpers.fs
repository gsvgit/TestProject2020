module TestProj2020.ImgHelpers
open System.Drawing
open System.IO

let loadAs2DArray (file:string) =
    let img = new Bitmap(file)
    let converter = new ImageConverter();
    //let a = converter.ConvertTo(img, typeof<byte[]>) :?> (byte[])
    let res = Array2D.zeroCreate img.Height img.Width
    for i in 0.. img.Height - 1 do
        for j in 0 .. img.Width - 1 do
            res.[i,j] <- img.GetPixel(j,i)
    //a |> Array.iteri (fun i x -> res.[i / img.Width , i % img.Width] <- x)
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
    |> Array.map (fun a -> Array.map (fun x -> (float x) / 256.0) a)

let edgesKernel =
    [|
      [|0;  0; -1;  0;  0|]
      [|0;  0; -1;  0;  0|]
      [|0;  0;  2;  0;  0|]
      [|0;  0;  0;  0;  0|]
      [|0;  0;  0;  0;  0|]
    |]
    |> Array.map (fun x -> Array.map float x)

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

    Array2D.mapi (fun x y v -> byte (processPixel x y)) img

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

    Array.Parallel.mapi (fun x l -> Array.Parallel.mapi (fun y _ ->  processPixel x y) l) img
    |> array2D
