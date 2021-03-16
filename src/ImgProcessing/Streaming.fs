module ImgPorcessing.Streaming

open TestProj2020

let listAllFiles dir =
    let files = System.IO.Directory.GetFiles(dir)
    List.ofArray files

let processAllFiles inDir outDir filters =
    let outFile fileFullPath =
        System.IO.Path.Combine(outDir, System.IO.Path.GetFileName fileFullPath)

    let platform, queue = ImgHelpers.getCommandQueue "*NVIDIA*"
    let filter = ImgHelpers.applyFilters platform queue 64 filters

    for file in (listAllFiles inDir) do
        printfn "%A" file
        let bArray = ImgHelpers.loadAs2DArray file
        let grey = ImgHelpers.toGrayscale bArray
        let res = filter grey
        ImgHelpers.save2DByteArrayAsImage res (outFile file)

type msg =
    | Go of AsyncReplyChannel<unit>
    | Img of string*byte[,]
    | EOS of AsyncReplyChannel<unit>

let imgLoader inDir (imgProcessor:MailboxProcessor<_>) =
    MailboxProcessor.Start(fun inbox ->
        let rec loop files =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Image loader is ready to finish!"
                    imgProcessor.PostAndReply (fun ch -> EOS ch)
                    printfn "Image loader is finished!"
                    ch.Reply()
                | Go ch ->
                    match files with
                    | [] ->
                        printfn "Image reading is finished!"
                        inbox.Post (EOS ch)
                        return! loop files
                    | file :: files ->
                        printfn "Load: %A" file
                        let bArray = ImgHelpers.loadAs2DArray file
                        let grey = ImgHelpers.toGrayscale bArray
                        imgProcessor.Post (Img (file,grey))
                        inbox.Post (Go ch)
                        return! loop files
            }
        loop (listAllFiles inDir)
        )

let imgProcessor filters (imgSaver:MailboxProcessor<_>) =

    let platform, queue = ImgHelpers.getCommandQueue "*NVIDIA*"
    let filter = ImgHelpers.applyFilters platform queue 64 filters

    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Image processor is ready to finish!"
                    imgSaver.PostAndReply (fun ch -> EOS ch)
                    printfn "Image processor is finished!"
                    ch.Reply()
                | Img (file,img) ->
                    printfn "Filter: %A" file
                    let filtered = filter img
                    imgSaver.Post (Img (file,filtered))
                    return! loop ()
            }
        loop ()
        )

let imgSaver outDir =
    let outFile fileFullPath =
        System.IO.Path.Combine(outDir, System.IO.Path.GetFileName fileFullPath)

    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async{
                let! msg = inbox.Receive()
                match msg with
                | EOS ch ->
                    printfn "Image saver is finished!"
                    ch.Reply()
                | Img (file, img) ->
                    printfn "Save: %A" file
                    ImgHelpers.save2DByteArrayAsImage img (outFile file)
                    return! loop ()
            }
        loop ()
        )

let processAllFilesAsync inDir outDir filters =
    let imgSaver = imgSaver outDir
    let imgProcessor = imgProcessor filters imgSaver
    let imgLoader = imgLoader inDir imgProcessor
    imgLoader.PostAndReply(fun ch -> Go ch)
