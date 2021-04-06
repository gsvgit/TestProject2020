namespace TestProj2020

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
        let time f =
            let start = System.DateTime.Now
            let r = f()
            let time = (System.DateTime.Now - start).TotalMilliseconds
            r,time

        //let tr, t1 = time (fun () -> tree1 28)
        let tree h = Parallel.genRandomTreeParallel h 2

        //let tree1 h = Parallel.genRandomTreeParallel2 h 1
        //let tree2 h = Parallel.genRandomTreeParallel3 h 1

        //
        //printfn "!!"
        //let _, t2 = time (fun () -> tree2 28)
        //printfn "%A %A" t2 t2

        //Tree creation time = 35423.0119, tree traverse time = 3334.0866, result = 1245664461
        let tr,tm = time (fun () -> tree 28)

        let r,t1 = time (fun () -> Parallel.sumTree tr)
        let r,t2 = time (fun () -> Parallel.sumTreeParallel tr 2)

        printfn "Tree creation time = %A, tree traverse time seq = %A, tree traverse time parallel = %A, result = %A" tm t1 t2 r

(*
        let mtx = Parallel.genRandomMatrix 1024

        let mxm () = Parallel.mm mtx mtx
        let mxmP1 () = Parallel.mmParallel mtx mtx
        let mxmP2 () = Parallel.mmParallel2 mtx mtx 4
        let mxmP3 () = Parallel.mmParallel2 mtx mtx 8
        let mxmP4 () = Parallel.mmParallel2 mtx mtx 2



        let _,t = time mxm
        let _,tp1 = time mxmP1
        let _,tp2 = time mxmP2
        let _,tp3 = time mxmP3
        let _,tp4 = time mxmP4

        printfn "Matrix multiplication. Sequential: %A. Parallel (naive): %A. Parallel (2 threads): %A. Parallel (4 threads): %A. Parallel (8 threads): %A" t tp1 tp4 tp2 tp3
*)
        0

