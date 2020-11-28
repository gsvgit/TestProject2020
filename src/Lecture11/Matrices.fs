module Matrices
open System.Collections.Generic
open System.Threading.Tasks
open System.Threading.Tasks

let multiply (m1:'t [,]) (m2:'t [,]) opMult opPlus =
    if m1.GetLength 1 = m2.GetLength 0
    then
        let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
        Parallel.For(0, m1.GetLength 0, fun i ->
        //for i in 0..m1.GetLength 0 - 1 do
            for j in 0..m2.GetLength 1 - 1 do
                for k in 0..m1.GetLength 1 - 1 do
                    res.[i,j] <- opPlus res.[i,j] (opMult m1.[i,k] m2.[k,j])
                    )
        res
    else failwith "Incorrect size of input matrices."


let kron (m1:'t [,]) (m2:'t [,]) opMult =
    let rows = m1.GetLength 0 * m2.GetLength 0
    let columns = m1.GetLength 1 * m2.GetLength 1
    let res = Array2D.zeroCreate rows columns
    for i in 0..rows - 1 do
        for j in 0..columns - 1 do
            res.[i,j] <- opMult m1.[i / m2.GetLength 0, j / m2.GetLength 1 ] m2.[i % m2.GetLength 0, j % m2.GetLength 0]
    res

let elementwiseAddInPlace (m1:'t [,]) (m2:'t [,]) opAdd =
    if m1.GetLength 0 = m2.GetLength 0 && m1.GetLength 1 = m2.GetLength 1
    then Array2D.iteri (fun i j x -> m1.[i,j] <- opAdd x m1.[i,j]) m2
    else failwith "Incorrect size of input matrices."

let toBooleanSparse (mtx:_[,]) =
    mtx
    |> Array2D.mapi (fun i j x -> if x then Some (i,j) else None)
    |> Seq.cast<_>
    |> Seq.choose id
    |> fun x -> new HashSet<_>(x)

let multBoolSparse mtx1 mtx2 =
    [|
        for (i,j) in mtx1 do
            for (k,l) in mtx2 do
                if k = j then i,l
    |] |> Array.distinct

let multBoolSparseParallel mtx1 mtx2 =
    mtx1
    |> Array.Parallel.map
        (fun (i,j) ->
            [|for (k,l) in mtx2 do
                 if k = j then i,l|])
    |> Array.concat |> Array.distinct


let multBoolSparseParallel2 (mtx1:HashSet<_>) mtx2 =
    let res = new System.Collections.Concurrent.ConcurrentBag <_>()
    Parallel.ForEach(mtx1, fun (i,j) ->
            let r = new HashSet<_>()
            for (k,l) in mtx2 do
                if k = j then r.Add((i,l))|> ignore
            res.Add r)
    //|> Array.iter (fun s -> res.UnionWith s)
    let r = new HashSet<_>()
    res//.ToArray()
    |> Seq.iter(fun x -> r.UnionWith x)
    r
    //|> Array.ofSeq

let elementwiseAddBoolSparseInplace (mtx1:HashSet<_>) mtx2 =
    mtx1.UnionWith mtx2

let cls mtx counter opMult opAdd =
    let count r =
        let mutable cnt = 0
        r |> Array2D.iter (fun i -> if counter i then cnt <- cnt + 1)
        cnt

    let mutable cls = mtx
    let mutable _continue = true
    while _continue do
        let prev = count cls
        let r =
            multiply
                cls
                cls
                opMult
                opAdd

        elementwiseAddInPlace cls r opAdd
        let cur = count cls
        //printfn "prev = %A cur = %A" prev cur
        if prev = cur
        then _continue <- false
    cls
