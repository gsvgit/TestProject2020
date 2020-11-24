module Matrices

let multiply (m1:'t [,]) (m2:'t [,]) opMult opPlus =
    if m1.GetLength 1 = m2.GetLength 0
    then
        let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
        for i in 0..m1.GetLength 0 - 1 do
            for j in 0..m2.GetLength 1 - 1 do
                for k in 0..m1.GetLength 1 - 1 do
                    res.[i,j] <- opPlus res.[i,j] (opMult m1.[i,k] m2.[k,j])
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
