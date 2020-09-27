module TestProj2020.Fib

let rec fib n =
    if n = 0 || n = 1
    then 1
    else
        fib (n-1)
      + fib (n-2)

let rec factorial n =
    if n = 1 then 1 else n * factorial (n-1)

let factorialTail n =
    let rec _go n acc =
        if n = 1
        then acc
        else _go (n - 1) (acc * n)
    _go n 1

let multiplyMatrices (m1:array<array<_>>) (m2:array<array<_>>) =
    let m = m1.Length
    let n = m1.[0].Length
    let k = m2.[0].Length
    let res = Array.init m (fun _ -> Array.zeroCreate k)
    for i in 0..m - 1 do
        for j in 0..k - 1 do
            for l in 0..n - 1 do
                res.[i].[j] <- res.[i].[j] + m1.[i].[l] * m2.[l].[j]
    res

let rec pow m n =
    let e = [|[|1; 0|]
              [|0; 1|]|]
    if n = 0
    then e
    else multiplyMatrices m (pow m (n-1))

let rec fibMatrix n =
    let m = [|[|0; 1|]
              [|1; 1|]|]

    pow m n

let arr2D =
    let x = Array2D.create 2 2 1
    let y = Array2D.create 2 2 2
    x.[0,0]


[<Struct>]
type Point2d =
    val X: float
    val Y: float

    new(x,y) = {X=x; Y=y}


let points n = Array.init n (fun i -> new Point2d(float i, float i))
let f n i = (points n).[i].X

let fList =
    let l = [1; 2; 3]
    let l1 = []
    let h = l1.Head
    let t = List.tail l
    2 :: l
