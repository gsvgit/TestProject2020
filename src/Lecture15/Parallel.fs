module TestProj2020.Parallel

type tree =
    | Leaf of int
    | Node of tree*tree

let genRandomTree h =
    let rand = new System.Random()
    let rec go h =
        if h = 1
        then Leaf(rand.Next())
        else Node (go (h-1), go (h-1))
    go h

let genRandomTreeParallel h p =
    let rand = new System.Random()
    let rec go h c =
        if h = 1
        then Leaf(rand.Next())
        else
            if c < p
            then
                let n1 = async{return go (h - 1) (c + 1)}
                let n2 = async{return go (h - 1) (c + 1)}
                let nds = [n1; n2] |> Async.Parallel |> Async.RunSynchronously
                Node (nds.[0], nds.[1])
            else Node (go (h - 1) (c + 1), go (h - 1) (c + 1))
    go h 0

let genRandomTreeParallel2 h p =
    let rand = new System.Random()
    let rec go h c =
        if h = 1
        then Leaf(rand.Next())
        else
            if c < p
            then
                let n1 = async{return go (h-2) (c + 1)}
                let n2 = async{return go (h-2) (c + 1)}
                let n3 = async{return go (h-2) (c + 1)}
                let n4 = async{return go (h-2) (c + 1)}
                let nds = [n1;n2;n3;n4] |> Async.Parallel |> Async.RunSynchronously
                Node(
                    Node (nds.[0],nds.[1]),
                    Node (nds.[2],nds.[3])
                )
            else Node (go (h-1) (c + 1), go (h-1) (c + 1))
    go h 0

let genRandomTreeParallel3 h p =
    let rand = new System.Random()
    let mutable x1 = Unchecked.defaultof<_>
    let mutable x2 = Unchecked.defaultof<_>
    let mutable x3 = Unchecked.defaultof<_>
    let mutable x4 = Unchecked.defaultof<_>
    let n1 = async{do x1 <- genRandomTree (h-2)}
    let n2 = async{do x2 <- genRandomTree (h-2)}
    let n3 = async{do x3 <- genRandomTree (h-2)}
    let n4 = async{do x4 <- genRandomTree (h-2)}
    let nds = [n1;n2;n3;n4] |> Async.Parallel |> Async.RunSynchronously
    Node(
        Node (x1,x2),
        Node (x3,x4)
    )

let rec sumTree tree =
    match tree with
    | Leaf (i) -> i
    | Node (n1,n2) -> sumTree n1 + sumTree n2

let rec sumTreeParallel tree p =
    let rec go tree c =
        match tree with
        | Leaf (i) -> i
        | Node (n1,n2) ->
            if c < p
            then
                let s1 = async{return go n1 (c + 1)}
                let s2 = async{return go n2 (c + 1)}
                let sms = [s1;s2] |> Async.Parallel |> Async.RunSynchronously
                sms.[0] + sms.[1]
            else sumTree n1 + sumTree n2
    go tree 0
//Tree creation time = 25504.1976,
//tree traverse time seq = 3134.4781,
//tree traverse time parallel = 914.5073,
//result = -1293323206
let genRandomMatrix n =
    let rand = new System.Random()
    Array2D.init n n (fun _ _ -> rand.Next())

let mm (m1:_[,]) (m2:_[,]) =
    let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
    for i in 0 .. m1.GetLength 0 - 1 do
        for j in 0 .. m2.GetLength 1 - 1 do
            for k in 0 .. m1.GetLength 1 - 1 do
                res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
    res
//Matrix multiplication.
//Sequential: 22768.2548.
//Parallel (naive): 7907.295.
//Parallel (2 threads): 10264.7956.
//Parallel (4 threads): 8426.7346.
//Parallel (8 threads): 6145.0574

let mmParallel (m1:_[,]) (m2:_[,]) =
    let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
    [ for i in 0 .. m1.GetLength 0 - 1 ->
          async {
                  // prinfn "!!!"
                  do
                      for j in 0 .. m2.GetLength 1 - 1 do
                          for k in 0 .. m1.GetLength 1 - 1 do
                              res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
          }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    res


let mmParallel2 (m1:_[,]) (m2:_[,]) n =
    let res = Array2D.zeroCreate (m1.GetLength 0) (m2.GetLength 1)
    let chunkSize = (m1.GetLength 0 - 1) / n
    [ for p in 0 .. n - 1 ->
          async { do
                      for i in p * chunkSize ..  chunkSize * (p + 1) - 1 do
                          for j in 0 .. m2.GetLength 1 - 1 do
                              for k in 0 .. m1.GetLength 1 - 1 do
                                  res.[i, j] <- res.[i, j] + (m1.[i, k] * m2.[k, j])
          }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    res
