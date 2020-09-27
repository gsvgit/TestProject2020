module TestProj2020.FSharpIntro

let x = 1 + 1

let y = 'a'

let z = "bool" + "string"

let c = true || false

let b = 1.0 * 10.0

let f x y z = x * z + y + 1

[<Measure>] type Base
[<Measure>] type Pow

let rec pow (x: int<Base>) (n: int<Pow>) =
    if n = 0<Pow>
    then 1
    else
        (int x) * (int (pow x (n-1<Pow>)))

let p = pow 2<Base>

let forDemo n =
    for i = 0 to n do printfn "%A" i
    for j = pow 2<Base> (n*1<Pow>) downto -1 do printfn "%A" j

let arrayDemo n x y =
    if n > 0
    then
        let a = Array.zeroCreate n
        a.[0] <- 1
        a.[2] <- 4

        for i in a do
            printfn "%A" i

        let rand = new System.Random()

        let mutable x = 0
        while x < n do
            a.[x] <- rand.Next()
            x <- x + 1
    else
        printfn "Papameter n should not be less then 0. Actual value: %A" n
