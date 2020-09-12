module TestProj2020.FSharpIntro

let x = 1 + 1

let y = 'a'

let z = "bool" + "string"

let c = true || false

let b = 1.0 * 10.0

let f x = x + 1

let rec pow x n =
    if n = 0 then 1 else x * pow x (n-1)

let forDemo n =
    for i = 0 to n do printfn "%A" n
    for j = pow 2 n downto -1 do printfn "%A" n

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
    else
        printfn "Papameter n should not be less then 0. Actual value: %A" n

