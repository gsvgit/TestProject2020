module TestProj2020.Sorts

let qSort lst =

    let split opCompare lst =
        let rec go lst part1 part2 =
            match lst with
            | [] -> part1, part2
            | hd :: tl ->
                if opCompare hd
                then go tl (hd :: part1) part2
                else go tl part1 (hd :: part2)
        go lst [] []

    let rec go lst =
        match lst with
        | [] -> []
        | [x] -> [x]
        | hd :: tl ->
            let left,right = split ((>) hd) tl
            (go left) @ hd :: (go right)

    go lst


let readArray file =
    let a = System.IO.File.ReadAllLines file
    let intArr = Array.zeroCreate a.Length
    let mutable j = 0
    for i in a do
        intArr.[j] <- int (i.Trim())
        j <- j + 1
    intArr

let f x y = x ||| y
