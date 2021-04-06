namespace TestProj2020

open MyList
open MyBinTree

module Main =
    open Lecture8
    open LongArith

    open System

    [<EntryPoint>]
    let main (argv: string array) =
        (*let lst = Cons(8, Cons(4, Cons (2, Nil)))
        let x = MyList.reduceBack (/) lst
        let y = MyList.reduce (/) lst

        let min' = MyList.foldBack min Int32.MaxValue lst
        let max' = MyList.foldBack max Int32.MinValue lst

        printfn "reduceBack = %A" x
        printfn "reduce = %A" y
        printfn "min = %A" min'
        printfn "max = %A" max'
*)
(*        let tree = Node
                       (Node
                            (Node
                                  (Leaf 1,
                                   Leaf 2),
                             Node (Leaf 3,
                                   Node
                                       (Leaf 4,
                                        Leaf 5))),
                        Node
                            (Leaf 9,
                             Leaf 10))

        let maxInTree tree = MyBinTree.fold max Int32.MinValue tree
        let minInTree tree = MyBinTree.fold min Int32.MaxValue tree

        printfn "Max in tree = %A" (maxInTree tree)
        printfn "Min in tree = %A" (minInTree tree)
*)
(*
        let tree = Node ((+), Leaf 1, Node((/), Leaf 4, Leaf 2))

        let tree2 = Node (1.0, Leaf 2, Node(3.0, Leaf 4, Leaf 5))

        printfn "Tree = %A" (MyBinTree.map ((+)1.0) ((*)3) tree2)
*)

        let myArrayMap f arr =
            let res = Array.zeroCreate (Array.length arr)
            for i in 0..Array.length arr - 1 do
                res.[i] <- f arr.[i]
            res

        let arr = [|1; 2; 3|]

        //printfn "Array = %A" arr

        Array.iteri (fun j x -> arr.[j] <- x + 1) arr

        //    printfn "Array iteri = %A" arr

        (*printfn "Array map = %A" (Array.map ((+)1) arr)

        printfn "Array map, second round = %A" (Array.map ((+)1) arr)

        printfn "My array map = %A" (myArrayMap ((+)1) arr)
        printfn "My array map, second round = %A" (myArrayMap ((+)1) arr)
*)
        let rec eval tree =
            match tree with
            | Leaf x -> x
            | Node (op, l, r) -> op (eval l) (eval r)

        // printfn "Eval = %A" (eval tree)

     (*   let r1 = LongArith.add (Pos, Singletone(9)) (Pos, Singletone(9))

        printfn "%A" r1


        let r2 = LongArith.mult (Pos, Singletone(9)) (Pos, Singletone(9))

        printfn "%A" r2


        let r2' = LongArith.add (Pos, Cons(1,Singletone(0))) (Neg, Singletone(1))

        printfn "%A" r2'

        let r3 = LongArith.mult r1 r2

        printfn "%A" r3


        let r4 = LongArith.substarct (Pos, Singletone 1) (Pos, Singletone 9)

        printfn "%A" r4

        let r5 = LongArith.substarct (Pos, Singletone 9) (Pos, Singletone 1)

        printfn "%A" r5


        let r5 = LongArith.substarct (Pos, Cons(9,Singletone 9)) (Pos, Singletone 2)

        printfn "%A" r5

        let r5 = LongArith.substarct (Pos, Singletone 2) (Pos, Cons(9,Singletone 9))

        printfn "%A" r5

        let r5 = LongArith.substarct (Neg, Singletone 2) (Pos, Cons(9,Singletone 9))

        printfn "%A" r5
*)
        let r2 = LongArith.mult (Pos, Cons(3,Singletone 4)) (Pos, Singletone(3))

        printfn "%A" r2

       (* let r7 = LongArith.fromInt64 -20L
        printfn "%A" r7

        let r8 = LongArith.toInt64 (Neg, Cons(2,Singletone 2))
        printfn "%A" r8
*)
        0
