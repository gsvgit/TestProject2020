module TestProj2020.Main

open Argu
open Lecture13.MyTree

type Sumator() =
    let mutable state = 0
    interface IActor<int,int> with
        member this.DoAction(d) =
            state <- state + d
            state

[<EntryPoint>]
let main (argv: string array) =
    let tree =
        new BinNode<_>(3,
                    new BinNode<_>(2,
                                new BinNode<_>(4,
                                            new Leaf<_>(5),
                                            new Leaf<_>(6)),
                                new Leaf<_>(3)),
                    new TriNode<_>(7,
                                new Leaf<_>(2),
                                new Leaf<_>(4),
                                new Leaf<_>(4)))

    let summator = new Sumator()
    let visitor = new Visitor<_,_>(summator :> IActor<_,_>)
    let res = visitor.Visit tree
    printfn "Sum = %A" res
    0
