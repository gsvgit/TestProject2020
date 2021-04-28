module Lecture10.Interfaces

type IMatrix<'elem> =
    abstract Item: int * int -> 'elem with get, set
    abstract Size: int * int with get

type MyMatrix<'elem> (n,m) =
    let store = Array2D.zeroCreate n m
    interface IMatrix<'elem> with
        member this.Item
            with get (i, j) = store.[i, j]
            and set (i, j) v = store.[i, j] <- v

        member this.Size = n,m

type ISmb<'t> =
    abstract PrettyString: string

type Smb<'t> =
    | Eps
    | Smb of 't
    interface ISmb<'t> with
        member this.PrettyString =
            match this with
            | Eps -> "Eps"
            | Smb t -> t.ToString()

[<AbstractClass>]
type FA<'smb> () =
    abstract StartState: int with get//, set
    abstract FinalState: int with get//, set
    abstract TransitionsList : array<int * int * ISmb<'smb>>
    member this.ToDot (outFile) =
        let header =
            [
                "digraph nfa"
                "{"
                "rankdir = LR"
                "node [shape = circle];"
                sprintf "%A[shape = circle, label = \"%A_Start\"]" this.StartState this.StartState
            ]

        let footer =
            [
                sprintf "%A[shape = doublecircle]" this.FinalState
                "}"
            ]

        let content =
            [
                for (s,f,t) in this.TransitionsList do
                    sprintf
                        "%A -> %A [label = \"%s\"]"
                        s
                        f
                        t.PrettyString
            ]

        System.IO.File.WriteAllLines (outFile, header @ content @ footer)


type MyDFA<'smb> (transitions, startSate, finalState) =
    inherit FA<'smb>()
    override this.StartState = startSate
    override this.FinalState = finalState
    override this.TransitionsList =
        let n,m = this.Transitions.Size
        [|
         for i in 0 .. n - 1 do
            for j in 0 .. m - 1 do
                yield i, j, this.Transitions.[i,j]
        |]

    member this.Transitions: IMatrix<ISmb<'smb>> = transitions



let m = new MyMatrix<_>(2,3) :> IMatrix<_>
m.[1,2] <- Smb(0) :> ISmb<_>

let fa = new MyDFA<_>(m,1,0)
fa.ToDot "file"
