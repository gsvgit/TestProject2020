//open Automata

module Automata
open System.Collections.Generic
open Automata
open Matrices

[<Struct>]
type MatrixNFA<'t> =
    val StartState : int
    val FinalState : int
    val Transitions : HashSet<NFASmb<'t>>[,]
    new (start, final, transitions) =
        {StartState = start; FinalState = final; Transitions = transitions}


let nfaToMatrixNFA (nfa:NFA<_>) =
    let mtx =
        let maxState = nfa.Transitions |> List.fold (fun a (s,_,f) -> max (max s a) f) 0
        let mtx = Array2D.init (maxState + 1) (maxState + 1) (fun _ _ -> new HashSet<_>())
        nfa.Transitions
        |> List.iter (fun (s,l,f) -> mtx.[s,f].Add l |> ignore)
        mtx
    new MatrixNFA<_> (nfa.StartState, nfa.FinalState, mtx)

let seqToAtm (input: list<_>) =
    let mtx =
        let mtx = Array2D.init (input.Length + 1) (input.Length + 1) (fun _ _ -> new HashSet<_>())
        for i in 0 .. input.Length - 1 do
            mtx.[i, i].Add (Eps) |> ignore
            mtx.[i, i + 1].Add (Smb (input.[i])) |> ignore
        mtx.[input.Length, input.Length].Add (Eps) |> ignore
        mtx
    new MatrixNFA<_>(0, input.Length, mtx)

let toDot (nfa:MatrixNFA<_>) outFile =
    let header =
        [
            "digraph nfa"
            "{"
            "rankdir = LR"
            "node [shape = circle];"
            sprintf "%A[shape = circle, label = \"%A_Start\"]" nfa.StartState nfa.StartState
        ]

    let footer =
        [
            sprintf "%A[shape = doublecircle]" nfa.FinalState
            "}"
        ]

    let content =
        nfa.Transitions
        |> Array2D.mapi (
            fun s f t ->
                t
                |> Seq.map (fun t ->
                    sprintf
                        "%A -> %A [label = \"%s\"]"
                        s
                        f
                        (match t with Eps -> "Eps" | Smb t -> sprintf "%A" t)))
        |> fun a ->  a |> Seq.cast<_> |> Seq.collect id |> Seq.toList

    System.IO.File.WriteAllLines (outFile, header @ content @ footer)


let accept (nfa:MatrixNFA<_>) (input: list<_>) =
    let nfa2 = seqToAtm input
    let intersection = kron nfa2.Transitions nfa.Transitions (fun s1 s2 -> let res = new HashSet<_>(s1) in res.IntersectWith s2; res)

    let newStartState = nfa2.StartState * (nfa.Transitions.GetLength 0) + nfa.StartState
    let newFinalState = nfa2.FinalState * (nfa.Transitions.GetLength 0) + nfa.FinalState

    toDot nfa2 "nfa2.dot"
    toDot (new MatrixNFA<_>(newStartState, newFinalState, intersection)) "outIntersection.dot"

    let projected = intersection |> Array2D.map (fun s -> s.Count > 0)

    let reachability =
        let count (r:_[,]) =
            let mutable cnt = 0
            Array2D.iter (fun i -> if i then cnt <- cnt + 1) r
            cnt
        let res = projected
        let mutable _continue = true
        while _continue do
            let prev = count res
            let r = multiply res res (&&) (||)
            elementwiseAddInPlace res r (||)
            let cur = count res
            if prev = cur
            then _continue <- false

        res

    reachability.[newStartState, newFinalState]

