//open Automata

module Automata
open System.Collections.Generic
open System.Security.Policy
open Automata
open Matrices

[<Struct>]
type MatrixNFA<'t> =
    val StartState : int
    val FinalState : HashSet<int>
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
    new MatrixNFA<_> (nfa.StartState, new HashSet<_>([nfa.FinalState]), mtx)

let seqToAtm (input: list<_>) =
    let mtx =
        let mtx = Array2D.init (input.Length + 1) (input.Length + 1) (fun _ _ -> new HashSet<_>())
        for i in 0 .. input.Length - 1 do
            mtx.[i, i + 1].Add (Smb (input.[i])) |> ignore
        mtx
    new MatrixNFA<_>(0, new HashSet<_>([input.Length]), mtx)

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
             for s in nfa.FinalState do
                sprintf "%A[shape = doublecircle]" s
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


let epsClosure (atm:MatrixNFA<_>) =
    let inline count (r:HashSet<_>[,]) =
        let mutable cnt = 0
        r |> Array2D.iter (fun i -> if i.Count > 0 then cnt <- cnt + 1)
        cnt

    let inline addSets (s1:HashSet<_>) s2 =
        let r = if s1 = null then new HashSet<_>() else new HashSet<_>(s1)
        r.UnionWith s2
        r

    let inline multSets s1 s2 =
        let r = new HashSet<_> ()
        for x in s1 do
            for y in s2 do
                match x,y with
                | Eps, e -> r.Add e |> ignore
                | _ -> ()
        r

    let eCls = cls atm.Transitions (fun i -> i.Count > 0) multSets addSets

    let intermediateResult = new MatrixNFA<_> (atm.StartState, atm.FinalState, eCls)
    //toDot intermediateResult "inter.dot"
    let newFinals = new HashSet<_>()

    eCls |> Array2D.iteri (fun i j x -> if x.Contains Eps && atm.FinalState.Contains j then newFinals.Add i |> ignore)
    newFinals.UnionWith atm.FinalState
    eCls |> Array2D.iteri (fun i j x -> x.Remove Eps |> ignore)

    let res = new MatrixNFA<_> (atm.StartState, atm.FinalState, eCls)
    //toDot res "resECls.dot"

    let boolMtx = res.Transitions |> Array2D.map (fun x -> x.Count > 0)

    let reachable = cls boolMtx (fun i -> i) (&&) (||)

    let reachableFromStart = new HashSet<_>()

    reachable |> Array2D.iteri (fun i j x -> if x && i = atm.StartState then reachableFromStart.Add j |> ignore)

    reachableFromStart.Add atm.StartState |> ignore

    let newStateToOldState = new Dictionary<_,_>()

    reachableFromStart |> Seq.iteri (fun i x -> newStateToOldState.Add (i,x))

    let newTransitions =
        Array2D.init
            newStateToOldState.Count
            newStateToOldState.Count
            (fun i j -> eCls.[newStateToOldState.[i], newStateToOldState.[j]])

    let res =
        new MatrixNFA<_>(
            (newStateToOldState |> Seq.find (fun x -> x.Value = atm.StartState)).Key
            , newStateToOldState
              |> Seq.filter (fun x -> newFinals.Contains x.Value)
              |> Seq.map (fun kvp -> kvp.Key)
              |> fun x -> new HashSet<_>(x)
            , newTransitions)

    //toDot res "res.dot"

    res


let accept (nfa:MatrixNFA<_>) (input: list<_>) =
    let nfa2 = seqToAtm input
    let intersection = kron nfa2.Transitions nfa.Transitions (fun s1 s2 -> let res = new HashSet<_>(s1) in res.IntersectWith s2; res)

    let newStartState = nfa2.StartState * (nfa.Transitions.GetLength 0) + nfa.StartState
    let newFinalStates =
        [
            for s1 in nfa2.FinalState do
                for s2 in nfa.FinalState do
                    s1 * (nfa.Transitions.GetLength 0) + s2
        ]

   // toDot nfa2 "nfa2.dot"
   // toDot (new MatrixNFA<_>(newStartState, new HashSet<_>(newFinalStates), intersection)) "outIntersection.dot"

    let projected = intersection |> Array2D.map (fun s -> s.Count > 0)

    let reachability = cls projected (fun i -> i) (&&) (||)

    newFinalStates
    |> List.fold (fun a s -> a || reachability.[newStartState,s]) false


let acceptWithSparseMatrix (nfa:MatrixNFA<_>) (input: list<_>) =
    let nfa2 = seqToAtm input
    let intersection = kron nfa2.Transitions nfa.Transitions (fun s1 s2 -> let res = new HashSet<_>(s1) in res.IntersectWith s2; res)

    let newStartState = nfa2.StartState * (nfa.Transitions.GetLength 0) + nfa.StartState
    let newFinalStates =
        [
            for s1 in nfa2.FinalState do
                for s2 in nfa.FinalState do
                    s1 * (nfa.Transitions.GetLength 0) + s2
        ]

   // toDot nfa2 "nfa2.dot"
   // toDot (new MatrixNFA<_>(newStartState, new HashSet<_>(newFinalStates), intersection)) "outIntersection.dot"

    let projected = intersection |> Array2D.map (fun s -> s.Count > 0)

    let reachability =
        let mutable res = toBooleanSparse projected
        let mutable _continue = true
        while _continue do
            let prev = Array.length res
            let r = multBoolSparseParallel2 res res
            res <- elementwiseAddBoolSparse res r
            let cur = Array.length res
            //printfn "prev = %A cur = %A" prev cur
            if prev = cur
            then _continue <- false
        res

    Array.exists (fun (i,j) -> newStartState = i && List.contains j newFinalStates) reachability
