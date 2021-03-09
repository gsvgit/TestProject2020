module Interpreter
open System.Collections.Generic

let private newDataToConsole = new Event<string>()

let Printed = newDataToConsole.Publish

let private runtimeException = new Event<string>()

let NewRuntimeException = runtimeException.Publish


type vType =
    | RE of Regexp.Regexp<char>
    | Bool of bool
    | Lst of list<int*int>

let rec processRegExp (vDict:Dictionary<_,_>) re =
    match re with
    | AST.RVar v ->
        let data =
            try
                vDict.[v]
            with
            | _ ->
                sprintf "Variable %A is not declared." v |> runtimeException.Trigger
                failwith "Variable %A is not declared." v
        match data with
        | RE r -> r
        | Bool _ ->
            sprintf "Variable %A has type bool, but regexp is expected." v |> runtimeException.Trigger
            failwithf "Variable %A has type bool, but regexp is expected." v
        | Lst _ ->
            sprintf "Variable %A has type list, but regexp is expected." v |> runtimeException.Trigger
            failwithf "Variable %A has type list, but regexp is expected." v
    | AST.RSmb s -> Regexp.RSmb s
    | AST.Alt(l, r) ->
        let l = processRegExp vDict l
        let r = processRegExp vDict r
        Regexp.Alt(l, r)
    | AST.Seq(l, r) ->
        let l = processRegExp vDict l
        let r = processRegExp vDict r
        Regexp.Seq(l, r)
    | AST.Star r ->
        let r = processRegExp vDict r
        Regexp.Star r
    | AST.Opt r ->
        let r = processRegExp vDict r
        Regexp.Alt(Regexp.REps, r)
    | AST.Intersect (l, r) ->
        let l = processRegExp vDict l
        let r = processRegExp vDict r
        Regexp.Intersect(l, r)

let processExpr vDict expr =
    let makeAtm regexp =
        let regexp = processRegExp vDict regexp
        let nfa = Regexp.regexpToNFA regexp
        let mtxNFA = Automata.nfaToMatrixNFA nfa
        Automata.epsClosure mtxNFA
    match expr with
    | AST.FindAll (str, re) ->
        Lst(Automata.findAll (makeAtm re) (str.ToCharArray() |> List.ofArray))
    | AST.IsAcceptable (str, re) ->
        Bool(Automata.accept (makeAtm re) (str.ToCharArray() |> List.ofArray))
    | AST.RegExp re ->
        RE (processRegExp vDict re)

let processStmt (vDict:Dictionary<_,_>) stmt =
    match stmt with
    | AST.Print v ->
        let data =
            try
                vDict.[v]
            with
            | _ ->
                sprintf "Variable %A is not declared." v |> runtimeException.Trigger
                failwithf "Variable %A is not declared." v
        match data with
        | RE r -> sprintf "%A" r |> newDataToConsole.Trigger
        | Bool b -> sprintf "%A" b |> newDataToConsole.Trigger
        | Lst l -> sprintf "%A" l |> newDataToConsole.Trigger
    | AST.VDecl(v,e) ->
        if vDict.ContainsKey v
        then vDict.[v] <- processExpr vDict e
        else vDict.Add(v, processExpr vDict e)
    vDict

let run ast =

    let vDict = new Dictionary<_,_>()
    ast
    |> List.fold processStmt vDict
    |> ignore
