namespace TestProj2020

open Automata

module Main =
    open Matrices
    open Regexp
    open RegexpParser

    let processRegexp regexp input =
        let nfa = regexpToNFA regexp

        recognizeNFA nfa input
        |> printfn "Recognition with NFA result: %A"

        let mtxNFA = nfaToMatrixNFA nfa
        accept mtxNFA input
        |> printfn "Recognition with matrices: %A"


    [<EntryPoint>]
    let main (argv: string array) =

        let re1 = Star (Alt (RSmb '1', RSmb '0'))
        let input1 = ['1'; '1'; '3']
        let input2 = ['1'; '1'; '1']

        let re2 =
            Seq(
                Alt (Alt (Alt (RSmb '1', RSmb '2'), RSmb '3'), RSmb '4'),
                Star (Alt (Alt (Alt (Alt (RSmb '1', RSmb '2'), RSmb '3'), RSmb '4'), RSmb '0'))
                )

        let input3 = ['1'; '2'; '3']
        let input4 = ['0'; '2'; '3']

        let re3 =
            Seq(
                RSmb '1',
                Star (Alt (RSmb '1', RSmb '0'))
                )

        let input5 = ['1']
        let input6 = ['0'; '1']

(*
        processRegexp re3 input3
        processRegexp re3 input4
        processRegexp re3 input5
        processRegexp re3 input6

        processRegexp re2 input3
        processRegexp re2 input4

        processRegexp re1 input1
        processRegexp re1 input2
*)
        let re4 = parseRegexpFromString "(1|0)*"
(*
        processRegexp re4 input1
        processRegexp re4 input2
*)
        let numRe = parseRegexpFromString "(1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)*.(0|1|2|3|4|5|6|7|8|9)*(1|2|3|4|5|6|7|8|9)"

        printfn "%A" numRe

        processRegexp numRe ['1';'2';'.';'0';'4';'2']

        0
