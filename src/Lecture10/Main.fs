namespace TestProj2020

open Automata

module Main =
    open Regexp

    [<EntryPoint>]
    let main (argv: string array) =

  (*      let transitions = [(0,'0',1); (1,'1',1) ; (1,'0',1)]
        let atm = new DFA<_>(0, [1], transitions)
        recognizeDFA atm ['1'; '1'; '1'; '0'; '1']
        |> printfn "Recognition with DFA result: %A"
*)
        let nfa =
            Star (Alt (RSmb '1', RSmb '0'))
            |> regexpToNFA

        nfaToDot "simpleNFA.dot" nfa

        recognizeNFA nfa ['1'; '1'; '1']
        |> printfn "Recognition with NFA result: %A"

        let nfa2 =
            Seq(
                Alt (Alt (Alt (RSmb '1', RSmb '2'), RSmb '3'), RSmb '4'),
                Star (Alt (Alt (Alt (Alt (RSmb '1', RSmb '2'), RSmb '3'), RSmb '4'), RSmb '0'))
            )
            |> regexpToNFA

        nfaToDot "NFA2.dot" nfa2

        recognizeNFA nfa2 ['0'; '2'; '3']
        |> printfn "Recognition with NFA result: %A"


        0
