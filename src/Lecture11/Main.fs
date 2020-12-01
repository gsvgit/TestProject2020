module TestProj2020.Main

open Argu

type CLIArguments =
    | Input of file:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "File to interpret."

open System.Collections.Generic
open Automata
open Matrices
open Regexp
open RegexpParser

let processRegexp regexp input =
    let nfa = regexpToNFA regexp

    let res, time = Lecture5.PerfTests.time (fun _ -> recognizeNFA nfa input)

    printfn "Recognition with graph NFA: %A in %A milliseconds" res time

    let mtxNFA = nfaToMatrixNFA nfa
    let eclsNFA = epsClosure mtxNFA
    let res, time = Lecture5.PerfTests.time (fun _ -> accept eclsNFA input)
    printfn "Recognition with matrix NFA: %A in %A milliseconds" res time
    printfn "------------------------"


[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "RegExp interpreter")
    let results = parser.Parse(argv)
    if results.Contains Input
    then
        let file = results.GetResult Input
        let ast = RegexpParser.parseRegexpFromString (System.IO.File.ReadAllText file)
        Interpreter.run ast
    else
        parser.PrintUsage() |> printfn "%s"
    0
