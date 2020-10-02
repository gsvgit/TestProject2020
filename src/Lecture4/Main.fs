namespace TestProj2020

module Main =
    open Argu

    type CLIArguments =
        | TaskPow of int*int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | TaskPow _ -> "Run pow function"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "TestProj2020")
        let results = parser.Parse(argv)
        if results.Contains TaskPow
        then
            ()
            //let x = results.GetResult TaskPow
            //printfn "%A" (FSharpIntro.pow ((fst x) * 1<FSharpIntro.Base>) ((snd x) * 1<FSharpIntro.Pow>)) //(TestProj2020.Fib.fibMatrix (int n))
        else
            parser.PrintUsage() |> printfn "%s"
        0
