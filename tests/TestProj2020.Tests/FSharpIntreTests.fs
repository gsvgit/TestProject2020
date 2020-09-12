module TestProj2020.Tests.FSharpIntreTests

open TestProj2020.FSharpIntro
open Expecto

[<Tests>]
let powTest =
     testList "Pow function tests"
        [
            testCase "Zero power of zeo should be one"
            <| fun _ -> Expect.equal 1 (pow 0 0) "It is strange that zero power of zero is not one."

            testCase "One power of zeo should be zero"
            <| fun _ -> Expect.equal 0 (pow 0 1) "It is strange that one power of zero is not zero."

        ]
