module TestProj2020.Tests.FSharpIntreTests

open TestProj2020.FSharpIntro
open Expecto


[<Tests>]
let powTest =
     testList "Pow function tests"
        [
            testProperty "Zero power of any number should be one"
            <| fun (i:int) -> Expect.equal 1 (pow (i*1<Base>) 0<Pow>) "It is strange that zero power of zero is not one."

            testCase "One power of zeo should be zero"
            <| fun _ -> Expect.equal 0 (pow 0<Base> 1<Pow>) "It is strange that one power of zero is not zero."

            testCase "Example to fail"
            <| fun _ -> Expect.equal 4 (pow 2<Base> 2<Pow>) "2^2 =?= 4"
        ]
