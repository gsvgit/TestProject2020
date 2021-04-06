module TestProj2020.Tests.FSharpIntreTests

open TestProj2020.FSharpIntro
open Expecto
open Lecture8


[<Tests>]
let powTest =
     testList "Pow function tests"
        [
            (*testProperty "Zero power of any number should be one"
            <| fun (i:int) -> Expect.equal 1 (pow (i*1<Base>) 0<Pow>) "It is strange that zero power of zero is not one."

            testCase "One power of zeo should be zero"
            <| fun _ -> Expect.equal 0 (pow 0<Base> 1<Pow>) "It is strange that one power of zero is not zero."

            testCase "Example to fail"
            <| fun _ -> Expect.equal 4 (pow 2<Base> 2<Pow>) "2^2 =?= 4"
*)
            testProperty "fromInt64 toInt64 is id"
            <| fun (i:int64) -> Expect.equal i (LongArith.fromInt64 i |> LongArith.toInt64     ) ""
            testProperty "Long mult"
            <| fun (i:int32,j:int32) -> Expect.equal
                                            (int64 (i * j))
                                            (LongArith.mult (LongArith.fromInt64 (int64 i)) (LongArith.fromInt64 (int64 j)) |> LongArith.toInt64) ""
            testProperty "Long add"
            <| fun (i:int32,j:int32) -> Expect.equal
                                            (int64 (i + j))
                                            (LongArith.add (LongArith.fromInt64 (int64 i)) (LongArith.fromInt64 (int64 j)) |> LongArith.toInt64) ""
        ]
