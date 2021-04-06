module TestProj2020.Tests.Lecture4Tests

open TestProj2020.Sorts
open Expecto

[<Tests>]
let sortTest =
     testList "List sort tests"
        [
           (* testProperty "My sort is like system sort"
            <| fun (lst:List<int>) -> Expect.sequenceEqual (List.sort lst) (qSort lst) "My sort should be equals to system sort."

            testProperty "Dummy test on array"
            <| fun (arr:array<int>) -> Expect.sequenceEqual arr arr "Arrays are equal as sequences."

            testProperty "Dummy test on tuples"
            <| fun (x,y,z,s) -> Expect.notEqual 0 (x*x + y*y + z*z + 1) "Sums of squares should be nonnegative."*)
        ]

[<Tests>]
let readedTest =
     testList "Array reader tests"
        [
            (*testCase "Check length of array"
            <| fun _ -> Expect.sequenceEqual [|1 ;2; 3; 4|] (readArray "TestArray1.txt") "Length of readed array should be 4."*)
        ]
