module Lecture7.TypesProblems

let inline add x y = x + y

let g =
    add 1 2
    add 1.0 2.0

let g1 =
    1 + 2
    1.0 + 2.0


[<Measure>] type _2dX
[<Measure>] type _2dY

type MyList<'t> =
    | Nil
    | Cons of 't * MyList<'t>

let myList = Cons(1, Nil)

let myList2 = Cons("e", Nil)

let myListHd lst =
    match lst with
    | Nil -> failwith "Head for empty list is not defined."
    | Cons (x,_) -> x

type Coordinates =
    | I of x:int<_2dX> * y:int<_2dY>
    | F of x:float<_2dX> * y:float<_2dY>

[<Struct>]
type PointInN =
    val X: int<_2dX>
    val Y: int<_2dY>
    new (x,y) = {X = x; Y = y}

[<Struct>]
type PointInR =
    val X: float<_2dX>
    val Y: float<_2dY>
    new (x,y) = {X = x; Y = y}

type Point =
    | N of PointInN
    | R of PointInR

let newPointInN x y = new PointInN(x, y)

let points = Cons(newPointInN 1<_2dX> 2<_2dY>, Nil)

let points' = Cons(new PointInR(1.0<_2dX>, 2.0<_2dY>), Nil)

let points'' = Cons(N(new PointInN(1<_2dX>, 2<_2dY>)), Cons(R(new PointInR(1.0<_2dX>, 2.0<_2dY>)), Nil))

let points''' = Cons(F(1.0<_2dX>, 2.0<_2dY>), Cons(I(1<_2dX>, 2<_2dY>), Nil))


type Stack <'t> (lst:List<'t>) =
    let mutable l = lst
    member self.Length = lst.Length
    member self.Add x = l <- x :: lst
    member self.Get () =
        match lst with
        | hd :: tl ->
            l <- tl
            hd
        | [] -> failwith "Can not get element from the empty stack"

type Queue <'t> (lst:List<'t>) =
    let mutable l = lst
    member self.Length = lst.Length
    member self.Add x = l <- x :: lst
    member self.Get () =
        match List.rev lst with
        | hd :: tl ->
            l <- List.rev tl
            hd
        | [] -> failwith "Can not get element from the empty stack"


let inline getter' (x : ^t) =
    let getter' = (^t : (member Get : int -> int) (x, 1))
    getter'

let inline getter2 (x : ^t) =
    let res = (^t : (member Get : _ -> int) (x, ()))
    res

let inline getter (x : ^t) =
    let res = (^t : (member Get : unit -> int) (x))
    res

let inline f (x: ^a) =
    let f' = (^a : (member Speak : string -> string) (x, "world"))
    f'

let get =
    let q = new Queue<_>([1])
    let s = new Stack<_>([1])
    getter s
    getter q
