module Lecture8.LongArith

type NonEmptyList<'t> =
    | Singletone of 't
    | Cons of 't * NonEmptyList<'t>

type Sign = Pos | Neg

type Number = Sign * NonEmptyList<int>

let rec fold f acc l =
    match l with
    | Singletone x -> f acc x
    | Cons (x,tl) -> fold f (f acc x) tl

let rev l =
    match l with
    | Singletone x -> l
    | Cons (x,tl) -> fold (fun acc x -> Cons(x, acc)) (Singletone x) tl

let rec map f l =
    match l with
    | Singletone x -> Singletone (f x)
    | Cons(x,tl) -> Cons(f x, map f tl)

let init l v =
    if l = 0
    then None
    else
        let rec _go l v =
            if l = 1
            then Singletone v
            else Cons(v, _go (l - 1) v)
        Some (_go l v)

let rec concat l1 l2 =
    match l1 with
    | Singletone x -> Cons (x,l2)
    | Cons (x,tl) -> Cons (x, concat tl l2)

let mapi f l =
    let rec _go l i =
        match l with
        | Singletone x -> Singletone (f i x)
        | Cons(x,tl) -> Cons(f i x, _go tl (i + 1))
    _go l 0

let length l = fold (fun acc _ -> acc + 1) 0 l

let rec foldBack2 f acc l1 l2 =
    if length l1 = length l2
    then
        match l1,l2 with
        | Singletone x, Singletone y -> f acc x y
        | Cons(x, tl1), Cons(y, tl2) -> f (foldBack2 f acc tl1 tl2) x y
    else failwithf "Lists should have equal lengths. Actually have: l1: %A , l2: %A" (length l1) (length l2)

let rec trim l =
    match l with
    | Cons(0, tl) -> trim tl
    | _ -> l

let alignRight l1 l2 v =

    let rec fill l cnt v =
        if cnt = 0
        then l
        else fill (Cons(v,l)) (cnt-1) v

    let lngth1 = length l1
    let lngth2 = length l2
    if lngth1 = lngth2
    then l1,l2
    elif lngth1 > lngth2
    then l1, (fill l2 (lngth1 - lngth2) v)
    else (fill l1 (lngth2 - lngth1) v), l2

let doSimpleOp l1 l2 op =
    let d,v =
        foldBack2 (fun (d,l) x y ->
            let v1 = (op x y) + d
            let d = v1 / 10
            let v = v1 % 10
            printfn "+ %A %A %A" v1 d v
            match l with
            | None -> d, Some (Singletone v)
            | Some y -> d, Some (Cons(v,y))) (0,None) l1 l2
    let d,v = d, v.Value //trim
    printfn "! %A %A" d v
    let v =
        if d = 0
        then
            let v = trim v
            match v with
            | Singletone x when x < 0 -> Neg, Singletone (-1 * x)
            | Cons(x,y) when x < 0 -> Neg, Cons (-1 * x, y)
            | _ -> Pos,v
        elif d > 0
        then Pos,Cons(d, v)
        else Neg,Cons(abs d, v)
    v

let gtAbs n1 n2 =
    let l1,l2 = alignRight (snd n1) (snd n2) 0
    foldBack2 (fun acc x y -> acc || x > y) false (rev l1) (rev l2)

let rec add n1 n2 =
    match n1,n2 with
    | (Pos, l1), (Pos,l2)
    | (Neg, l1), (Neg, l2) ->
        let l1,l2 = alignRight l1 l2 0
        let v = doSimpleOp l1 l2 (+)
        (fst n1, snd v)
    | (Pos, l1), (Neg, l2) -> substarct n1 (Pos, l2)
    | (Neg, l1), (Pos, l2) -> substarct n2 (Pos, l1)

and substarct n1 n2 =
    match n1,n2 with
    | (Pos, l1), (Pos, l2) ->
        let gt = gtAbs n1 n2
        let l1,l2 = if gt then alignRight l2 l1 0 else alignRight l1 l2 0
        let v = doSimpleOp l1 l2 (fun y x -> if x < y then -10 - (10 + x - y) else x - y)
        if gt then v else (Neg, snd v)
    | (Pos, l1), (Neg, l2)
    | (Neg, l1), (Neg, l2) -> add n1 (Pos, l2 )
    | (Neg, l1), (Pos, l2) -> (Neg, snd (add (Pos, l1) n2))


let mult n1 n2 =
    let finalSign,l1,l2 =
        match n1,n2 with
        | (Pos, l1), (Pos, l2)
        | (Neg, l1), (Neg, l2) -> Pos, l1, l2
        | (Pos, l1), (Neg, l2)
        | (Neg, l1), (Pos, l2) -> Neg, l1 ,l2

    let v =
        let handleDigit d =
            let l1,l2 = alignRight l1 (Singletone d) d
            let v = doSimpleOp l1 l2 (*)
            v

        map handleDigit l2
        |> rev
        |> mapi (fun i x -> match init i 0 with None -> snd x | Some y -> concat (snd x) y)
        |> map (fun x -> Pos,x)
        |> fold add (Pos, Singletone 0)

    finalSign, trim (snd v)

let toInt64 n =
    if length (snd n) > 18
    then failwith "Number is tool ong."
    else
        let l = length (snd n)
        let r =
            mapi (fun i (x:int) -> (int64 x) * pown 10L (l - i - 1)) (snd n)
            |> fold (fun acc x -> acc + x) 0L
        if fst n = Neg then -1L * r else r

let fromInt64 n =
    let r =
        let x = (string (abs n)).ToCharArray()
        Array.foldBack (fun x acc ->
                           match acc with
                           | None -> Some (Singletone (int (string x)))
                           | Some y -> Some(Cons (int (string x),y)))
                       x
                       None
    (if n < 0L then Neg else Pos),r.Value
