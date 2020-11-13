module MyList

type MyList<'t> =
    | Nil
    | Cons of 't * MyList<'t>

let rec reduceBack f lst =
    match lst with
    | Nil | Cons (_, Nil) -> failwith "List is too short. It should contains at least two elements."
    | Cons (hd1, Cons (hd2, Nil)) -> f hd1 hd2
    | Cons (hd, tl) -> f hd (reduceBack f tl)

let reduce f lst =
    let rec _go acc lst =
        match lst with
        | Nil -> acc
        | Cons (hd, tl) -> _go (f acc hd) tl

    match lst with
    | Nil | Cons (_, Nil) -> failwith "List is too short. It should contains at least two elements."
    | Cons (hd, tl) -> _go hd tl

let rec foldBack f acc lst =
    match lst with
    | Nil -> failwith "List can not be empty."
    | Cons (hd, Nil) -> f acc hd
    | Cons (hd, tl) -> f (foldBack f acc tl) hd
