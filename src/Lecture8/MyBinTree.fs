module MyBinTree

type MyBinTree'<'tVal> =
    | Leaf of 'tVal
    | Node of 'tVal * MyBinTree<int, 'tVal> * MyBinTree<int, 'tVal>

and MyBinTree<'tNode, 'tLeaf> =
    | Leaf of MyBinTree'<'tLeaf>
    | Node of 'tNode * MyBinTree<'tNode, 'tLeaf> * MyBinTree<'tNode, 'tLeaf>


(*let rec fold f acc tree =
    match tree with
    | Leaf x -> f acc x
    | Node (op, l, r) -> fold f (fold f acc l) r
*)

let rec map nodeF leafF tree =
    match tree with
    | Leaf x -> Leaf (leafF x)
    | Node (v, l, r) -> Node (nodeF v, map nodeF leafF l, map nodeF leafF r)
