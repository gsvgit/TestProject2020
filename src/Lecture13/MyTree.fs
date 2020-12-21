module Lecture13.MyTree

type IActor<'data,'res> =
    abstract member DoAction : 'data -> 'res

type INode<'t> =
    abstract member Data: 't

type BinNode<'t>(data, leftCh:INode<'t>, rightCh:INode<'t>) =
    interface INode<'t> with
        member this.Data = data
    member this.LeftChild = leftCh
    member this.RightChild = rightCh

type TriNode<'t>(data, leftCh:INode<'t>, midCh:INode<'t>, rightCh:INode<'t>) =
    interface INode<'t> with
        member this.Data = data
    member this.LeftChild = leftCh
    member this.RightChild = rightCh
    member this.MIddleChild = midCh

type Leaf<'t>(data) =
    interface INode<'t> with
        member this.Data = data

type Visitor<'nodeData,'res>(actor:IActor<'nodeData,'res>) =
    member this.Visit(root:INode<'nodeData>) =
        match root with
        | :? BinNode<_> as n -> this.VisitNode(n)
        | :? Leaf<_> as l -> this.VisitLeaf(l)
        | x -> failwithf "Unexpected type of node: %A" x

    member private this.VisitNode(n:BinNode<'nodeData>) =
            let newAcc = actor.DoAction((n :> INode<_>).Data)
            let l = this.Visit(n.LeftChild)
            this.Visit(n.RightChild)

    member private this.VisitLeaf(n:Leaf<'nodeData>) =
            actor.DoAction((n :> INode<_>).Data)
