module Tests

open System
open Xunit
open FsCheck
open FPP1.TreeManager

//[<Fact>]
//let ``Property 1`` () =
//    let rec checkDistances offset poslist tree =
//        match poslist,tree with
//        |   [], Node((_,x'),subtrees) -> 
//        |   h::t,Node((_,x'),subtrees) when h+1.<= offset + x' -> checkChildren offset+x' t subtrees
//
//    and checkChildren offset poslist treelist =
//        match treelist with
//        | [] -> true
//        | h::t -> checkDistances offset poslist h && checkChildren offset poslist t


[<Fact>]
let ``Property 1`` () =
    let rec checkDistances x (acc:list<float*Tree<'a*float>>) (ps:list<float*Tree<'a*float>>) =
        match acc,ps with
        | [],[] -> true
        | (os,Node((_,x'),subtrees))::ps',[] -> checkDistances (os+x') (List.map (fun e-> (os+x',e)) subtrees) ps'
        | _,(os,Node((_,x'),subtrees))::ps' when x+1.<= os+x'-> checkDistances (os+x') (acc@(List.map (fun e-> (os+x',e)) subtrees)) ps'
        | _,_                                        -> false
    let checkDistances' (t:Tree<int>) = checkDistances 0. [(0.0, design t)] []
    Check.QuickThrowOnFailure checkDistances'

[<Fact>]
let ``Property 2`` () = 
    let rec checkCentering (trees:list<Tree<'a*float>>) =
        match trees with
        | []                  -> true
        | Node(_,[])::t       -> checkCentering t
        | Node(_,subtrees)::t -> 
            let (Node((_,x),_)) = List.head subtrees
            let (Node((_,y),_)) = List.last subtrees
            if x+y = 0. then checkCentering (t@subtrees) else false
    let checkProperty (t:Tree<int>) = checkCentering [design t]
    Check.QuickThrowOnFailure checkProperty

[<Fact>]
let ``Property 3`` () =
    let rec flip t =
        match t with
        | Node(x,[]) -> t
        | Node(x,st) -> Node(x,flip' st)
    and flip' l =
        match l with 
        | [] -> []
        | h::t -> (flip' t)@([flip h])
    let rec checkSymmetry = function
        | ([],[]) -> true
        | ([],_) -> false
        | (_,[]) -> false
        | (Node((xl,x),xst)::xtail, Node((yl,y),yst)::ytail) when x=(-y) && xl = yl -> checkSymmetry(xtail@xst,ytail@(List.rev yst))
        | _ -> false
    let checkProperty (t:Tree<int>) =
        let flipped = flip t
        checkSymmetry ([design t],[design flipped])
    Check.QuickThrowOnFailure checkProperty
//Node (0, [Node (0, [Node (0, [])]); Node (0, [Node (0, [])])])
