module Tests

open System
open Xunit
open FsCheck
open FPP1.TreeManager

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
let ``Property 1 Fail`` () = // Checks distances between subtrees
    let rec checkDistances x (acc:list<float*Tree<'a*float>>) (ps:list<float*Tree<'a*float>>) =
        match acc,ps with
        | [],[] -> true
        | (os,Node((_,x'),subtrees))::ps',[] -> checkDistances (os+x') (List.map (fun e-> (os+x',e)) subtrees) ps'
        | _,(os,Node((_,x'),subtrees))::ps' when x+1.<= os+x'-> checkDistances (os+x') (acc@(List.map (fun e-> (os+x',e)) subtrees)) ps'
        | _,_                                        -> false
    let prop1_fail = Node(("A",0.0),[Node(("B",-0.2),[]);Node(("C",0.2),[])])
    let checkDistances' = not (checkDistances 0. [(0.0, prop1_fail)] [])
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
let ``Property 2 Fail`` () = // Tests centering with correct distances
    let rec checkCentering (trees:list<Tree<'a*float>>) =
        match trees with
        | []                  -> true
        | Node(_,[])::t       -> checkCentering t
        | Node(_,subtrees)::t -> 
            let (Node((_,x),_)) = List.head subtrees
            let (Node((_,y),_)) = List.last subtrees
            if x+y = 0. then checkCentering (t@subtrees) else false
    let prop2_fail = Node(("A",0.0),[Node(("B",-0.3),[]);Node(("C",0.7),[])])
    let checkProperty = not (checkCentering [prop2_fail])
    Check.QuickThrowOnFailure checkProperty

[<Fact>]
let ``Property 3`` () =
    let rec reflect (Node(v, subtrees)) = Node(v, List.map reflect (List.rev subtrees))
    let rec reflectpos (Node((v,x),subtrees)) = Node((v,-1.*x), List.map reflectpos subtrees)
    let checkProperty (t:Tree<int>) = design t = reflect(reflectpos(design (reflect t)))
    Check.QuickThrowOnFailure checkProperty

[<Fact>]
let ``Property 3 Fail`` () = // Tests failing scenario from paper
    let rec reflect (Node(v, subtrees)) = Node(v, List.map reflect (List.rev subtrees))
    let rec reflectpos (Node((v,x),subtrees)) = Node((v,-1.*x), List.map reflectpos subtrees)
    let n0 = Node(("C",-1.),[])
    let n1 = Node(("D",0.),[])
    let n2 = Node(("E",1.),[])
    let n3 = Node(("H",-1.),[])
    let n4 = Node(("I",0.),[])
    let n5 = Node(("J",1.),[])
    let n6 = Node(("B",-1.5),[n0;n1;n2])
    let n7 = Node(("G",1.5),[n3;n4;n5])
    let n8 = Node(("F",-0.25),[])
    let n9 = Node(("A",0.),[n6;n8;n7])
    let checkProperty = not(n9 = reflect(reflectpos(reflect n9)))
    Check.QuickThrowOnFailure checkProperty

[<Fact>]
let ``Property 4`` () =
    let rec collect (Node(x,subtrees)) = [Node(x,subtrees)]@(List.collect collect subtrees)
    let rec checkSimilar e l =
        match e,l with
        | e,[] -> true
        | e,h::t -> (similar e h) = (same e h) && checkSimilar e t
    let rec checkSimilar' = function
        | [] -> true
        | h::t -> checkSimilar h t && checkSimilar' t
    let checkProperty (t:Tree<int>) = 
        let posTree = design t
        let nodes = collect posTree
        checkSimilar' nodes
    Check.QuickThrowOnFailure checkProperty

[<Fact>]
let ``Property 4 Fail`` () = // Tests failing scenario from paper
    let rec collect (Node(x,subtrees)) = [Node(x,subtrees)]@(List.collect collect subtrees)
    let rec checkSimilar e l =
        match e,l with
        | e,[] -> true
        | e,h::t -> (similar e h) = (same e h) && checkSimilar e t
    let rec checkSimilar' = function
        | [] -> true
        | h::t -> checkSimilar h t && checkSimilar' t
    let checkProperty = 
        let n0 = Node(("C",-1.),[])
        let n1 = Node(("D",0.),[])
        let n2 = Node(("E",1.),[])
        let n3 = Node(("H",-2.),[])
        let n4 = Node(("I",0.),[])
        let n5 = Node(("J",2.),[])
        let n6 = Node(("B",-1.),[n0;n1;n2])
        let n7 = Node(("G",0.),[n3;n4;n5])
        let n8 = Node(("F",1.),[n7])
        let n9 = Node(("A",0.),[n6;n8])
        let nodes = collect n9
        not (checkSimilar' nodes) // Negation as we expect to fail
    Check.QuickThrowOnFailure checkProperty