module Tests

open System
open Xunit
open FsCheck
open FPP1.TreeManager

[<Fact>]
let ``My test`` () =
    let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
    Check.QuickThrowOnFailure revRevIsOrig

[<Fact>]
let ``My test 2`` () =
    let revRevIsOrig (xs:list<int>) =  List.rev xs = xs
    Check.QuickThrowOnFailure revRevIsOrig