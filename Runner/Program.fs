// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FPP1.TreeManager
open PostScriptTranslator.Translator

let n1 = Node(1, [])
let n2 = Node(2, [])
let n3 = Node(3, [])
let n4 = Node(4, [])
let n5 = Node(5, [])

let test1 = Node(2, [ Node(5, []) ])

[<EntryPoint>]
let main argv =
    let t = design test1
    let s = ps t
    printf "%s" s
    0 // return an integer exit code
