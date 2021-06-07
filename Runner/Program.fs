open System
open FPP1.TreeManager
open PostScriptGenerator.Generator
   
let n1 = Node (1, [])
let n2 = Node (2, [Node (4, []); Node (6, []); Node (8, [])])
let n3 = Node (5, [n1; n2; n1])

[<EntryPoint>]
let main argv =
    let postree = design n3
    let result = toPSslow postree
    printf "%s" result
    0
