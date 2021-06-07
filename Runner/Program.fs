open System
open FPP1.TreeManager
open PostScriptGenerator.Generator
   

let n0 = Node ('A', [Node ('B', [Node ('C', []); Node ('D', []); Node ('E', [])]); Node ('F', [Node ('G', []); Node ('H', []); Node ('I', [])]); Node ('J', [Node ('K', []); Node ('L', []); Node ('M', [])])])

[<EntryPoint>]
let main argv =
    let postree = design n0
    let result = toPSslow postree
    printf "%s" result
    0
