open System
open FPP1.TreeManager
open PostScriptGenerator.Generator
   
let n1 = Node (1, [])
let n2 = Node (2, [Node (4, []); Node (6, []); Node (8, [])])
let n3 = Node (5, [n1; n2; n1])

let p1 = Node ((1, 10.0), [])
let p2 = Node ((2, 20.0), [Node ((4, 40.0), []); Node ((6, 60.0), []); Node ((8, 80.0), [])])
let p3 = Node ((5, 50.0), [p1; p2; p1])


[<EntryPoint>]
let main argv =
    let t = design n3
    let s = ps t
    printf "%s" s
    0 // return an integer exit code
