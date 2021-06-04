open FPP1.TreeManager
    
   
let n1 = Node (1, [])
let n2 = Node (2, [Node (4, []); Node (6, []); Node (8, [])])
let n3 = Node (5, [n1; n2; n1])

let p1 = Node ((1, 10.0), [])
let p2 = Node ((2, 20.0), [Node ((4, 40.0), []); Node ((6, 60.0), []); Node ((8, 80.0), [])])
let p3 = Node ((5, 50.0), [p1; p2; p1])



[<EntryPoint>]
let main argv =
    let x1 = moveextent ([(10.0, 5.0); (20.0, 23.0); (1.0, 34.0)], 2.0)
    0
    