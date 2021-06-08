open System
open FPP1.TreeManager
open PostScriptGenerator.Generator
   

let n1 = Node ("NODE1", []);
let n2 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", []); Node ("NODE4", [])]);
let n3 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", []); Node ("NODE5", [])]); Node ("NODE3", [Node ("NODE6", []); Node ("NODE7", [])])]);
let n4 = Node ("NODE1", [Node ("NODE2", [Node ("NODE5", []); Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE3", [Node ("NODE8", []); Node ("NODE9", []); Node ("NODE10", [])]); Node ("NODE4", [Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])])
let n5 = Node ("NODE1", [Node ("NODE2", [Node ("NODE6", []); Node ("NODE7", []); Node ("NODE8", []); Node ("NODE9", [Node ("NODE10", []); Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])]); Node ("NODE3", []); Node ("NODE4", []); Node ("NODE5", [])]);
let n6 = Node ("NODE1", [Node ("NODE2", [Node ("NODE3", [Node ("NODE4", [Node ("NODE5", []);]);]);]);]);
let n7 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", [Node ("NODE4", []); Node ("NODE5", [Node ("NODE6", []); Node ("NODE7", [])])])]);
let n8 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", [Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE5", [])]); Node ("NODE3", [])]);
let fact = Node ("Block", [Node ("VarDec", [Node ("n", []); Node ("IntTyp", []); Node ("Int 4", [])]); Node ("VarDec", [Node ("y", []); Node ("IntTyp", []); Node ("Int 1", [])]); Node ("Seq", [Node ("While", [Node ("ApplyPrim", [Node ("<>", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 0", [])]); Node ("Seq", [Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"y\"", []); Node ("ApplyPrim", [Node ("*", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"n\"", []); Node ("ApplyPrim", [Node ("-", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 1", [])])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])])])]);

[<EntryPoint>]
let main argv =
    let postree = design fact

    let result = toPSfast postree
    printf "%s" result
    0