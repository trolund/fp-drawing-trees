open System
open System.Diagnostics

open FPP1.TreeManager
open AST
open PostScriptGenerator.Generator
open System.Runtime.InteropServices
open PerformanceTest.PerformanceTest
   
let n1 = Node ("NODE1", []);
let n2 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", []); Node ("NODE4", [])]);
let n3 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", []); Node ("NODE5", [])]); Node ("NODE3", [Node ("NODE6", []); Node ("NODE7", [])])]);
let n4 = Node ("NODE1", [Node ("NODE2", [Node ("NODE5", []); Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE3", [Node ("NODE8", []); Node ("NODE9", []); Node ("NODE10", [])]); Node ("NODE4", [Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])])
let n5 = Node ("NODE1", [Node ("NODE2", [Node ("NODE6", []); Node ("NODE7", []); Node ("NODE8", []); Node ("NODE9", [Node ("NODE10", []); Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])]); Node ("NODE3", []); Node ("NODE4", []); Node ("NODE5", [])]);
let n6 = Node ("NODE1", [Node ("NODE2", [Node ("NODE3", [Node ("NODE4", [Node ("NODE5", []);]);]);]);]);
let n7 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", [Node ("NODE4", []); Node ("NODE5", [Node ("NODE6", []); Node ("NODE7", [])])])]);
let n8 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", [Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE5", [])]); Node ("NODE3", [])]);
let fact = Node ("Block", [Node ("VarDec", [Node ("n", []); Node ("IntTyp", []); Node ("Int 4", [])]); Node ("VarDec", [Node ("y", []); Node ("IntTyp", []); Node ("Int 1", [])]); Node ("Seq", [Node ("While", [Node ("ApplyPrim", [Node ("<>", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 0", [])]); Node ("Seq", [Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"y\"", []); Node ("ApplyPrim", [Node ("*", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"n\"", []); Node ("ApplyPrim", [Node ("-", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 1", [])])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])])])]);

let test = Node ("NODE1", [Node ("NODE2", [Node ("NODE6", [n1; n2; n3; n7; n6; n1; n1; n1; n2; n3]); Node ("NODE7", [fact; fact; fact; fact; fact; fact; n4; n3; n1; n2; n2; n3]); Node ("NODE8", [fact; n3; n2; n1; fact]); Node ("NODE9", [Node ("NODE10", [fact; n1; n5; n6]); Node ("NODE11", [n7]); Node ("NODE12", [n2]); Node ("NODE13", [n8])])]); Node ("NODE3", []); Node ("NODE4", [n7]); Node ("NODE5", [fact; fact; fact; fact; fact; fact; fact])]);

let p = P ([VarDec (ITyp, "x")],
            [Ass (AVar "x", N 1);
              Do
                (GC
                   [(Apply ("=", [Access (AVar "x"); N 1]),
                     [PrintLn (Access (AVar "x"));
                      Ass (AVar "x", Apply ("+", [Access (AVar "x"); N 1]))]);
                    (Apply ("=", [Access (AVar "x"); N 2]),
                     [PrintLn (Access (AVar "x"));
                      Ass (AVar "x", Apply ("+", [Access (AVar "x"); N 1]))]);
                    (Apply ("=", [Access (AVar "x"); N 3]),
                     [PrintLn (Access (AVar "x"));
                      Ass (AVar "x", Apply ("+", [Access (AVar "x"); N 1]))])]);
              PrintLn (Access (AVar "x"))])


let producePDF () =     
    let file = "testProgram"
    let tree = parseProgram p
    treeToFile file tree
   
    let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows);
    let scriptFile = if isWindows then 
                        "../genPDF.bat"
                     else 
                        "../genPDF.sh";

    let procStart = ProcessStartInfo(scriptFile, file, WorkingDirectory = ".")
    let proc      = new Process(StartInfo = procStart)
    proc.Start() |> ignore


[<EntryPoint>]
let main argv =
  performanceTest()
  producePDF()

  0