open System
open System.Diagnostics

open FPP1.TreeManager
open PostScriptGenerator.Generator
open System.Runtime.InteropServices
   

let n1 = Node ("NODE1", []);
let n2 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", []); Node ("NODE4", [])]);
let n3 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", []); Node ("NODE5", [])]); Node ("NODE3", [Node ("NODE6", []); Node ("NODE7", [])])]);
let n4 = Node ("NODE1", [Node ("NODE2", [Node ("NODE5", []); Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE3", [Node ("NODE8", []); Node ("NODE9", []); Node ("NODE10", [])]); Node ("NODE4", [Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])])
let n5 = Node ("NODE1", [Node ("NODE2", [Node ("NODE6", []); Node ("NODE7", []); Node ("NODE8", []); Node ("NODE9", [Node ("NODE10", []); Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])]); Node ("NODE3", []); Node ("NODE4", []); Node ("NODE5", [])]);
let n6 = Node ("NODE1", [Node ("NODE2", [Node ("NODE3", [Node ("NODE4", [Node ("NODE5", []);]);]);]);]);
let n7 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", [Node ("NODE4", []); Node ("NODE5", [Node ("NODE6", []); Node ("NODE7", [])])])]);
let n8 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", [Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE5", [])]); Node ("NODE3", [])]);
let fact = Node ("Block", [Node ("VarDec", [Node ("n", []); Node ("IntTyp", []); Node ("Int 4", [])]); Node ("VarDec", [Node ("y", []); Node ("IntTyp", []); Node ("Int 1", [])]); Node ("Seq", [Node ("While", [Node ("ApplyPrim", [Node ("<>", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 0", [])]); Node ("Seq", [Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"y\"", []); Node ("ApplyPrim", [Node ("*", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"n\"", []); Node ("ApplyPrim", [Node ("-", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 1", [])])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])])])]);

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

let timeFunc n f = 
  // start timer.
  let stopWatch = Stopwatch.StartNew()

  // method to test
  let v = f()

  // stop timer.
  stopWatch.Stop()

  // print result
  printfn "%s : %f" n stopWatch.Elapsed.TotalMilliseconds
  stopWatch.Elapsed.TotalMilliseconds

let multiRun n f = 

  let rec aux acc n =
    match n with
    | n when n = 0 -> acc
    | n -> let rand = new Random()
           let v = f()
           aux (v :: acc) (n - 1) 

  aux [] n

let avg l =        
    let sum, count = List.fold (fun (s, c) v -> (s + v, c + 1.)) (0.,0.) l
    sum / count

[<EntryPoint>]
let main argv =
  let tree = design n8

  let slowavg = multiRun 20 (fun() -> timeFunc "slow" (fun() -> toPSslow tree )) |> avg
  let fastavg = multiRun 20 (fun() -> timeFunc "fast" (fun() -> toPSfast tree )) |> avg

  printfn "%s : %f" "avg slow" slowavg
  printfn "%s : %f" "avg fast" fastavg

  producePDF()

  0