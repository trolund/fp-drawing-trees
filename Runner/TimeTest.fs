// namespace Runner

// open System.Diagnostics
// open FPP1.TreeManager
// open PostScriptGenerator.Generator

// module PerformanceTest = 

//   let n1 = Node ("NODE1", []);
//   let n2 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", []); Node ("NODE4", [])]);
//   let n3 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", []); Node ("NODE5", [])]); Node ("NODE3", [Node ("NODE6", []); Node ("NODE7", [])])]);
//   let n4 = Node ("NODE1", [Node ("NODE2", [Node ("NODE5", []); Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE3", [Node ("NODE8", []); Node ("NODE9", []); Node ("NODE10", [])]); Node ("NODE4", [Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])])
//   let n5 = Node ("NODE1", [Node ("NODE2", [Node ("NODE6", []); Node ("NODE7", []); Node ("NODE8", []); Node ("NODE9", [Node ("NODE10", []); Node ("NODE11", []); Node ("NODE12", []); Node ("NODE13", [])])]); Node ("NODE3", []); Node ("NODE4", []); Node ("NODE5", [])]);
//   let n6 = Node ("NODE1", [Node ("NODE2", [Node ("NODE3", [Node ("NODE4", [Node ("NODE5", []);]);]);]);]);
//   let n7 = Node ("NODE1", [Node ("NODE2", []); Node ("NODE3", [Node ("NODE4", []); Node ("NODE5", [Node ("NODE6", []); Node ("NODE7", [])])])]);
//   let n8 = Node ("NODE1", [Node ("NODE2", [Node ("NODE4", [Node ("NODE6", []); Node ("NODE7", [])]); Node ("NODE5", [])]); Node ("NODE3", [])]);
//   let fact = Node ("Block", [Node ("VarDec", [Node ("n", []); Node ("IntTyp", []); Node ("Int 4", [])]); Node ("VarDec", [Node ("y", []); Node ("IntTyp", []); Node ("Int 1", [])]); Node ("Seq", [Node ("While", [Node ("ApplyPrim", [Node ("<>", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 0", [])]); Node ("Seq", [Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"y\"", []); Node ("ApplyPrim", [Node ("*", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("ContOf", [Node ("Var \"y\"", [])])])]); Node ("Ass", [Node ("Var \"n\"", []); Node ("ApplyPrim", [Node ("-", []); Node ("ContOf", [Node ("Var \"n\"", [])]); Node ("Int 1", [])])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"n\"", [])])])]); Node ("PrintLn", [Node ("ApplyPrim", [Node ("toString", []); Node ("ContOf", [Node ("Var \"y\"", [])])])])])]);

//   let timeFunc n f = 
//       // start timer.
//       let stopWatch = Stopwatch.StartNew()

//       // method to test
//       let _ = f ()

//       // stop timer.
//       stopWatch.Stop()

//       let res = stopWatch.Elapsed.TotalMilliseconds
//       // print result
//       printfn "%s : %f" n res
//       res

//   let multiRun n tl f = 

//     let rec aux acc tl' =
//       match tl' with
//       | [] -> acc
//       | t::tx -> 
//              let v = (timeFunc n (fun() -> f t ))
//              aux (v :: acc) tx

//     aux [] tl

//   let avg l =        
//       let sum, count = List.fold (fun (s, c) v -> (s + v, c + 1.)) (0.,0.) l
//       sum / count

//   let pTest() = 
//     let t1 = design n1
//     let t2 = design n2
//     let t3 = design n3
//     let t4 = design n4
//     let t5 = design n5
//     let t6 = design n6
//     let t7 = design n7
//     let t8 = design n8
//     let t9 = design fact

//     let slowavg = multiRun "slow" [t1; t2; t3; t4; t5; t6; t7; t8; t9] (fun t -> toPSslow t) |> avg
//     let fastavg = multiRun "fast" [t1; t2; t3; t4; t5; t6; t7; t8; t9] (fun t -> toPSfast t) |> avg

//     printfn "%s : %f" "avg slow" slowavg
//     printfn "%s : %f" "avg fast" fastavg