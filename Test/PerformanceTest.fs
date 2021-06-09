namespace PerformanceTest

open FPP1.TreeManager
open System.Diagnostics
open PostScriptGenerator.Generator

module PerformanceTest = 

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

    let timeFunc n f = 
      // start timer.
      let stopWatch = Stopwatch.StartNew()
    
      // method to test
      let _ = f ()
    
      // stop timer.
      stopWatch.Stop()
    
      // print result
      printfn "%s : %f" n stopWatch.Elapsed.TotalMilliseconds
      stopWatch.Elapsed.TotalMilliseconds
    
    let multiRun n tl f = 
    
      let rec aux acc tl' =
        match tl' with
        | [] -> acc
        | t::tx -> 
               let v = (timeFunc n (fun() -> f t ))
               aux (v :: acc) tx
    
      // actual run 
      aux [] tl
    
    let multiRunFlet2 tl f1 f2 = 
    
      let rec aux acc tl' =
        match tl' with
        | [] -> acc
        | t::tx when tx.Length % 2 = 0 -> 
               let v2 = (timeFunc "f2" (fun() -> f2 t ))
               let v1 = (timeFunc "f1" (fun() -> f1 t ))
               aux ((v1, v2) :: acc) tx
        | t::tx when tx.Length % 2 <> 0 -> 
               let v1 = (timeFunc "f1" (fun() -> f1 t ))
               let v2 = (timeFunc "f2" (fun() -> f2 t ))    
               aux ((v1, v2) :: acc) tx
    
      // actual run 
      aux [] tl
    
    let multiRunFlet n f1 f2 i = 
    
        let rec aux acc n =
          match n with
          | 0 -> acc
          | n when n % 2 = 0 -> 
                 let v2 = (timeFunc "f2" (fun() -> f2 i ))
                 let v1 = (timeFunc "f1" (fun() -> f1 i ))
                 aux ((v1, v2) :: acc) (n - 1)
          | n when n % 2 <> 0 -> 
                 let v1 = (timeFunc "f1" (fun() -> f1 i ))
                 let v2 = (timeFunc "f2" (fun() -> f2 i ))    
                 aux ((v1, v2) :: acc) (n - 1)
    
        // actual run 
        aux [] n
    
    let rec tupleToLists acc l =
      match l with
      | [] -> acc
      | (v1, v2)::ls -> let (av1, av2) = acc
                        tupleToLists (v1 :: av1, v2 :: av2) ls
    
    let avg l =        
        let sum, count = List.fold (fun (s, c) v -> (s + v, c + 1.)) (0.,0.) l
        sum / count

    let performanceTest() =
        // let t1 = design n1
         // let t2 = design n2
         // let t3 = design n3
         // let t4 = design n4
         // let t5 = design n5
         // let t6 = design n6
         // let t7 = design n7
         // let t8 = design n8
         // let t9 = design fact

         let t10 = design test

         // let slowavg = multiRun "slow" [t1; t2; t3; t4; t5; t6; t7; t8; t9] (fun t -> toPSslow t) |> avg
         // let fastavg = multiRun "fast" [t1; t2; t3; t4; t5; t6; t7; t8; t9] (fun t -> toPSfast t) |> avg

         // printfn "%s : %f" "avg slow" slowavg
         // printfn "%s : %f" "avg fast" fastavg

         // let slow = timeFunc "slow" (fun() -> toPSslow t10)
         // let fast = timeFunc "fast" (fun() -> toPSslow t10)


         // printfn "%s : %f" "res slow" slow
         // printfn "%s : %f" "res fast" fast

         // let testValues = [t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10; t10]
         //let res = multiRunFlet testValues (fun t -> toPSfast t) (fun t -> toPSslow t)
         let res = multiRunFlet 1000 (fun t -> toPSfast t) (fun t -> toPSslow t) t10

         let (listf1, listf2 ) = tupleToLists ([], []) res

         printfn "%s : %f" "avg slow" (listf1 |> avg)
         printfn "%s : %f" "avg fast" (listf2 |> avg)