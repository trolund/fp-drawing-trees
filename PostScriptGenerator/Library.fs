namespace PostScriptGenerator

open FPP1.TreeManager

module Generator =

    let moveTo x y = string x + " " + string y + " moveto\n"
    let text s = "(" + string s + ") " + "show"

    let ps t =

    let toPSslow tree =
  
        let fm = string 20
        let lh = string 40

        let b = "%!PS\n /Courier \n 20 selectfont\n
 (Hello world4!) show\n"

        let rec genPSTree tree x y =
            match tree with
            | Node ((label, pos), [])           -> "\n" + x + (string pos) + " (" + string label + ")" + " show"
            | Node ((label, pos), subtrees)     -> "\n" + y + fm + " " + fm + " lineto"
        and genPSChildren children acc =
            match children with
            | []                                -> ""
            | Node ((label, pos), subtrees)::ts -> ""
            
        b + genPSTree tree "0" "0" + e

    let toPSfast tree =
        ""
