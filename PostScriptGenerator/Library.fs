namespace PostScriptGenerator

open FPP1.TreeManager

module Generator =

    let toPSslow tree =
  
        let fm = string 20
        let lh = string 40

        let beginning = "%!PS \n 700 999 translate \n newpath \n /Courier \n 20 selectfont \n (Hello world4!) show\n"

        let moveTo x y = string x + " " + string y + " moveto\n"
        let lineto x y = string x + " " + string y + " lineto\n"
        let label l = "(" + string l + ") " + "show"

        let rec genPSTree tree x y =
            match tree with
            | Node ((label, pos), [])           -> "\n" + x + (string pos) + " (" + string label + ")" + " show"
            | Node ((label, pos), subtrees)     -> "\n" + y + fm + " " + fm + " lineto"
        and genPSChildren children acc =
            match children with
            | []                                -> ""
            | Node ((label, pos), subtrees)::ts -> ""

        beginning + genPSTree tree "0" "0"

    let toPSfast tree =
        ""
