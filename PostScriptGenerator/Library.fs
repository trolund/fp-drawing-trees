namespace PostScriptGenerator

open FPP1.TreeManager

module Generator =

    let lineMargin = 10.0
    let nodeHeight = 20.0
    let layerHeight = 50.0
    let nodeWidth = 50.0

    let startX = 0.0
    let startY = -20.0

    let beginning = "%!\n<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n1 1 scale\n700 999 translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n"
    let endFile = "showpage"
    let stroke = "stroke\n"

    let toPSslow tree =
        let moveto x y = 
            string x + " " + string y + " moveto\n"

        let lineto x y = 
            string x + " " + string y + " lineto\n"

        let label l 
            = "(" + string l + ") dup stringwidth pop 2 div neg 0 rmoveto show\n"

        let comment s = "%% " + s

        let rec lineWidth subtrees =
            match subtrees with 
            | []                     -> 0.0
            | Node ((_, pos), _)::[] -> (abs pos) * nodeWidth
            | Node ((_, pos), _)::ts -> let (Node((_, pos'), _)) = (List.rev ts).Head
                                        ((abs pos) + (abs pos')) * nodeWidth
                                       

        let rec genLines children x y =
            match children with
            | []                     -> ""
            | Node ((l, pos), _)::ts -> let x' = if pos = 0.0 then
                                                     x
                                                 else
                                                     x + pos * nodeWidth 
                                        let out = moveto x' y + lineto x' (y - layerHeight)
                                        let out = out + genLines ts x y
                                        out


        let rec genPSTree tree x y =
            match tree with
            | Node ((l, pos), [])           -> moveto x y + label l
            | Node ((l, pos), subtrees)     -> let out = moveto x y
                                               let y = y - lineMargin
                                               let out = out + label l + moveto x y
                                               let y = y - nodeHeight
                                               let out = out + lineto x y
                                               let lineWidth = lineWidth subtrees
                                               let out = out + comment ("lineWidth: " + string lineWidth + "\n")
                                               let x = x - (lineWidth / 2.0)
                                               let out = out + moveto x y
                                               let x = x + lineWidth
                                               let out = out + lineto x y
                                               let x = x - (lineWidth / 2.0)
                                               let out = out + stroke
                                               let out = out + genLines subtrees x y
                                               let y = y - layerHeight - 16.0;
                                               let out = out + genPSChildren subtrees x y
                                               out
        and genPSChildren children x y =
            match children with
            | []    -> ""
            | t::ts -> let (Node((_, pos), _)) = t
                       let out = comment ("before: x = " + string x + ", y =  " + string y + ", pos = " + string pos + "\n")
                       let x' = if pos = 0.0 then
                                   x
                                else 
                                   x + pos * nodeWidth 
                       let out = out + comment ("after: x = " + string x + ", y =  " + string y + "\n")            
                       let out = out + genPSTree t x' y + genPSChildren ts x y 
                       out + stroke        

        beginning + genPSTree tree startX startY + endFile

    let toPSfast tree =
        ""
