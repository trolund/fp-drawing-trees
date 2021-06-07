namespace PostScriptGenerator

open FPP1.TreeManager

module Generator =

    let toPSslow tree =
 
        let lineMargin = 10.0
        let nodeHeight = 20.0
        let layerHeight = 50.0
        let nodeWidth = 50.0

        let startX = 0.0
        let startY = -20.0

        let beginning = "%!\n<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n1 1 scale\n700 999 translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n" + string startX + " " + string startY + " moveto\n"
        let endFile = "showpage"
        let stroke = "stroke\n"

        let moveTo x y = string x + " " + string y + " moveto\n"
        let lineto x y = string x + " " + string y + " lineto\n"
        let label l = "(" + string l + ") dup stringwidth pop 2 div neg 0 rmoveto show\n"

        let rec lineWidth subtrees acc =
            match subtrees with 
            | []                     -> acc * nodeWidth
            | Node ((_, pos), _)::ts -> lineWidth ts ((abs pos) + acc)

        let rec genPSTree tree x y =
            match tree with
            | Node ((l, pos), [])           -> moveTo x y + label l
            | Node ((l, pos), subtrees)     -> let y = y - lineMargin
                                               let out = label l + moveTo x y
                                               let y = y - nodeHeight
                                               let out = out + lineto x y
                                               let lineWidth = lineWidth subtrees 0.
                                               let x = x - (lineWidth / 2.)
                                               let out = out + moveTo x y
                                               let x = x + lineWidth
                                               let out = out + lineto x y                    
                                               let out = out + stroke
                                               out + genPSChildren subtrees startX y
        and genPSChildren children x y =
            match children with
            | []                            -> ""
            | Node ((l, pos), subtrees)::ts -> let x = pos * nodeWidth
                                               let out = moveTo x y + lineto x (y - layerHeight)
                                               let out = out + genPSChildren ts x y
                                               let out = out + List.fold (fun acc t -> acc + genPSTree t x (y - layerHeight - nodeHeight)) "" subtrees
                                               out + stroke
                                               
                                               
                                               //out + List.fold (fun acc t -> acc + genPSTree t x (y - layerHeight - nodeHeight)) "" ts + List.fold (fun acc t -> acc + genPSTree t x (y - layerHeight - nodeHeight)) "" subtrees

        beginning + genPSTree tree startX startY + endFile

        // + List.fold (fun acc t -> acc + genPSTree t x (y - layerHeight - lineMargin)) "" ts

    let toPSfast tree =
        ""
