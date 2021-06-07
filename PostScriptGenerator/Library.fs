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

        let beginning = "%!\n<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n1 1 scale\n700 999 translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n"
        let endFile = "showpage"
        let stroke = "stroke\n"

        let moveTo x y = string x + " " + string y + " moveto\n"
        let lineto x y = string x + " " + string y + " lineto\n"
        let label l = "(" + string l + ") dup stringwidth pop 2 div neg 0 rmoveto show\n"

        let rec lineWidth subtrees acc =
            match subtrees with 
            | []                     -> acc * nodeWidth
            | Node ((_, pos), _)::ts -> lineWidth ts ((abs pos) + acc)


        let rec genLines children x y =
            match children with
            | []                     -> ""
            | Node ((l, pos), _)::ts -> let x' = if pos = 0.0 then
                                                     x
                                                 else 
                                                     x + pos * nodeWidth  
                                        let out = moveTo x' y + lineto x' (y - layerHeight)
                                        let out = out + genLines ts x y
                                        out


        let rec genPSTree tree x y =
            match tree with
            | Node ((l, pos), [])           -> moveTo x y + label l
            | Node ((l, pos), subtrees)     -> let out = moveTo x y
                                               let y = y - lineMargin
                                               let out = out + label l + moveTo x y
                                               let y = y - nodeHeight
                                               let out = out + lineto x y
                                               let lineWidth = lineWidth subtrees 0.
                                               let x = x - (lineWidth / 2.)
                                               let out = out + moveTo x y
                                               let x = x + lineWidth
                                               let out = out + lineto x y   
                                               let x = x - (lineWidth / 2.)
                                               let out = out + stroke
                                               let out = out + genLines subtrees x y
                                               let y = y - layerHeight - nodeHeight
                                               out + genPSChildren subtrees x y
                                               out
        and genPSChildren children x y =
            match children with
            | []                            -> ""
            | Node ((l, pos), subtrees)::ts -> let out = List.fold (fun acc t -> acc + genPSTree t x y) "" ts
                                               let out = out + List.fold (fun acc t -> acc + genPSTree t x y) "" subtrees
                                               out

                                                            
                                              
                                               //out + List.fold (fun acc t -> acc + genPSTree t x (y - layerHeight - nodeHeight)) "" ts + List.fold (fun acc t -> acc + genPSTree t x (y - layerHeight - nodeHeight)) "" subtrees

        beginning + genPSTree tree startX startY + endFile

        // + List.fold (fun acc t -> acc + genPSTree t x (y - layerHeight - lineMargin)) "" ts

    let toPSfast tree =
        ""
