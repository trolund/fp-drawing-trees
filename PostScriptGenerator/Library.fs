namespace PostScriptGenerator

open FPP1.TreeManager

module Generator =

    let startX       = 0.0
    let startY       = -20.0

    let parentMargin = 10.0
    let nodeHeight   = 20.0
    let nodeWidth    = 50.0
    let depthHeight  = 50.0
    let depthMargin  = 16.0

    let psPre        = "%!\n<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n1 1 scale\n700 999 translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n"
    let psPost       = "showpage"
    let stroke       = "stroke\n"


    let toPSslow t =
        let moveto x y = 
            string x + " " + string y + " moveto\n"

        let lineto x y = 
            string x + " " + string y + " lineto\n"

        let label l 
            = "(" + string l + ") dup stringwidth pop 2 div neg 0 rmoveto show\n"

        let positionX x pos =
            match pos with
            | 0.0 -> x
            | _   -> x + pos * nodeWidth

        let rec subtreeWidth ts =
            match ts with 
            | []                      -> 0.0
            | Node ((_, pos), _)::[]  -> abs pos * nodeWidth
            | Node ((_, pos), _)::ts' -> let (Node((_, pos'), _)) = List.last ts'
                                         (abs pos + abs pos') * nodeWidth
                                       
        let rec subtreeLines ts x y =
            match ts with
            | []                     -> ""
            | Node ((_, pos), _)::ts -> let x' = positionX x pos 
                                        moveto x' y + lineto x' (y - depthHeight) + subtreeLines ts x y
                                        

        let rec psTree t x y =
            match t with
            | Node ((l, _), []) -> moveto x y + label l
            | Node ((l, _), ts) -> let out = moveto x y
                                   let y = y - parentMargin
                                   let out = out + label l + moveto x y
                                   let y = y - nodeHeight
                                   let out = out + lineto x y
                                   let lineWidth = subtreeWidth ts
                                   let x = x - (lineWidth / 2.0)
                                   let out = out + moveto x y
                                   let x = x + lineWidth
                                   let out = out + lineto x y + stroke
                                   let x = x - (lineWidth / 2.0)
                                   let out = out + subtreeLines ts x y + psSubtrees ts x (y - depthHeight - depthMargin)
                                   out
        and psSubtrees ts x y =
            match ts with
            | []     -> ""
            | t::ts' -> let (Node((_, pos), _)) = t
                        let x' = positionX x pos
                        psTree t x' y + psSubtrees ts' x y
                           
        psPre + psTree t startX startY + psPost

    let toPSfast t =
        ""