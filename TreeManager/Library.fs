namespace FPP1

module TreeManager =

    type 'a Tree = Node of 'a * ('a Tree list)
    type Extent = (float * float) list


    let movetree (Node ((label, x), subtrees), x': float) = 
        Node((label, x + x'), subtrees)


    let moveextent (e, x) =
        List.map (fun (p, q) -> (p + x, q + x)) e


    let rec merge ps qs =
        match (ps, qs) with
        | ([], qs)                     -> qs
        | (ps, [])                     -> ps
        | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge ps qs


    let mergelist es = 
        List.fold merge [] es


    let rmax (p: float, q: float) = 
        if p > q then p else q


    let rec fit =
        function
        | ((_, p) :: ps), ((q, _) :: qs) -> rmax (fit (ps, qs), p - q + 1.0)
        | _                              -> 0.0


    let fitlistl es =
        let rec fitlistl' acc =
            function
            | []      -> []
            | e :: es ->
                let x = fit (acc, e)
                x :: fitlistl' (merge acc (moveextent (e, x))) es
        fitlistl' [] es


    // val flipextent : Extent -> Extent = map (fn (p,q) => (~q,~p))
    let flipextent e =
        List.map (fun (p, q) -> (-q, -p)) e


    // val fitlistr = rev o map ~ o fitlistl o map flipextent o rev
    // let fitlistr (es: Extent list) =
    //    List.rev (List.map (-) (fitlistl (List.map flipextent (List.rev es))));;
    let fitlistr (es: Extent list) =
        List.rev es
        |> List.map flipextent
        |> fitlistl
        |> List.map (fun e -> -e)
        |> List.rev


    let mean (x, y) = 
        (x + y) / 2.0


    let fitlist es =
        List.map mean (List.zip (fitlistl es) (fitlistr es))


    let design tree =
        let rec design' (Node (label, subtrees)) =
            let (trees, extents) = List.unzip (List.map design' subtrees)
            let positions = fitlist extents
            let ptrees =
                List.map movetree (List.zip trees positions)
            let pextents =
                List.map moveextent (List.zip extents positions)
            let resultextent = (0.0, 0.0) :: mergelist pextents
            let resulttree = Node((label, 0.0), ptrees)
            (resulttree, resultextent)
        fst (design' tree)
