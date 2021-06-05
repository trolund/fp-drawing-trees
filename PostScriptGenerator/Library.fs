namespace PostScriptTranslator

open FPP1.TreeManager

module Translator =

    let moveTo x y = string x + " " + string y + " moveto\n"
    let text s = "(" + string s + ") " + "show"

    let ps t =

        let fm = string 20
        let lh = string 40

        let b = "%!PS\n /Courier \n 20 selectfont\n
 (Hello world4!) show\n"

        let e = "showpage" 

        // let rec aux acc =
        //     function
        //     | Node ((v, pos), []) -> moveTo pos 20 + " " + text v
        //     | Node ((v, pos), tl) -> acc + List.fold (fun x -> aux "" x) tl

        b + e
