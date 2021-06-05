namespace PostScriptTranslator

open FPP1.TreeManager

module Translator =

    let ps t =

        let fm = string 20
        let lh = string 40

        let b =
            "%!PS /Times-Bold findfont 36 scalefont setfont"

        let e = "showpage"

        let rec aux acc =
            function
            | Node (v, []) -> acc + " (" + string v + ")" + " show"
            | Node (v, tl) -> acc + fm + " " + fm + " lineto"

        b + aux "" t + e

        b + aux "" t + e
