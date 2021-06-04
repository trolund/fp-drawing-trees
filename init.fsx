type 'a Tree = Node of 'a * ('a Tree list)


let rec design =
    function
    | Node (a, tree) -> a
