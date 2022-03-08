type label = L | H | HD;;

(* Follow the higher security label? *)
let join lab1 lab2 =
    if lab1 = HD || lab2 = HD then HD
    else if lab1 = H || lab2 = H then H
    else L

let labelLessThanEq lab1 lab2 =
    match lab1, lab2 with
    | L, L -> true
    | L, H -> true
    | H, H -> true
    | H, HD -> true
    | HD, H -> true
    | HD, HD -> true
    | _, _ -> false

let joinOpt lab1 lab2 = 
    match lab1, lab2 with
    | None, None -> None
    | None, Some x -> Some x
    | Some x, None -> Some x
    | Some x, Some HD -> Some HD
    | Some HD, Some x -> Some HD
    | Some x, Some H -> Some H
    | Some H, Some x -> Some H
    | Some L, Some L -> Some L
    
let labelLessThanEqOpt lab1 lab2 = 
    match lab1, lab2 with
    | None, None -> true
    | None, Some x -> true
    | Some x, None -> false
    | Some x, Some HD -> true
    | Some HD, Some x -> false
    | Some x, Some H -> true
    | Some H, Some L -> false
    | Some L, Some L -> true
