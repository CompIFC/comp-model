
let rec take n l = if n == 0 then [] else match l with a :: r -> a :: take (n-1) r | [] -> [];;

