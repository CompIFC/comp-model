open Labels
open Language
open Auxfunctions

type lcell = var * value 
  
type tcell = var * value * label 

type scell =
  | VarVal of var * value 
  | LocNode of node

type tscell =
  | TaintVal of var * value * label option
  | TaintNode of node * label option



let rec readSMEVal (m:lcell list) (x:var) =
  match m with
  | [] -> Some DV
  | (k, o)::t -> if k = x then (Some o) else readSMEVal t x


let rec updateSME m x v =
  match m with
  | [] -> [(x, v)]
  | (k, o)::t -> if k = x then (x, v)::t else ((k, o)::(updateSME t x v))


let rec readFacetedVal pc m x =
  match m with
  | [] -> (Some DV, None)
  | (k, o)::t -> if k = x then (Some (getFacet o pc), None) else readFacetedVal pc t x


let rec updateFacet m pc x v =
  match m with
  | [] -> (match pc with 
          | None -> [(x, createFacet (getFacet v (Some H)) (getFacet v (Some L)))]
          | Some L -> [(x, FacValue ({fh=Void;fl= valueProj v L}))]
          | Some H -> [(x, FacValue ({fh=valueProj v H; fl=Void}))]
          | _ -> [])
  | (k, o)::t -> 
          if k = x then 
            (let c = (match pc with 
                      | Some L -> (x, FacValue ({fh=valueProj o H;fl=valueProj v L}))
                      | Some H -> (x, FacValue ({fh=valueProj v H;fl=valueProj o L}))
                      | _ -> (x, createFacet (getFacet v (Some H)) (getFacet v (Some L))))
            in c::t)
          else (k, o)::(updateFacet t pc x v)


let rec readTaintedVal m pc x : (value option * label option) =
  match m with
  | [] -> (Some DV, Some H)
  | (k, v, l)::t -> if k = x then (Some v, (joinOpt (Some l) pc)) else readTaintedVal t pc x


let rec updateTaintVal m x v l =
  match m with
  | [] -> [(x, v, l)]
  | (k, o, ol)::t ->  if k = x then (x, v, l)::t else (k, o, ol)::(updateTaintVal t x v l)


let updateTaint pc m x v l = 
  match l with 
  | None -> ()
  | Some l -> (match pc with 
              | None -> m := updateTaintVal !m x v l
              | Some HD -> ()
              | Some p -> m := updateTaintVal !m x v (join p l))


let rec getValueInStore m l =
  match m with 
  | [] -> Some DV
  | (VarVal (k, o))::m' -> if k = l then Some o else getValueInStore m' l
  | h::m' -> getValueInStore m' l


let rec updateValueInStore m x v =
  match m with
  | [] -> [VarVal (x, v)]
  | (VarVal (k, o))::t -> if k = x then (VarVal (x, v))::t else (VarVal (k, o))::(updateValueInStore t x v)
  | h::t -> h::(updateValueInStore t x v)


let rec getFacetedValueInStore pc m l =
  match m with 
  | [] -> Some DV
  | (VarVal (k, o))::m' -> if k = l then Some (getFacet o pc) else getFacetedValueInStore pc m' l
  | h::m' -> getFacetedValueInStore pc m' l


let rec updateFacetedValueInStore m pc x v =
  match m with 
  | [] -> [VarVal (x, mkFacet v pc)]
  | (VarVal (k, o))::m' -> 
      if k = x then (match pc with  | None -> (VarVal (x, v))::m'
                                    | Some l -> (VarVal (x, updFacet v l o))::m')
      else (VarVal (k, o))::(updateFacetedValueInStore m' pc x v)
  | h::m' -> h::(updateFacetedValueInStore m' pc x v)


let rec getTaintedValueInStore m pc i =
  match m with 
  | [] -> (None, pc)
  | (TaintVal (k, o, l))::m' -> if k = i then (Some o, l) else getTaintedValueInStore m' pc i
  | h::m' -> getTaintedValueInStore m' pc i


let rec updateTaintedValueInStore m x v l =
  match m with
  | [] -> [TaintVal (x, v, l)]
  | (TaintVal (k, o, l'))::t -> if k = x then (TaintVal (x, v, l))::t 
                                else ((TaintVal (k, o, l'))::(updateTaintedValueInStore t x v l))
  | h::t -> (h::(updateTaintedValueInStore t x v l))


let rec getNodeInStore m l =
  match m with 
  | [] -> None
  | (LocNode n)::m' -> if n.nid = l then Some n else getNodeInStore m' l
  | h::m' -> getNodeInStore m' l


let rec updateNodeInStore m l nn =
  match m with 
  | [] -> [LocNode nn]
  | (LocNode n)::m' -> if n.nid = l then (LocNode nn)::m' else (LocNode n)::(updateNodeInStore m' l nn)
  | h::m' -> h::(updateNodeInStore m' l nn)


let rec updateNodeValueInStore m i v =
  match m with 
  | [] -> []
  | (LocNode n)::m' ->  if n.nid = i then (n.v := v; (LocNode n)::m')
                        else (LocNode n)::(updateNodeValueInStore m' i v)
  | h::m' -> h::(updateNodeValueInStore m' i v)


let rec getFacetedNodeInStore m pc i =
  match m with 
  | [] -> None
  | (LocNode n)::m' ->  if n.nid = i then projNode n pc else getFacetedNodeInStore m' pc i
  | h::m' -> getFacetedNodeInStore m' pc i


let rec updateFacetedNodeInStore m l i nn =
  match m with 
  | [] -> [LocNode (updFacetNode nn l nullNode)]
  | (LocNode n)::m' ->  if n.nid = i then ((LocNode (updFacetNode nn l n))::m')
                        else (LocNode n)::(updateFacetedNodeInStore m' l i nn)
  | h::m' -> h::(updateFacetedNodeInStore m' l i nn)


let rec updateFacetedNodeValueInStore m pc i v =
  match m with 
  | [] -> []
  | (LocNode n)::m' ->  if n.nid = i then 
                          (match pc with  
                          | None -> let m1 = (updateFacetedNodeValueInStore [LocNode n] (Some H) i v) in
                                    (updateFacetedNodeValueInStore m1 (Some L) i v)@m'
                          | Some L -> n.v := createFacet (getFacet v (Some L)) (getFacet !(n.v) (Some L)); (LocNode n)::m'
                          | Some H -> n.v := createFacet (getFacet !(n.v) (Some H)) (getFacet v (Some H)); (LocNode n)::m'
                          | _ -> (LocNode n)::m')
                        else (LocNode n)::(updateFacetedNodeValueInStore m' pc i v)
  | h::m' -> h::(updateFacetedNodeValueInStore m' pc i v)


let rec getTaintedNodeInStore m i l =
  match m with 
  | [] -> (None, l)
  | (TaintNode (n, nl))::m' -> if n.nid = i then (Some n, nl) else getTaintedNodeInStore m' i (joinOpt nl l)
  | h::m' -> getTaintedNodeInStore m' i l


let rec updateTaintedNodeInStore m i nn l =
  match m with 
  | [] -> [TaintNode (nn, Some l)]
  | (TaintNode (n, nl))::m' ->  if n.nid = i then (TaintNode (nn, joinOpt (Some l) nl))::m'
                                else (TaintNode (n, nl))::(updateTaintedNodeInStore m' i nn l)
  | h::m' -> h::(updateTaintedNodeInStore m' i nn l)


let rec updateTaintedNodeValueInStore m pc i v l =
  match m with 
  | [] -> []
  | (TaintNode (n, nl))::m' ->  if n.nid = i then 
                                   (n.v := v; (TaintNode (n, joinOpt (joinOpt pc l) nl))::m')
                                else (TaintNode (n, nl))::(updateTaintedNodeValueInStore m' pc i v l)
  | h::m' -> h::(updateTaintedNodeValueInStore m' pc i v l)
