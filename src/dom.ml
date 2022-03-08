open Labels
open Language
open Store
open Auxfunctions


let getValue (v:value) (l:label option) =
  match l with 
  | None -> v
  | Some l' -> if valueProj v l' = Void then Void else valueProj v l'

let updValue (v:value) (nv:value) (l:label option): (value) =
  match l with 
  | None -> nv
  | Some l' -> updFacet nv l' v


let rec replaceE e x v = 
  match e with
  | Val v' -> Val v'
  | Var y -> if y = x then Val v else Var y
  | Bop (o, e1, e2) -> Bop (o, (replaceE e1 x v), (replaceE e2 x v))
  | NodeValue e1 -> NodeValue (replaceE e1 x v)

let rec replace cmd x v = 
  match cmd with
  | Seq (Skip, c) -> Seq (Skip, replace c x v)
  | Seq (c1, c2) -> Seq (replace c1 x v, replace c2 x v)
  | AssignVar (y, e) ->  AssignVar (y, replaceE e x v)
  | If (e, c1, c2) -> If (replaceE e x v, replace c1 x v, replace c2 x v)
  | While (e, c) -> While (replaceE e x v, replace c x v)
  | Output (l, e) -> Output (l, replaceE e x v)
  | Declassify (y, i, e) -> Declassify (y, i, replaceE e x v)
  | Dispatch (a, ev, e) -> Dispatch (a, ev, replaceE e x v)
  | CreateElement (y, i, e) -> CreateElement (y, i, replaceE e x v)
  | FacCmd (c1, c2) -> FacCmd (replace c1 x v, replace c2 x v)
  | _ -> cmd

let rec mapEH (hlist:handlers) (v:value) : (command * label option) list = 
  match hlist with 
  | [] -> []
  | (x, c, l)::tl ->  (replace c x v, l)::(mapEH tl v) 


let lookupEH  (store: (module Store)) (pc:label option) (ev:event) : (command * label option) list  = 
  let (i, e, v) = ev in
  let module S = (val store) in
  match S.getNode pc i with
  | None, _ -> print_endline "cant find node"; []
  | Some n, l ->  let ehl = handlersAtPC (getEventHandler !(n.ehMap) e) pc in 
                  let eh = handlersJoinLabel ehl (joinOpt l pc) in
                  (mapEH eh v)


let createElemSub (store: (module Store)) (pc:label option) (i:id) (nv:value) : (module Store) =
  let module S = (val store) in
  let n = (match pc with 
  | None -> {nid = i; v = ref nv; ehMap = ref []; }
  | Some pcl -> let on = {nid = i; v = ref nv; ehMap = ref []; } in
                if (valueProj nv pcl) = Void then nullNode else on) in 
  S.createNode pc i n pc; store
  	

let createElem  (store: (module Store)) (pc:label option) (i:id) (nv:value) (l:label option) : (module Store) =
  let module S = (val store) in
  (match pc, S.globalstoretype with 
  | None, Low ->  (createElemSub store (Some L) i (getFacet nv (Some H)))
  | None, Tainted -> (createElemSub store (Some L) i (getFacet nv (Some L)))
  | None, SMS -> let store' =  createElemSub store (Some H) i (getFacet nv (Some H)) in
  				 createElemSub store' (Some L) i (getFacet nv (Some L))
  | _, Faceted -> (createElemSub store pc i (getFacet nv pc))
  | Some pcl, Low -> if labelLessThanEq pcl L then (createElemSub store pc i nv) else store
  | Some pcl, Tainted -> if labelLessThanEq pcl L then createElemSub store (joinOpt pc l) i nv else createElemSub store (Some HD) i nv
  | Some pcl, SMS -> createElemSub store pc i (getFacet nv pc))
  
  
let lookupValSub  (store: (module Store)) (pc:label option) (i:id)  : (value * label option) = 
  let module S = (val store) in
  match S.getNode pc i with
  | None, _ -> Void, None
  | Some nl, ol -> (match S.globalstoretype with 
                    | Low -> if labelLessThanEqOpt pc (Some L) then (DV, pc) else (!(nl.v), (joinOpt pc ol))
                    | Faceted -> ((getFacet !(nl.v) pc), pc)
                    | _ -> (!(nl.v), (joinOpt pc ol))
                    )


let lookupVal  (store: (module Store)) (pc:label option) (a:value)  : (value * label option) = 
  match a with 
  | Id l -> lookupValSub store pc l
  | _ -> Void, None


let rec addEH  (store: (module Store)) (pc:label option) (i:id) (e:eventName) (eh:handler) : (module Store) = 
  let module S = (val store) in
  match pc, S.globalstoretype with 
  | None, Faceted -> (match S.getNode pc i with
                | None, _ -> store
                | Some n, _ -> n.ehMap := addEventHandler (!(n.ehMap)) e eh; store)
  | None, _ -> let store' = addEH store (Some H) i e eh in addEH store' (Some L) i e eh
  | Some l, _ ->  (match S.getNode pc i with
                | None, _ -> store
                | Some n, _ -> n.ehMap := addEventHandler (!(n.ehMap)) e eh; store)


let triggerSub  (store: (module Store)) (pc:label option) (i:id) (e:eventName) (v:value) : (event * label option) list = 
  let module S = (val store) in
  match S.getNode pc i with
  | None, _ -> []
  | Some n, l -> match pc, v with 
                  | None, FacValue fv -> mergeEvs (n.nid, e, fv.fh) H (n.nid, e, fv.fl) L
                  | _, v -> [((n.nid, e, v), joinOpt pc l)]


let triggerAll  (store: (module Store)) (pc:label option) (i:id) (e:eventName) (v:value) : (event * label option) list = 
  let module S = (val store) in
  (match S.globalstoretype with 
  | Low -> (match S.getNode pc i with | None, _ -> [] | Some n, l -> [((n.nid, e, v), Some L); ((n.nid, e, v), Some H)])
  | SMS -> (triggerSub store (Some H) i e v)@(triggerSub store (Some L) i e v)
  | Faceted -> (match S.getNode pc i with | None, _ -> [] | Some n, l -> [((n.nid, e, v), None)])
  | Tainted -> (match S.getNode pc i with | None, _ -> [] | Some n, l -> [((n.nid, e, v), Some H); ((n.nid, e, v), Some L)])
  )


let trigger  (store: (module Store)) (pc:label option) (i:id) (e:eventName) (v:value) : (event * label option) list = 
	triggerSub store pc i e v 


let getNodeLabel  (store: (module Store)) (pc:label) (i:id) : label option = 
  let module S = (val store) in
  match S.getNode (Some pc) i with
  | None, _ -> None
  | Some n, nl -> nl
