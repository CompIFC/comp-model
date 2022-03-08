open Labels
open Language


(* Function definitions *)
let globalcount : id ref = ref 0


let getNewIdAddr (unit) : id = 
    globalcount := !globalcount + 1; !globalcount


let nullNode = {nid = 0; v = ref Void; ehMap = ref [];}


let initialIds : (id list) ref = ref []


let stringOfDI d : string = 
    match d with 
    | Low -> "Low"
    | Faceted -> "Faceted"
    | SMS -> "SME"
    | Tainted -> "Tainted"


let getOutputValue (v:value option) = 
  match v with 
  | Some (Num res) -> Some (Out(res)) 
  | Some (Bool true) -> Some (Out(1))
  | Some (Bool false) -> Some (Out(0))
  | Some DV -> Some (Out(-1))
  | _ -> None


let rec print_output ol =
  match ol with
  | (Out out)::tl -> Printf.printf "%d\n" out; print_output tl
  | [] -> ()


let rec print_value v = 
  match v with 
  | Void -> print_endline "void"
  | DV -> print_endline "dv"
  | Num i -> print_endline "number"
  | Bool b -> print_endline "bool"
  | Id i -> print_endline "id"
  | FacValue fv -> print_endline "faceted"; print_value fv.fh; print_value fv.fl


let outCondition s o pc =
  match o with 
  | None -> false
  | Some (v, l) ->  if s = "Tainted" then true
                    else if Some l = pc then true 
                    else if pc = None && s = "Faceted" then true
                    else false


let getOutput o pc = 
  match o with 
  | None -> None
  | Some (v, l) -> Some v


let rec getEventLabel e v labMap =
  match labMap with
  | [] -> L
  | ((ev, _), l)::labMap' -> if e = ev then l else getEventLabel e v labMap'


let lookupLab e labMap =
  let (ie, eve, ve) = e in
  	if List.mem ie (!initialIds) then getEventLabel eve ve labMap
	else HD


let rec getEventHandler eMap event =
  match eMap with 
  | [] -> [] 
  | (f, l)::eMap' ->  if f = event then l
                      else getEventHandler eMap' event


let rec addEventHandler eMap event h =
  match eMap with 
  | [] -> [(event, [h])]
  | (f, l)::eMap' -> if f = event then (f, l@[h])::eMap'
                     else (f, l)::(addEventHandler eMap' event h)


let rec removeElement (l:'a list) (e:'a) = 
  match l with
  | [] -> []
  | h::tl -> if h = e then tl else h::(removeElement tl e)


let rec uniqueElements (l:'a list) (l':'a list) = 
  match l with
  | [] -> l'
  | h::tl -> if List.mem h l' then uniqueElements tl l' else h::(uniqueElements tl l')


let rec isProducer kappa = 
  match kappa with 
  | Config (c, s, e) -> if s = P then true else false
  | SMEConfig (kh, kl) -> isProducer kh || isProducer kl





(*
  PROJECTIONS OF FACETED VALUES - REQUIRES A LABEL (NOT NONE)
*)
let valueProj (v:value) (pc:label) : value = 
  match v with 
  | FacValue fv -> if pc = H then fv.fh else fv.fl
  | _ -> v


let rec handlersProj (v:handlers) (pc:label option) : handlers = 
  match v with 
  | [] -> []
  | (x, c, l)::v' ->  if pc = None || labelLessThanEqOpt l pc then (x, c, l)::(handlersProj v' pc) 
                      else (handlersProj v' pc)


let rec ehMapProj (v:eventHandlerMap) (pc:label option) : eventHandlerMap = 
  match v with 
  | [] -> []
  | (e, h)::v' -> let h' = handlersProj h pc in 
                  if h' = [] then ehMapProj v' pc else (e, h')::(ehMapProj v' pc)



let rec handlersAtPC (v:handlers) (pc:label option) : handlers = 
  match v with 
  | [] -> []
  | (x, c, l)::v' ->  if l = pc || l = None then (x, c, l)::(handlersAtPC v' pc) 
                      else (handlersAtPC v' pc)


let rec ehMapAtPC(v:eventHandlerMap) (pc:label option) : eventHandlerMap = 
  match v with 
  | [] -> []
  | (e, h)::v' -> let h' = handlersAtPC h pc in 
                  if h' = [] then ehMapAtPC v' pc else (e, h')::(ehMapAtPC v' pc)


let rec handlersJoinLabel (v:handlers) (pc:label option) : handlers = 
  match v with 
  | [] -> []
  | (x, c, l)::v' ->  (x, c, joinOpt l pc)::(handlersJoinLabel v' pc) 


(*
  FACETED FUNCTIONS - CREATE, UPDATE, ... FACETED VALUE
*)
let createFacet vh vl =
  if vh = vl then vh
  else FacValue ({fh=vh;fl=vl})


let setFacet ch cl = 
  if ch = cl then ch
  else FacCmd (ch, cl)


let mkFacet v pc = 
  match pc with 
  | None -> v
  | Some L -> FacValue ({fh=Void;fl=v})
  | Some H -> FacValue ({fh=v;fl=Void})
  | _ -> DV


let getFacet v pc = 
  match pc with 
  | None -> v
  | Some l -> let vl = valueProj v l in if vl = Void then DV else vl


let updFacet v' l v = 
  match v with 
  | FacValue fv -> if l = H then FacValue ({fh=v';fl=fv.fl}) else FacValue ({fh=fv.fh;fl=v'})
  | av -> if l = H then FacValue ({fh=v';fl=av}) else FacValue ({fh=av;fl=v'})


let projNode (n:node) (pc:label option) : node option = 
  match pc with 
  | None ->  Some n
  | Some l ->
      (if !(n.v) != Void && (valueProj !(n.v) l) = Void then None
      else Some ({nid = n.nid; v = ref (valueProj !(n.v) l); ehMap = ref (ehMapProj !(n.ehMap) pc);}))


let mergeEvs ev l ev' l' = 
  if ev = ev' then [(ev, None)]
  else [(ev, Some l); (ev', Some l')]


let updFacetNode (newN: node) (l:label option) (oldN: node) =
  if oldN = nullNode then
    ({nid = newN.nid; v = ref (mkFacet !(newN.v) l); ehMap = (newN.ehMap);})
  else if oldN.nid = newN.nid then
    (match l with  
    | None -> newN
    | Some pc -> ({nid = newN.nid; v = ref (updFacet !(newN.v) pc !(oldN.v)); ehMap = (newN.ehMap);}))
  else oldN



(*
  Operations for expression evaluation
*)
let applyOp (o:op) (v1:value) (v2:value) : value =
  match v1 with
  | Num n1 -> (
                match v2 with 
                | Num n2 -> (match o with 
                            | Plus -> Num (n1 + n2)
                            | Minus -> Num (n1 - n2)
                            | And -> Void
                            | Or -> Void
                            | Equals -> Bool (n1 = n2)
                            | NotEquals -> Bool (n1 != n2)  )
                | FacValue fv -> 
                  (match fv.fh, fv.fl with 
                  | Num n2, Num n2' ->
                            (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl= Num (n1 + n2')})
                            | Minus -> FacValue ({fh=Num (n1 - n2); fl= Num (n1 - n2')})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2); fl= Bool (n1 = n2')})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2); fl= Bool (n1 != n2')}) )
                  | Num n2, Void -> 
                            (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl=Void})
                            | Minus -> FacValue ({fh=Num (n1 - n2); fl=Void})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2);fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2);fl= Void}))
                  | Void, Num n2' -> 
                            (match o with 
                            | Plus -> FacValue ({fh=Void; fl=Num (n1 + n2')})
                            | Minus -> FacValue ({fh=Void; fl =Num (n1 - n2')})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Void;fl= Bool (n1 = n2')})
                            | NotEquals -> FacValue ({fh=Void;fl= Bool (n1 != n2')}))
                  | _, _ -> Void)
                | _ -> Void)
  | Bool b1 -> (match v2 with 
                | Bool b2 -> (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> Bool (b1 && b2)
                            | Or -> Bool (b1 || b2)
                            | Equals -> Bool (b1 = b2)
                            | NotEquals -> Bool (b1 != b2))
                | FacValue fv -> 
                  (match fv.fh, fv.fl with 
                  | Bool b2, Bool b2' ->
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Bool (b1 && b2')})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl= Bool (b1 || b2')})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl= Bool (b1 = b2')})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl= Bool (b1 != b2')}) )
                  | Bool b2, Void -> 
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Void})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl=Void})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl= Void})  )
                  | Void, Bool b2' -> 
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Void; fl= Bool (b1 && b2')})
                            | Or -> FacValue ({fh=Void; fl= Bool (b1 || b2')})
                            | Equals -> FacValue ({fh=Bool (b1 = b2'); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2'); fl= Void}))
                  | _, _ -> Void)
                | _ -> Void)
  | FacValue fv ->
    (match fv.fh, fv.fl with 
    | Num n1, Num n1' ->
                (match v2 with 
                | Num n2 -> (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl=Num (n1' + n2)})
                            | Minus -> FacValue ({fh=Num (n1 - n2); fl=Num (n1' - n2)})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2); fl=Bool (n1' = n2)})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2); fl=Bool (n1' != n2)})  )
                | FacValue fv' -> 
                  (match fv'.fh, fv'.fl with 
                  | Num n2, Num n2' ->
                            (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl= Num (n1' + n2')})
                            | Minus -> FacValue ({fh=Num (n1' - n2); fl= Num (n1' - n2')})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2); fl= Bool (n1' = n2')})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2); fl= Bool (n1' != n2')})  )
                  | Num n2, Void -> 
                            (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl= Void})
                            | Minus -> FacValue ({fh=Num (n1 - n2); fl= Void})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2); fl= Void}))
                  | Void, Num n2' -> 
                            (match o with 
                            | Plus -> FacValue ({fh=Void; fl= Num (n1' + n2')})
                            | Minus -> FacValue ({fh=Void; fl= Num (n1' - n2')})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Void; fl=Bool (n1' = n2')})
                            | NotEquals -> FacValue ({fh=Void; fl=Bool (n1' != n2')}))
                  | _, _ -> Void)
                | _ -> Void)
    | Bool b1, Bool b1' ->  
                (match v2 with 
                | Bool b2 -> (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Bool (b1' && b2)})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl= Bool (b1' || b2)})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl= Bool (b1' = b2)})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl= Bool (b1' != b2)}) )
                | FacValue fv' -> 
                  (match fv'.fh, fv'.fl with 
                  | Bool b2, Bool b2' ->
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Bool (b1' && b2')})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl= Bool (b1' || b2')})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl= Bool (b1' = b2')})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl= Bool (b1' != b2')})  )
                  | Bool b2, Void -> 
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Void})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl= Void})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl= Void}) )
                  | Void, Bool b2' -> 
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Void; fl=Bool (b1' && b2')})
                            | Or -> FacValue ({fh=Void; fl=Bool (b1' || b2')})
                            | Equals -> FacValue ({fh=Void;fl= Bool (b1' = b2')})
                            | NotEquals -> FacValue ({fh=Void;fl= Bool (b1' != b2')}) )
                  | _, _ -> Void)
                | _ -> Void)            
    | Num n1, Void ->
                (match v2 with 
                | Num n2 -> (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl= Void})
                            | Minus -> FacValue ({fh=Num (n1 - n2); fl= Void})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2); fl= Void})  )
                | FacValue fv' -> 
                  (match fv'.fh, fv'.fl with 
                  | Num n2, Num n2' ->
                            (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl= Void})
                            | Minus -> FacValue ({fh=Num (n1 - n2); fl= Void})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2); fl= Void})  )
                  | Num n2, Void -> 
                            (match o with 
                            | Plus -> FacValue ({fh=Num (n1 + n2); fl= Void})
                            | Minus -> FacValue ({fh=Num (n1 - n2); fl= Void})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Bool (n1 = n2); fl=Void})
                            | NotEquals -> FacValue ({fh=Bool (n1 != n2); fl=Void}) )
                  | Void, Num n2' -> Void
                  | _, _ -> Void)
                | _ -> Void)
    | Bool b1, Void ->  
                (match v2 with 
                | Bool b2 -> (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Void})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl= Void})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl= Void})  )
                | FacValue fv' -> 
                  (match fv'.fh, fv'.fl with 
                  | Bool b2, Bool b2' ->
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Void})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl= Void})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl= Void})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl= Void})  )
                  | Bool b2, Void -> 
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Bool (b1 && b2); fl= Void})
                            | Or -> FacValue ({fh=Bool (b1 || b2); fl= Void})
                            | Equals -> FacValue ({fh=Bool (b1 = b2); fl=Void})
                            | NotEquals -> FacValue ({fh=Bool (b1 != b2); fl=Void}) )
                  | Void, Bool b2' -> Void
                  | _, _ -> Void)
                | _ -> Void)            
    | Void, Num n1' ->
                (match v2 with 
                | Num n2 -> (match o with 
                            | Plus -> FacValue ({fh=Void; fl=Num (n1' + n2)})
                            | Minus -> FacValue ({fh=Void; fl=Num (n1' - n2)})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Void; fl=Bool (n1' = n2)})
                            | NotEquals -> FacValue ({fh=Void; fl=Bool (n1' != n2)})  )
                | FacValue fv' -> 
                  (match fv'.fh, fv'.fl with 
                  | Num n2, Num n2' ->
                            (match o with 
                            | Plus -> FacValue ({fh=Void; fl=Num (n1' + n2')})
                            | Minus -> FacValue ({fh=Void; fl=Num (n1' - n2')})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Void; fl=Bool (n1' = n2')})
                            | NotEquals -> FacValue ({fh=Void; fl=Bool (n1' != n2')}) )
                  | Num n2, Void -> Void
                  | Void, Num n2' -> 
                            (match o with 
                            | Plus -> FacValue ({fh=Void; fl=Num (n1' + n2')})
                            | Minus -> FacValue ({fh=Void; fl=Num (n1' - n2')})
                            | And -> Void
                            | Or -> Void
                            | Equals -> FacValue ({fh=Void; fl=Bool (n1' = n2')})
                            | NotEquals -> FacValue ({fh=Void; fl=Bool (n1' != n2')}) )
                  | _, _ -> Void)
                | _ -> Void)
    | Void, Bool b1' ->  
                (match v2 with 
                | Bool b2 -> (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Void; fl=Bool (b1' && b2)})
                            | Or -> FacValue ({fh=Void; fl=Bool (b1' || b2)})
                            | Equals -> FacValue ({fh=Void; fl=Bool (b1' = b2)})
                            | NotEquals -> FacValue ({fh=Void; fl=Bool (b1' != b2)})  )
                | FacValue fv' -> 
                  (match fv'.fh, fv'.fl with 
                  | Bool b2, Bool b2' -> 
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Void; fl=Bool (b1' && b2')})
                            | Or -> FacValue ({fh=Void; fl=Bool (b1' || b2')})
                            | Equals -> FacValue ({fh=Void; fl=Bool (b1' = b2')})
                            | NotEquals -> FacValue ({fh=Void; fl=Bool (b1' != b2')})  )
                  | Bool b2, Void -> Void
                  | Void, Bool b2' -> 
                            (match o with 
                            | Plus -> Void
                            | Minus -> Void
                            | And -> FacValue ({fh=Void; fl=Bool (b1' && b2')})
                            | Or -> FacValue ({fh=Void; fl=Bool (b1' || b2')})
                            | Equals -> FacValue ({fh=Void; fl=Bool (b1' = b2')})
                            | NotEquals -> FacValue ({fh=Void; fl=Bool (b1' != b2')}) )
                  | _, _ -> Void)
                | _ -> Void)
    | _, _ -> Void)            
  | _ -> Void


