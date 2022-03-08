open Labels
open Language
open Dom
open Release
open Store
open Auxfunctions
open Exprsemantics

let assignLocal
  (store:(module Store))
  (pc: label option)
  (x: var)
  (v: value)
  (l: label option)
  : (module Store)
= let module S = (val store) in S.assign pc x v l; store 

let assignGlobal
  (store:(module Store))
  (pc: label option)
  (x: var)
  (v: value)
  (l: label option)
  : (module Store)
= let module S = (val store) in S.setValue pc x v l; store 

let assign
  (g: globalVariables)
  (store:(module Store))
  (pc: label option)
  (x: var)
  (v: value)
  (l: label option)
  : (module Store)
= if List.mem x g then assignGlobal store pc x v l
  else assignLocal store pc x v l

(*
 Local Evaluation
 II B 2
 *)
let rec local_evaluation
   (g: globalVariables)
   (pc: label option)
   (dCh: (module DChannel))
   (store: (module Store))
   (cmd: command)
   : ((value * label) option * (module Store) * command * ((event * label option) list))
= let module S = (val store) in 
  let stype = S.localstoretype in
  let di = S.globalstoretype in 
  match cmd with
  | Seq (Skip, c) -> (None, store, c, []) (* Skip *)
  | Seq (c1, c2) -> let (out, store', c1', e) = local_evaluation g pc dCh store c1 in
                        (out, store', Seq (c1', c2), e) (* Seq *)
  | AssignVar (x, e) ->  let (v, l) = (if List.mem x g then evalExprValue g store pc e (stringOfDI di) 
  										                else evalExprValue g store pc e stype) in
                         let store' = assign g store pc x v l in   
                         (None, store', Skip, [])
  | AssignNode (id, e) -> let (v, l) = evalExprValue g store pc e (stringOfDI di) in
                          (None, (S.setNodeValue pc id v l; store), Skip, [])
  | CreateElement (x, i, e) -> 
                      let (v, l) = evalExprValue g store pc e (stringOfDI di) in 
                      let store' = createElem store pc i v l in
                      let store'' = assign g store' pc x (Id i) None in     
                      (None, store'', Skip, [])
  | Dispatch (x, ev, e) -> 
                      let (va, _) = read g store pc x in 
                      let ehl = (match va with 
                                | Some (Id a) ->  let (v, l') = evalExprValue g store pc e (stringOfDI di) in
                                                  (trigger store (joinOpt pc l') a ev v)   
                                | _ -> []) in
                      (None, store, Skip, ehl)
  | AddHandler (_, x, ev, y, c) -> 
                      let (v, _) = read g store pc x in 
                      (match v with 
                      | (Some (Id a)) ->  let store' = addEH store pc a ev (y, c, pc) in   
                                          (None, store', Skip, [])
                      | _ -> (None, store, Skip, []))
  | If (e, c1, c2) -> (match evalExprValue g store pc e stype with
                      | ((Bool true), _) -> (None, store, c1, []) (* If-True *)
                      | ((Bool false), _) -> (None, store, c2, [])  (* If-False *)
                      | ((Num 0), _) -> (None, store, c2, [])  (* If-False *)
                      | ((Num _), _) -> (None, store, c1, [])  (* If-True *)
                      | (FacValue fv, _) -> 
                            let c1' = (if (fv.fh = Bool false || fv.fh = Num 0) then c2 else c1) in 
                            let c2' = (if (fv.fl = Bool false || fv.fl = Num 0) then c2 else c1) in 
                            (None, store, FacCmd (c1', c2'), [])
                      | _ -> (None, store, Skip, []))
  | While (e, c) -> (match evalExprValue g store pc e stype with
                    | ((Bool true), _) -> (None, store, Seq (c, While (e, c)), []) (* While-True *)
                    | ((Bool false), _) -> (None, store, Skip, [])  (* While-False *)
                    | ((Num 0), _) -> (None, store, Skip, [])  (* While-False *)
                    | ((Num _), _) -> (None, store, Seq (c, While (e, c)), [])  (* While-True *)
                    | (FacValue fv, _) -> 
                            let c1' = (if (fv.fh = Bool false || fv.fh = Num 0) then Skip else Seq (c, While (e, c))) in 
                            let c2' = (if (fv.fl = Bool false || fv.fl = Num 0) then Skip else Seq (c, While (e, c))) in 
                            (None, store, FacCmd (c1', c2'), [])
                    | _ -> (None, store, Skip, []))
  | Output (l, e) ->  let (v, l') = (match evalExprValue g store pc e stype with
                                    | (FacValue fv, l') -> (valueProj (FacValue fv) l, Some l)
                                    | (v', l') -> (v', l')) in
                      (if labelLessThanEqOpt (joinOpt pc l') (Some l) then 
                            (Some (v, l), store, Skip, [])
                      else  (None, store, Skip, []))
  | Declassify (x, i, e) -> (match pc with 
                            | Some H -> (None, store, AssignVar (x, e), [])
                            | _ ->  (let module D = (val dCh) in
                                    (match D.read i with
                                    | None -> if stype = "Faceted" && pc = None then (None, store, setFacet (AssignVar (x, e)) Skip, [])
                                              else (None, store, Skip, [])
                                    | Some v -> if stype = "Faceted" && pc = None then 
                                                    let (v', l) = evalExprValue g store pc e stype in 
                                                    let (o, s, f, e) = (None, store, setFacet (AssignVar (x, Val v')) (AssignVar (x, Val v)), []) in
                                                    (o, s, f, e)
                                                else (None, store, AssignVar (x, Val v), []))))
  | _ -> (None, store, Skip, [])



let rec findCmd (ch:command * label option) (cl:(command * label option) list) (dst:string) =
  let (c, l) = ch in 
  match cl with 
  | [] -> ((c, l), [])
  | (c1, l1)::tl -> if c = c1 && dst = "Faceted" then ((c, None), tl)
                    else if c = c1 && dst = "Tainted" then ((c, Some L), tl)
                    else (let (ch', t) = findCmd (c, l) tl dst in
                      (ch', (c1, l1)::t))


let rec mergeK kh kl s = 
  if s = "SME" then SMEConfig (kh, kl)
  else if s = "Faceted" || s = "Tainted" then 
    (match kh, kl with 
    | Config (ch, sh, eh), Config (cl, sl, el) ->
      (match ch with  | [] -> Config (cl, sl, el)
              | ch'::chlist -> let (c, cl') = findCmd ch' cl s in 
                       let k = mergeK (Config (chlist, sh, eh)) (Config (cl', sl, el)) s in
                       (match k with 
                       | Config (c1, s1, e1) -> Config (c::c1, s1, e1)
                       | _ ->  (print_endline "Found SMEConfig error in mergeK execution!"; kh))
      )  
    | _, _ -> (print_endline "Weird merge error in faceted/tainted execution of mergeK!"; kh)
    )
  else (print_endline "Weird merge error!"; kh)


let rec lookupEHAt (store: (module Store)) (pc:label option) (ev:event) (k:config) : config = 
  let module S = (val store) in
  match k with 
  | SMEConfig (kh, kl) ->   let (kh', kl') = (match pc with   
                | Some L -> (kh, lookupEHAt store pc ev kl)
                | Some H -> (lookupEHAt store pc ev kh, kl)
                | None -> (lookupEHAt store (Some H) ev kh, lookupEHAt store (Some L) ev kl)
                | _ -> print_endline "Error in lookupEHAt SME"; (kh, kl)) in 
                SMEConfig (kh', kl')
  | Config (c, s, e) -> (match pc with 
              | None -> let kh' = lookupEHAt store (Some H) ev k in
                        let kl' = lookupEHAt store (Some L) ev k in
                        mergeK kh' kl' (S.localstoretype)
              | _ -> let c' = lookupEH store pc ev in Config (c'@c, P, e))


let rec lookupEHAll (store: (module Store)) (pc:label option) (ev:event) (k:config) : config = 
  let module S = (val store) in
  match k with 
  | SMEConfig (kh, kl) ->   let (kh', kl') = (match pc with   
                  | Some L -> (kh, lookupEHAll store pc ev kl)
                  | Some H -> (lookupEHAll store pc ev kh, kl)
                  | None -> (lookupEHAll store (Some H) ev kh, lookupEHAll store (Some L) ev kl)
                  | _ -> print_endline "Error in lookupEHAll SME"; (kh, kl)) in 
                SMEConfig (kh', kl')
  | Config (c, s, e) -> (match pc, S.globalstoretype with 
              | None, Faceted -> 
                              if S.localstoretype = "Faceted" then
                                let c' = lookupEH store pc ev in Config (c'@c, P, e)
                              else
                                let kl' = lookupEHAll store (Some L) ev k in
                                let kh' = lookupEHAll store (Some H) ev k in
                                mergeK kh' kl' (S.localstoretype)  
              | _, Faceted -> let c' = lookupEH store pc ev in Config (c'@c, P, e)
              | None, _ ->  let kl' = lookupEHAll store (Some L) ev k in
                            let kh' = lookupEHAll store (Some H) ev k in
                            mergeK kh' kl' (S.localstoretype)
              | _, _ -> let c' = lookupEH store pc ev in Config (c'@c, P, e))


let lookupEHAllConfig 
  (k:config) 
  (store: (module Store)) 
  (pc:label option) 
  (ev:event) 
  : config  
= lookupEHAll store pc ev k


let lookupEHAtConfig 
  (k:config)
  (store: (module Store)) 
  (pc:label option) 
  (ev:event) 
  : config  
= lookupEHAt store pc ev k


let rec lookupEHs 
  (store: (module Store)) 
  (pc:label option) 
  (ev:(event * label option) list) 
  (k:config) : config  
= let module S = (val store) in 
  match ev with
  | [] -> (match k with 
        | Config ([], P, []) -> Config ([], C, [])
        | Config (clist, P, []) -> Config (clist, P, [])
        | _ -> k)
  | (e, l)::ev' ->  
        let k' = (if S.localstoretype = "Tainted" then (match pc with | None | Some L -> lookupEHAllConfig k store None e | _ -> lookupEHAllConfig k store (Some H) e)
                  else lookupEHAtConfig k store (joinOpt pc l) e) in
        lookupEHs store pc ev' k'



(*
 Single Execution Semantics
 *)
let rec single_step
   (g: globalVariables)
   (dCh: (module DChannel))
   (store: (module Store))
   (kappa: config)
   : ((value * label) option * label option * (module Store) * config)
=
match kappa with
| Config ([], P, []) -> (None, None, store, Config ([], C, []))
| Config (((Skip, p)::clist), P, []) -> 
            if clist != [] then (* Clear-Locals *)
              (None, p, globalProj store g, Config (clist, P, []))
            else (* PtoC *)
              (None, p, globalProj store g, Config ([], C, []))
| Config (((Skip, p)::clist), P, ev) -> (* LC *)
            let k' = lookupEHs store p ev (Config (clist, P, [])) in
            (None, p, globalProj store g, k')
| Config (((cmd, p)::clist), P, ev) -> (* P *)
      (match cmd, p with 
      | Seq ((FacCmd (ch, Skip)), c2), None ->
                          let k' = lookupEHs store (Some L) ev (Config ((ch, Some H)::(c2, None)::clist, P, [])) in
                          (None, (Some L), globalProj store g, k')
      | FacCmd (ch, Skip), None -> 
                          let k' = lookupEHs store (Some L) ev (Config ((ch, Some H)::clist, P, [])) in
                          (None, (Some L), globalProj store g, k')
      | Seq ((FacCmd (ch, cl)), c2), None ->
                          let (o, store', cmd', ev') = local_evaluation g (Some L) dCh store cl in
                          (o, (Some L), store', Config ((Seq (FacCmd (ch, cmd'), c2), p)::clist, P, ev@ev'))
      | FacCmd (ch, cl), None -> let (o, store', cmd', ev') = local_evaluation g (Some L) dCh store cl in
                    (o, (Some L), store', Config ((FacCmd (ch, cmd'), p)::clist, P, ev@ev'))
      | _, _ -> let (o, store', cmd', ev') = local_evaluation g p dCh store cmd in
                (o, p, store', Config ((cmd', p)::clist, P, ev@ev')))
| SMEConfig (kh, kl) ->
      if isProducer kl then 
        let (o, p, store', k') = single_step g dCh store kl in
        (o, Some L, store', SMEConfig (kh, k'))
      else if isProducer kh then 
        let (o, p, store', k') = single_step g dCh store kh in
        (o, Some H, store', SMEConfig (k', kl))
      else 
        (print_endline "SME Error?";  (None, None, store, kappa))
| _ -> (print_endline "Error";  (None, None, store, kappa))
