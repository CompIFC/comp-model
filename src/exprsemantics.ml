open Labels
open Language
open Dom
open Release
open Store
open Auxfunctions


let globalProj
  (store:(module Store)) 
  (g: globalVariables)
  : (module Store)
= let module S = (val store) in S.reset; store 


let readLocal
  (store:(module Store))
  (pc: label option)
  (x: var)
  : (value option * label option)
= let module S = (val store) in 
  S.read pc x


let readGlobal
  (store:(module Store))
  (pc: label option)
  (x: var)
  : (value option * label option)
= let module S = (val store) in S.getValue pc x


let read
  (g: globalVariables)
  (store:(module Store))
  (pc: label option)
  (x: var)
  : (value option * label option)
= if List.mem x g then readGlobal store pc x
  else readLocal store pc x


let rec getExprValue 
  (g: globalVariables) 
  (store: (module Store)) 
  (pc: label option) 
  (e: exp) 
  : (value * label option) 
= (match e with
  | Val v -> (v, pc)
  | Var y -> let (v, l) = read g store pc y in 
              (match v, l with
              | None, _ -> (Void, pc)
              | Some v', Some l -> (v', Some l)
              | Some v', None -> (v', pc))
  | Bop (oper, e1, e2) -> applyBop g store pc oper e1 e2
  | NodeValue e -> lookupVal store pc (getExprId g store pc e))
and applyBop 
  (g: globalVariables) 
  (store: (module Store)) 
  (pc: label option) 
  (o: op) 
  (e1: exp) 
  (e2: exp) 
  : (value * label option) 
= let (v1, l1) = getExprValue g store pc e1 in 
  let (v2, l2) = getExprValue g store pc e2 in 
  (applyOp o v1 v2, joinOpt l1 l2)
and getExprId
  (g: globalVariables) 
  (store: (module Store)) 
  (pc: label option) 
  (e: exp)
  : value
= let (v, l) = getExprValue g store pc e in v



let convert (v:value * label option)
    (pc:label option)
    (dst:string)
    : (value * label option)
= let (vs, l) = v in
  (match pc with 
  | None -> if dst = "Faceted" then (vs, None) else (valueProj vs L, Some L)
  | Some pcl -> let nv = valueProj vs pcl in
                if dst = "Tainted" then (nv, joinOpt l pc)
                else if labelLessThanEqOpt l pc then (nv, pc)
                else (DV, pc))


let createFct
	(vh: value * label option)
	(vl: value * label option)
	: (value * label option)
=	let (vl', l') = vl in 
	let (vh', h') = vh in
	if labelLessThanEqOpt l' (Some L) then (createFacet vh' vl', None)
	else (createFacet vh' DV, None)


let evalExprValue 
	  (g: globalVariables) 
  	(store: (module Store)) 
  	(pc: label option) 
  	(e: exp) 
  	(dst: string)
  	: (value * label option) 
= let module S = (val store) in
	match pc with 
	| None ->
        if (S.localstoretype = "Faceted") then 
					(let vl = getExprValue g store (Some L) e in
					let vh = getExprValue g store (Some H) e in
					(convert (createFct vh vl) pc dst))
				else (print_endline S.localstoretype; print_endline "Incorrect pc"; convert (Void, pc) pc dst)
	| Some pcl -> (convert (getExprValue g store (Some pcl) e) pc dst)
