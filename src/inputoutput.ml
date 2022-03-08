open Labels
open Language
open Dom
open Release
open Store
open Auxfunctions
open Exprsemantics
open Semantics

(*
 Top-level Input Rules
 *)
let input_step
   (lM: labelContext)
   (g: globalVariables)
   (r: (module ReleaseModule))
   (dCh: (module DChannel))
   (store: (module Store))
   (i: input)
   (k: config)
   : config
= let (Input ev) = i in
    (match (lookupLab ev lM) with
    | H ->  let module R = (val r) in
            (match (R.relfunc ev) with
            | (dv, None) ->    (* I-NR1 *)
                      	((* print_endline "I - NR"; *)
                        let module DC = (val dCh) in
                      	let k' = lookupEHAllConfig k store (Some H) ev in
                        DC.update dv; k')
            | (dv, Some ev') -> (* I-R *)
                     	((* print_endline "I - R"; *)
                      	let module DC = (val dCh) in
                     	(DC.update dv;
                        (if ev = ev' then (lookupEHAllConfig k store None ev)
                        else (let k1 = lookupEHAtConfig k store (Some H) ev in 
                              lookupEHAtConfig k1 store (Some L) ev'))
                        )))
    | HD -> (* I-NR2 *)
            ((* print_endline "I - NR2"; *)
            lookupEHAllConfig k store (Some H) ev)
    | L -> (* MFI-L *)
            ((* print_endline "I - L"; *)
            lookupEHAllConfig k store None ev))
;;


(*
 Top-level Output Rules	
 *)
let rec output_step
   (lM: labelContext)
   (g: globalVariables)
   (r: (module ReleaseModule))
   (dCh: (module DChannel))
   (store: (module Store))
   (kappa: config)
   : (value option * ((module ReleaseModule) * (module DChannel) * (module Store) * config))
=
if isProducer kappa then 
	let (o, pc, store', kappa') = single_step g dCh store kappa in 
	let module S = (val store) in
	if (outCondition S.localstoretype o pc) then 
		(getOutput o pc, (r, dCh, store', kappa'))
	else (None, (r, dCh, store', kappa'))
else 
  (None, (r, dCh, store, kappa))


let rec process_io_events 
   (lM: labelContext)
   (g: globalVariables)
   (r: (module ReleaseModule))
   (dCh: (module DChannel))
   (store: (module Store))
   (kappa: config)
   (i: input list) 
   (ol:output list)
   : (output list * ((module ReleaseModule) * (module DChannel) * (module Store) * config))
= match kappa, i with
  | Config ([], C, []), []
  | SMEConfig (Config ([], C, []), Config ([], C, [])), [] -> (ol, (r, dCh, store, kappa))
  | Config ([], C, []), (ih::itl)
  | SMEConfig (Config ([], C, []), Config ([], C, [])), (ih::itl) ->          
           			   	let k' = input_step lM g r dCh store ih kappa in
              			process_io_events lM g r dCh store k' itl ol
  | _, _ ->  let (o, (r', dCh', store', k')) = output_step lM g r dCh store kappa in 
             match getOutputValue o with 
              | None -> process_io_events lM g r' dCh' store' k' i ol
              | Some out -> process_io_events lM g r' dCh' store' k' i (ol@[out])
;;