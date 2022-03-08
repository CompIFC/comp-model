open Labels
open Language
open Dom
open Release
open Store
open Auxfunctions
open Semantics
open Inputoutput

(* Declassification Function *)
module SimpleRM : ReleaseModule = struct
 let counter = ref 0
 let value = ref 0
 (* 
    write the declassify function here 
    returns releaseval and event option
 *)
 let relfunc e = 
    let (i, ev, v) = e in 
    if ev = "click" then
      match v with
      | Num 1 -> (Void, None)
      | Num vl -> (value := !value + vl; 
                  counter := !counter + 1;
                  (if (!counter) = 2 then 
                    (let d = (!value)/(!counter) in 
                    counter := 0; 
                    ((Num d), (Some (i, ev, v))))
                  else (Void, None)))  
      | _ -> (Void, None)
    else if ev = "keypress" then
      match v with
      | Num 101 -> ((Num 101), (Some (i, ev, v)))
      (* | Num _ -> (Void, (Some (i, ev, v))) *)
      | _ -> (Void, (Some (i, ev, v)))
    else
      (Void, None)
end 
;;

let initializeDom (s:(module Store)) = 
  let ri = getNewIdAddr () in
  let n1i = getNewIdAddr () in
  let n2i = getNewIdAddr () in
  let n3i = getNewIdAddr () in
  let n4i = getNewIdAddr () in
  let n5i = getNewIdAddr () in
  let n6i = getNewIdAddr () in
  let n7i = getNewIdAddr () in
  let n8i = getNewIdAddr () in
  let n9i = getNewIdAddr () in
  let n0i = getNewIdAddr () in
  let nni = getNewIdAddr () in
  let domRoot = 
     {nid = ri;
      v = ref (Num 12345);
      ehMap = ref [
      				("keypress",
                        [("k", (If (Bop (Equals, Var "k", Val (Num 42)), AssignVar ("l", Var "k"), Skip)), None)]);
      				("click", 
                        [
                        ("c", (Seq (If (Bop (Equals, Var "l", Val (Num 42)), 
                                            AssignVar ("o", (Val (Num 1))), Skip),
                                    Seq (Output (H, Var "o"), 
                                        Output (L, Var "o")))), None);]);
                    ("mouseover",
                    	[
                    	("c", (Output (L, Var "o")), None);
                    	]);
      			  ];
      } in 
  let node1 = 
     {nid = n1i;
      v = ref (Num 12345);
      ehMap = ref [
      				("keypress",
                        [("k", (AssignVar ("l", Var "k")), None)]);
      				("click", 
                        [
                        ("c", (If (Bop (Equals, Var "l", Val (Num 42)), 
                                            Output (L, Val (Num 0)), Skip)), None);]);
                    ("mouseover",
                    	[
                    	("c", (Output (L, Var "o")), None);
                    	]);
      			  ];
      } in 
  let node2 = 
     {nid = n2i;
      v = ref (Num 12345);
      (* Other event handler examples
      ehMap = ref [("click", [("y", (Seq (Declassify ("x", 0, (Var "x")), Output (L, (Var "x")))), (Some L))])]; 
      ehMap = ref [("click", 
                        [("z", (If ((Var "x"), Output (L, (Val (Num 0))), Output (L, (Val (Num 1))))), (Some L)); 
                        ("y", (AssignVar ("x", (Var "y"))), (Some H))]);
                  ]; *)
      ehMap = ref [("click", 
                        [
                        ("z", (Seq (CreateElement ("h", nni, Val (Num 0)), Output (L, (Val (Num 30))))), None); 
                        ("y", (Seq (AssignNode (nni, (Val (Num 1))), Output (L, Var "h"))), None);
                        ("a", (Seq (AssignVar ("l", (Val (Num 10))), 
                                    Seq (If (Bop (Equals, Var "h", Val (Num 1)), 
                                            AssignVar ("l", (Val (Num 11))), 
                                            AssignVar ("l", (Val (Num 12)))), 
                                        Output (L, Var "l")))), None);
                        ("a", (Seq (AssignVar ("l", (Val (Num 0))), 
                                    Seq (If (Bop (Equals, Var "h", Val (Num 0)), AssignVar ("l", (Val (Num 1))), Skip), 
                                        Seq(If (Bop (Equals, Var "l", Val (Num 1)), AssignVar ("l", (Val (Num 2))), 
                                            AssignVar ("l", (Val (Num 3)))), Output (H, Var "l"))))), None)]);
                   ("keypress",
                        [("y", (Seq (Declassify ("x", 0, (Var "x")), Output (L, (Var "x")))), None)]);
                  ]; 
      } in
  let pwdnode = 
     {nid = n3i;
      v = ref (Num 12345);
      ehMap = ref [("keypress", [(* ("y", If (Bop (NotEquals, Var "l", Val (Num 42)), Output (H, (Var "y")), Skip), None); *)
  								  ("y", Seq ((AssignVar ("p", (Var "y"))),
  								 			(Seq (AssignVar ("i", (Val (Num 0))),
  								 				While (Bop (NotEquals, Var "i", Val (Num 10)),	
  								 					Seq (AssignVar ("i", (Bop (Plus, Var "i", Val (Num 1)))),
		  												Seq (If (Bop (Equals, Var "p", Val (Num 1)),
  																AssignVar ("l", (Val (Num 42))),
  																Skip),
  															If (Bop (Equals, Var "p", Val (Num 2)),
																AssignVar ("l", (Val (Num 22))),
																Skip))))
  												))), None) ]);
                   ("submit", [("y", If (Bop (NotEquals, Var "l", Val (Num 42)), Output (H, (Var "y")), Skip), None);]);
                  ]; 		
      } in     
  let node4 = 
     {nid = n4i;
      v = ref (Num 12345);
      ehMap = ref [];
      } in 
  let node5 = 
     {nid = n5i;
      v = ref (Num 12345);
      ehMap = ref [];
      } in 
  let node6 = 
     {nid = n6i;
      v = ref (Num 12345);
      ehMap = ref [];
      } in 
  let node7 = 
     {nid = n7i;
      v = ref (Num 12345);
      ehMap = ref [];
      } in 
  let node8 = 
     {nid = n8i;
      v = ref (Num 12345);
      ehMap = ref [];
      } in 
  let node9 = 
     {nid = n9i;
      v = ref (Num 12345);
      ehMap = ref [];
      } in 
  let node10 = 
     {nid = n0i;
      v = ref (Num 12345);
      ehMap = ref [];
      } in 
  let module S = (val s) in
  
  (* *** LOW DOM *** *)
  (if S.globalstoretype = Low then 
    (
    S.setValue (Some H) "l" (Num 0) (Some L);
    S.setValue (Some H) "o" (Num 2) (Some L);
    let pbytes = (Gc.allocated_bytes ()) in                   
    S.createNode (Some L) ri domRoot (Some L); 
    S.createNode (Some L) n1i node1 (Some L); 
    S.createNode (Some L) n2i node2 (Some L); 
    S.createNode (Some L) n3i pwdnode (Some H);
    S.createNode (Some L) n4i node4 (Some L); 
    S.createNode (Some L) n5i node5 (Some L); 
    S.createNode (Some L) n6i node6 (Some L); 
    S.createNode (Some L) n7i node7 (Some L); 
    S.createNode (Some L) n8i node8 (Some L); 
    S.createNode (Some L) n9i node9 (Some L); 
    S.createNode (Some L) n0i node10 (Some L); 
    let npbytes = (Gc.allocated_bytes ()) in
    (Printf.printf "Node bytes: %f bytes\n" (npbytes -. pbytes));
    ()
	)
  else if S.globalstoretype = SMS then 
  (* *** SMS DOM  *** *)
    (
    S.setValue (Some L) "l" (Num 0) (Some L);
    S.setValue (Some L) "o" (Num 2) (Some L);
    S.setValue (Some H) "l" (Num 0) (Some H);
    S.setValue (Some H) "o" (Num 2) (Some H);
    let pbytes = (Gc.allocated_bytes ()) in                   
    S.createNode (Some L) ri domRoot (Some L); 
    S.createNode (Some H) ri domRoot (Some L); 
    S.createNode (Some L) n1i node1 (Some L); 
    S.createNode (Some H) n1i node1 (Some L); 
    S.createNode (Some L) n2i node2 (Some L); 
    S.createNode (Some H) n2i node2 (Some L);
    S.createNode (Some L) n3i pwdnode (Some L);
    S.createNode (Some H) n3i pwdnode (Some H);
    S.createNode (Some L) n4i node4 (Some L); 
    S.createNode (Some H) n4i node4 (Some L); 
    S.createNode (Some L) n5i node5 (Some L); 
    S.createNode (Some H) n5i node5 (Some L); 
    S.createNode (Some L) n6i node6 (Some L); 
    S.createNode (Some H) n6i node6 (Some L); 
    S.createNode (Some L) n7i node7 (Some L); 
    S.createNode (Some H) n7i node7 (Some L); 
    S.createNode (Some L) n8i node8 (Some L); 
    S.createNode (Some H) n8i node8 (Some L); 
    S.createNode (Some L) n9i node9 (Some L); 
    S.createNode (Some H) n9i node9 (Some L); 
    S.createNode (Some L) n0i node10 (Some L); 
    S.createNode (Some H) n0i node10 (Some L); 
 	  let npbytes = (Gc.allocated_bytes ()) in
    (Printf.printf "Node bytes: %f bytes\n" (npbytes -. pbytes));
    ()
  )
  else if S.globalstoretype = Faceted then 
  (* *** FACETED DOM *** *)
    (
     S.setValue None "l" (Num 0) (Some L);
    S.setValue None "o" (Num 2) (Some L); 
    let pbytes = (Gc.allocated_bytes ()) in                   
    S.createNode None ri domRoot (Some L); 
    S.createNode None n1i node1 (Some L); 
    S.createNode None n2i node2 (Some L); 
    S.createNode None n3i pwdnode (Some H);
    S.createNode None n4i node4 (Some L); 
    S.createNode None n5i node5 (Some L); 
    S.createNode None n6i node6 (Some L); 
    S.createNode None n7i node7 (Some L); 
    S.createNode None n8i node8 (Some L); 
    S.createNode None n9i node9 (Some L); 
    S.createNode None n0i node10 (Some L); 
    let npbytes = (Gc.allocated_bytes ()) in
    (Printf.printf "Node bytes: %f bytes\n" (npbytes -. pbytes));
    ()
  )
  else 
    (
    S.setValue (Some L) "l" (Num 0) (Some L);
    S.setValue (Some L) "o" (Num 2) (Some L);
    let pbytes = (Gc.allocated_bytes ()) in                   
    S.createNode (Some L) ri domRoot (Some L); 
    S.createNode (Some L) n1i node1 (Some L); 
    S.createNode (Some L) n2i node2 (Some L); 
    S.createNode (Some L) n3i pwdnode (Some H);
    S.createNode (Some L) n4i node4 (Some L); 
    S.createNode (Some L) n5i node5 (Some L); 
    S.createNode (Some L) n6i node6 (Some L); 
    S.createNode (Some L) n7i node7 (Some L); 
    S.createNode (Some L) n8i node8 (Some L); 
    S.createNode (Some L) n9i node9 (Some L); 
    S.createNode (Some L) n0i node10 (Some L); 
    let npbytes = (Gc.allocated_bytes ()) in
    (Printf.printf "Node bytes: %f bytes\n" (npbytes -. pbytes));
    ()
    )
  );  
  initialIds := [ri; n1i; n2i; n3i]@(!initialIds);
  s
;;


let process_events
   (lM: labelContext)
   (g: globalVariables)
   (di: domImpl)
   (execstate: string)
   (i: input list) : (output list * (module Store))
= 
  let r = (module SimpleRM : ReleaseModule) in
  let dCh = (module DecChannel : DChannel) in
  match execstate, di with
  | "SME", SMS ->  let s = (module SMESMS : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i []), s)
  | "SME", Low ->  let s = (module SMELow : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i []), s)
  | "SME", Faceted ->  let s = (module SMEFaceted : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i []), s)
  | "SME", Tainted ->  let s = (module SMETainted : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i []), s)
  | "Faceted", SMS -> let s = (module FacetedSMS : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | "Faceted", Low -> let s = (module FacetedLow : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | "Faceted", Faceted -> let s = (module FacetedFaceted : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | "Faceted", Tainted -> let s = (module FacetedTainted : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | "Tainted", SMS -> let s = (module TaintSMS : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | "Tainted", Low -> let s = (module TaintLow : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | "Tainted", Faceted -> let s = (module TaintFaceted : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | "Tainted", Tainted -> let s = (module TaintTainted : Store) in
              let s = (initializeDom s) in
              (fst (process_io_events lM g r dCh s (Config ([], C, [])) i []), s)
  | _ -> let s = (module TaintTainted : Store) in
         let s = (initializeDom s) in ([], s)
  ;;


let process_next_events
   (lM: labelContext)
   (g: globalVariables)
   (di: domImpl)
   (execstate: string)
   (i: input list) 
   (s: (module Store)) : (output list)
= 
  let r = (module SimpleRM : ReleaseModule) in
  let dCh = (module DecChannel : DChannel) in
  match execstate, di with
  | "SME", SMS ->  
              fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i [])
  | "SME", Low ->  
              fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i [])
  | "SME", Faceted ->  
              fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i [])
  | "SME", Tainted ->  
              fst (process_io_events lM g r dCh s (SMEConfig (Config ([], C, []), Config ([], C, []))) i [])
  | "Faceted", SMS -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i [])
  | "Faceted", Low -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i [])
  | "Faceted", Faceted -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i [])
  | "Faceted", Tainted -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i [])
  | "Tainted", SMS -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i []) 
  | "Tainted", Low -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i []) 
  | "Tainted", Faceted -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i []) 
  | "Tainted", Tainted -> 
              fst (process_io_events lM g r dCh s (Config ([], C, [])) i []) 
  | _ -> []
  ;;

let rec run100Times oc (n:int) (tt:float) =
if n = 0 then (Printf.printf "Execution time: %fs\n" tt; tt)
else	
  let labelMap : labelContext = [(("click", Void), H); (("mouseover", Void), L); (("keypress", Void), H)] in
  let inputlist = [
                  (* Different inputs for different event handler examples
                  (Input (2, "keypress", (Num 42))); 
    		  	   	  (Input (2, "click", (Num 1))); 
  	     			    (Input (1, "mouseover", (Num 1))); 
                       (Input (3, "click", (Num 35))); 
                  (Input (3, "click", (Num 35))); 
                    (Input (3, "keypress", (Num 101)));  *)
                  (Input (4, "keypress", (Num 96)))
                   ] in
  let t = Sys.time() in
  let o,s = process_events labelMap ["l"; "o"] Faceted "Faceted" inputlist in 
  print_output o; 
  let time_taken = (Sys.time() -. t) in 
  Printf.fprintf oc "%fs\n" (Sys.time() -. t);
  (run100Times oc (n - 1) (time_taken +. tt))
;;

let main = 
  let labelMap : labelContext = [(("click", Void), H); (("mouseover", Void), L); (("keypress", Void), H); (("submit", Void), H)] in
  let inputlist = [(Input (4, "keypress", (Num 96)))] in
  let ainputlist = [(Input (4, "submit", (Num 96)))] in
  let rec helper e n l =
	     if n = 0 then l else (helper e (n-1) (e::l)) in
  (* Add number here for the password node *)
  let newinputlist = helper (Input (4, "keypress", (Num 96))) 100 inputlist in 
  let anewinputlist = helper (Input (4, "submit", (Num 96))) 1000 ainputlist in 
  let t = Sys.time() in
  let pbytes = (Gc.allocated_bytes ()) in 
  let o, s = process_events labelMap ["l"; "o"] Tainted "SME" newinputlist in 
  print_output o; 
  let t1 = (Sys.time() -. t) in 
  Printf.printf "Execution time: %fs\n" t1;
  Printf.printf "Allocated bytes: %f bytes\n" (Gc.allocated_bytes () -. pbytes);
  Gc.print_stat stdout; 
;;

main
