open Labels
open Language
open Auxfunctions
open Storeaux


module type Store =
sig
  val localstoretype : string
  val globalstoretype : domImpl
  val reset : unit
  val read : label option -> var -> (value option * label option)
  val assign : label option -> var -> value -> label option -> unit
  val getValue : label option -> var -> (value option * label option)
  val setValue : label option -> var -> value -> label option -> unit
  val getNode : label option -> id -> (node option * label option)
  val createNode : label option -> id -> node -> label option -> unit
  val setNodeValue : label option -> id -> value -> label option -> unit
end
;;


module SMESMS: Store = struct
  let localstoretype = "SME"
  let globalstoretype = SMS
  
  let llowmem : (lcell list) ref = ref []

  let lhighmem : (lcell list) ref = ref []

  let glowmem : (scell list) ref = ref []

  let ghighmem : (scell list) ref = ref []

  let reset = llowmem := []; lhighmem := []

  let read l x = 
      match l with 
      | Some L -> (readSMEVal (!llowmem) x, None)
      | Some H -> (readSMEVal (!lhighmem) x, None)
      | _ -> (None, None)

  let assign pc x v l = 
      match pc with  
      | Some L -> llowmem := updateSME !llowmem x v
      | Some H -> lhighmem := updateSME !lhighmem x v
      | _ -> ()

  let getValue l x =
      (match l with  
      | Some L -> (getValueInStore (!glowmem) x, l)
      | Some H -> (getValueInStore (!ghighmem) x, l)
      | _ -> (None, None))

  let setValue pc x v l =
      (match pc with  
      | Some L -> (* let pbytes = (Gc.allocated_bytes ()) in  *)
                  glowmem := updateValueInStore !glowmem x v;
                  (* let npbytes = (Gc.allocated_bytes ()) in
                  (Printf.printf "Used bytes: %f bytes\n" (npbytes -. pbytes)); *)
                  ()  
      | Some H -> (* let pbytes = (Gc.allocated_bytes ()) in  *)
                  ghighmem := updateValueInStore !ghighmem x v;
                  (* let npbytes = (Gc.allocated_bytes ()) in
                  (Printf.printf "Used bytes: %f bytes\n" (npbytes -. pbytes)); *)
                  ()
      | _ -> ())

  let getNode l i =
      (match l with  
      | Some L -> (getNodeInStore (!glowmem) i, None)
      | Some H -> (getNodeInStore (!ghighmem) i, None)
      | _ -> (None, None))

  let createNode l i n ol =
    if n = nullNode then ()
    else (match l with  
          | Some L -> glowmem := updateNodeInStore !glowmem i n
          | Some H -> ghighmem := updateNodeInStore !ghighmem i n
          | _ -> ())

  let setNodeValue pc i v l = 
      (match pc with  
      | Some L -> glowmem := updateNodeValueInStore !glowmem i v
      | Some H -> ghighmem := updateNodeValueInStore !ghighmem i v
      | _ -> ())

end
;;


module SMELow: Store = struct
  let localstoretype = "SME"
  let globalstoretype = Low
  
  let sharedmem : (scell list) ref = ref []

  let llowmem : (lcell list) ref = ref []

  let lhighmem : (lcell list) ref = ref []

  let reset = llowmem := []; lhighmem := []

  let read l x = 
      match l with 
      | Some L -> (readSMEVal (!llowmem) x, None)
      | Some H -> (readSMEVal (!lhighmem) x, None)
      | _ -> (None, None)

  let assign pc x v l = 
      match pc with  
      | Some L -> llowmem := updateSME !llowmem x v
      | Some H -> lhighmem := updateSME !lhighmem x v
      | _ -> ()

  let getValue pc x =
      if labelLessThanEqOpt pc (Some L) then (Some DV, None) else (getValueInStore (!sharedmem) x, None)

  let setValue pc x v l =
      (* let pbytes = (Gc.allocated_bytes ()) in *)
      (if labelLessThanEqOpt pc (Some L) then () else sharedmem := updateValueInStore !sharedmem x v);
      (* let npbytes = (Gc.allocated_bytes ()) in *)
      (* (Printf.printf "Used bytes: %f bytes\n" (npbytes -. pbytes)); *)
      ()  

  let getNode pc i = (getNodeInStore (!sharedmem) i, None)
      
  let createNode pc i n ol =
      if n = nullNode then ()
      else if labelLessThanEqOpt pc (Some L) then sharedmem := updateNodeInStore !sharedmem i n 
      else ()
(*      else if pc = Some H && getNode d l i != (None, None) then sharedmem := updateNodeInStore !sharedmem i n 
        else if pc = Some L && getNode d l i = (None, None) then sharedmem := updateNodeInStore !sharedmem i n 
        else ()
 *)   
  let setNodeValue pc i v l = 
      if labelLessThanEqOpt pc (Some L) then () else sharedmem := updateNodeValueInStore !sharedmem i v 

end
;;


module SMEFaceted: Store = struct
  let localstoretype = "SME"
  let globalstoretype = Faceted
  
  let sharedmem : (scell list) ref = ref []

  let llowmem : (lcell list) ref = ref []

  let lhighmem : (lcell list) ref = ref []

  let reset = llowmem := []; lhighmem := []

  let read l x = 
      match l with 
      | Some L -> (readSMEVal (!llowmem) x, None)
      | Some H -> (readSMEVal (!lhighmem) x, None)
      | _ -> (None, None)

  let assign pc x v l = 
      match pc with  
      | Some L -> llowmem := updateSME !llowmem x v
      | Some H -> lhighmem := updateSME !lhighmem x v
      | _ -> ()

  let getValue l x = (getFacetedValueInStore l (!sharedmem) x, None)

  let setValue pc x v l = 
    (* let pbytes = (Gc.allocated_bytes ()) in  *)
    sharedmem := updateFacetedValueInStore (!sharedmem) pc x v;
    (* let npbytes = (Gc.allocated_bytes ()) in *)
    (* (Printf.printf "Used bytes: %f bytes\n" (npbytes -. pbytes)); *)
    ()

  let getNode l i = (getFacetedNodeInStore (!sharedmem) l i, None)

  let createNode l i n ol =
    (* let pbytes = (Gc.allocated_bytes ()) in  *)
    (if n = nullNode then ()
    else sharedmem := updateFacetedNodeInStore (!sharedmem) l i n);
    (* let npbytes = (Gc.allocated_bytes ()) in *)
    (* (Printf.printf "Used node bytes: %f bytes\n" (npbytes -. pbytes)); *)
    ()

  let setNodeValue pc i v l = sharedmem := updateFacetedNodeValueInStore (!sharedmem) pc i v

end
;;


module SMETainted: Store = struct
  let localstoretype = "SME"
  let globalstoretype = Tainted
  
  let sharedmem : (tscell list) ref = ref []

  let llowmem : (lcell list) ref = ref []

  let lhighmem : (lcell list) ref = ref []

  let reset = llowmem := []; lhighmem := []

  let read l x = 
      match l with 
      | Some L -> (readSMEVal (!llowmem) x, None)
      | Some H -> (readSMEVal (!lhighmem) x, None)
      | _ -> (None, None)

  let assign pc x v l = 
      match pc with  
      | Some L -> llowmem := updateSME !llowmem x v
      | Some H -> lhighmem := updateSME !lhighmem x v
      | _ -> ()

  let getValue l i = getTaintedValueInStore (!sharedmem) l i

  let setValue pc x v l = 
      (* let pbytes = (Gc.allocated_bytes ()) in *)
      sharedmem := updateTaintedValueInStore (!sharedmem) x v (joinOpt pc l);
      (* let npbytes = (Gc.allocated_bytes ()) in *)
      (* (Printf.printf "Used bytes: %f bytes\n" (npbytes -. pbytes)); *)
      ()  

  let getNode l i = getTaintedNodeInStore (!sharedmem) i l

  let createNode l i n ol =
    if n = nullNode then ()
    else (match l, ol with 
          | Some pc, Some ol -> sharedmem := updateTaintedNodeInStore (!sharedmem) i n (join pc ol)
          | _, _ -> ())

  let setNodeValue pc i v l = sharedmem := updateTaintedNodeValueInStore (!sharedmem) pc i v l

end
;;


module FacetedTainted: Store = struct
  let localstoretype = "Faceted"
  let globalstoretype = Tainted

  let sharedmem : (tscell list) ref = ref []

  let localmem : (lcell list) ref = ref []

  let reset = localmem := []

  let read pc x = readFacetedVal pc (!localmem) x 

  let assign pc x v l = localmem := updateFacet !localmem pc x v

   let getValue l i = getTaintedValueInStore (!sharedmem) l i

  let setValue pc x v l = sharedmem := updateTaintedValueInStore (!sharedmem) x v (joinOpt pc l)

  let getNode l i = getTaintedNodeInStore (!sharedmem) i l

  let createNode l i n ol =
    if n = nullNode then ()
    else (match l, ol with 
          | Some pc, Some ol -> sharedmem := updateTaintedNodeInStore (!sharedmem) i n (join pc ol)
          | _, _ -> ())

  let setNodeValue pc i v l = sharedmem := updateTaintedNodeValueInStore (!sharedmem) pc i v l

end
;;


module FacetedSMS: Store = struct
  let localstoretype = "Faceted"
  let globalstoretype = SMS

  let localmem : (lcell list) ref = ref []

  let glowmem : (scell list) ref = ref []

  let ghighmem : (scell list) ref = ref []

  let reset = localmem := []

  let read pc x = readFacetedVal pc (!localmem) x 

  let assign pc x v l = localmem := updateFacet !localmem pc x v

  (* let setValue pc x v l = 
      (match pc with  
               | Some L -> let v' = (valueProj v L) in 
                            if v' = Void then () else lowmem := updateValueInStore !lowmem x v'
               | Some H -> let v' = (valueProj v H) in 
                            if v' = Void then () else highmem := updateValueInStore !highmem x v'
               | None ->  lowmem := updateValueInStore !lowmem x (valueProj v L); 
                          highmem := updateValueInStore !highmem x (valueProj v H)
               | _ -> ()) *)
      
  let getValue l x =
      (match l with  
      | Some L -> (getValueInStore (!glowmem) x, l)
      | Some H -> (getValueInStore (!ghighmem) x, l)
      | _ -> (None, None))

  let setValue pc x v l =
      (match pc with  
      | Some L -> glowmem := updateValueInStore !glowmem x v
      | Some H -> ghighmem := updateValueInStore !ghighmem x v
      | _ -> ())

  let getNode l i =
      (match l with  
      | Some L -> (getNodeInStore (!glowmem) i, None)
      | Some H -> (getNodeInStore (!ghighmem) i, None)
      | _ -> (None, None))

  let createNode l i n ol =
      if n = nullNode then ()
      else (match l with  
          | Some L -> glowmem := updateNodeInStore !glowmem i n
          | Some H -> ghighmem := updateNodeInStore !ghighmem i n
          | _ -> ())

  let setNodeValue pc i v l = 
      (match pc with  
      | Some L -> glowmem := updateNodeValueInStore !glowmem i v
      | Some H -> ghighmem := updateNodeValueInStore !ghighmem i v
      | _ -> ())

end
;;


module FacetedLow: Store = struct 
  let localstoretype = "Faceted"
  let globalstoretype = Low

  let localmem : (lcell list) ref = ref []

  let sharedmem : (scell list) ref = ref []

  let reset = localmem := []

  let read pc x = readFacetedVal pc (!localmem) x 

  let assign pc x v l = localmem := updateFacet !localmem pc x v

  let getValue pc x =
      if labelLessThanEqOpt pc (Some L) then (Some DV, None) else (getValueInStore (!sharedmem) x, None) 

  let setValue pc x v l =
      if labelLessThanEqOpt pc (Some L) then () else sharedmem := updateValueInStore !sharedmem x v 

  let getNode pc i = (getNodeInStore (!sharedmem) i, None)
      
  let createNode pc i n ol =
      if n = nullNode then ()
      else if labelLessThanEqOpt pc (Some L)  then sharedmem := updateNodeInStore !sharedmem i n 
      else ()

  let setNodeValue pc i v l = 
      if labelLessThanEqOpt pc (Some L) then () else sharedmem := updateNodeValueInStore !sharedmem i v 

end
;;


module FacetedFaceted: Store = struct

  let localstoretype = "Faceted"
  let globalstoretype = Faceted

  let sharedmem : (scell list) ref = ref []

  let localmem : (lcell list) ref = ref []

  let reset = localmem := []

  let read pc x = readFacetedVal pc (!localmem) x 

  let assign pc x v l = localmem := updateFacet !localmem pc x v

  let getValue l x = (getFacetedValueInStore l (!sharedmem) x, None)

  let setValue pc x v l =       
  (*  let pbytes = (Gc.allocated_bytes ()) in *)
      sharedmem := updateFacetedValueInStore (!sharedmem) pc x v;
      (* let npbytes = (Gc.allocated_bytes ()) in *)
      (* (Printf.printf "Used bytes: %f bytes\n" (npbytes -. pbytes)); *)
      ()  


  let getNode l i = (getFacetedNodeInStore (!sharedmem) l i, None)

  let createNode l i n ol =
    if n = nullNode then ()
    else sharedmem := updateFacetedNodeInStore (!sharedmem) l i n

  let setNodeValue pc i v l = sharedmem := updateFacetedNodeValueInStore (!sharedmem) pc i v

end
;;


module TaintSMS: Store = struct

  let localstoretype = "Tainted"
  let globalstoretype = SMS

  let localmem : (tcell list) ref = ref []

  let glowmem : (scell list) ref = ref []

  let ghighmem : (scell list) ref = ref []

  let reset = localmem := []

  let read l x = readTaintedVal (!localmem) l x

  let assign pc x v l = updateTaint pc localmem x v l

   let getValue l x =
      (match l with  
      | Some L -> (getValueInStore (!glowmem) x, l)
      | Some H -> (getValueInStore (!ghighmem) x, l)
      | _ -> (None, None))

  let setValue pc x v l =
      (match pc with  
      | Some L -> glowmem := updateValueInStore !glowmem x v
      | Some H -> ghighmem := updateValueInStore !ghighmem x v
      | _ -> ())

  let getNode l i =
      (match l with  
      | Some L -> (getNodeInStore (!glowmem) i, None)
      | Some H -> (getNodeInStore (!ghighmem) i, None)
      | _ -> (None, None))

  let createNode l i n ol =
    if n = nullNode then ()
    else (match l with  
          | Some L -> glowmem := updateNodeInStore !glowmem i n
          | Some H -> ghighmem := updateNodeInStore !ghighmem i n
          | _ -> ())

  let setNodeValue pc i v l = 
      (match pc with  
      | Some L -> glowmem := updateNodeValueInStore !glowmem i v
      | Some H -> ghighmem := updateNodeValueInStore !ghighmem i v
      | _ -> ())
    
end
;;


module TaintLow: Store = struct
  
  let localstoretype = "Tainted"
  let globalstoretype = Low

  let sharedmem : (scell list) ref = ref []

  let localmem : (tcell list) ref = ref []

  let reset = localmem := []

  let read l x = readTaintedVal (!localmem) l x

  let assign pc x v l = updateTaint pc localmem x v l

  let getValue pc x =
      if labelLessThanEqOpt pc (Some L) then (Some DV, None) else (getValueInStore (!sharedmem) x, None) 

  let setValue pc x v l =
      if labelLessThanEqOpt pc (Some L) then () else sharedmem := updateValueInStore !sharedmem x v 

  let getNode pc i = (getNodeInStore (!sharedmem) i, None)
      
  let createNode pc i n ol =
      if n = nullNode then ()
      else if labelLessThanEqOpt pc (Some L)  then sharedmem := updateNodeInStore !sharedmem i n 
      else ()
      
  let setNodeValue pc i v l = 
      if labelLessThanEqOpt pc (Some L) then () else sharedmem := updateNodeValueInStore !sharedmem i v 

end
;;


module TaintFaceted: Store = struct
   
  let localstoretype = "Tainted"
  let globalstoretype = Faceted

  let sharedmem : (scell list) ref = ref []

  let localmem : (tcell list) ref = ref []

  let reset = localmem := []

  let read l x =  readTaintedVal (!localmem) l x

  let assign pc x v l = updateTaint pc localmem x v l

  let getValue l x = (getFacetedValueInStore l (!sharedmem) x, None)

  let setValue pc x v l = sharedmem := updateFacetedValueInStore (!sharedmem) pc x v

  let getNode l i = (getFacetedNodeInStore (!sharedmem) l i, None)

  let createNode l i n ol =
    if n = nullNode then ()
    else sharedmem := updateFacetedNodeInStore (!sharedmem) l i n
      
  let setNodeValue pc i v l = sharedmem := updateFacetedNodeValueInStore (!sharedmem) pc i v

end
;;


module TaintTainted: Store = struct
  let localstoretype = "Tainted"
  let globalstoretype = Tainted

  let sharedmem : (tscell list) ref = ref []

  let localmem : (tcell list) ref = ref []

  let reset = localmem := []
  
  let read l x = readTaintedVal (!localmem) l x

  let assign pc x v l = updateTaint pc localmem x v l

  let getValue l i = getTaintedValueInStore (!sharedmem) l i

  let setValue pc x v l = 
      (* let pbytes = (Gc.allocated_bytes ()) in *)
      sharedmem := updateTaintedValueInStore (!sharedmem) x v (joinOpt pc l);
      (* let npbytes = (Gc.allocated_bytes ()) in *)
      (* (Printf.printf "Used bytes: %f bytes\n" (npbytes -. pbytes)); *)
      ()  

  let getNode l i = getTaintedNodeInStore (!sharedmem) i l

  let createNode l i n ol =
    if n = nullNode then ()
    else (match l, ol with 
          | Some pc, Some ol -> sharedmem := updateTaintedNodeInStore (!sharedmem) i n (join pc ol)
          | _, _ -> ())

  let setNodeValue pc i v l = sharedmem := updateTaintedNodeValueInStore (!sharedmem) pc i v l

end
;;
