module Browser


open BrowserIO
open Assumed
open FStar.List.Tot.Base


(* --internal browser structures --*)


(* a type to represent address of a window *)
type win_ref = nat

(** Generates a fresh [win_ref]. *)

let fresh_win_ref (win:win_ref) : win_ref = win + 1



(** A typf eor referencing a "page", which is a window's contents at a point
      in time. *)
type page_ref = nat

(** Generates a fresh [page_ref]. *)
let fresh_page_ref (page:page_ref): page_ref = page + 1

(** A type representing the address of a document node in memory. *)
type node_ref = nat

(** Generates a fresh [node_ref]. *)
let fresh_node_ref (nref:node_ref): node_ref = nref + 1

(** A type representing the address of an activation record in memory. *)
type act_ref = nat

 (** Generates a fresh [act_ref]. *)
let fresh_act_ref (act:act_ref): act_ref = act + 1


(** The type of data that indexes cookies. *)
type cookie_id = {
    cookie_id_domain: domain;
    cookie_id_path: path;
    cookie_id_key: string;
}
  (** A type of a window's "name" property. *)
  type win_name =
    | No_name
    | Str_name of string

  (** A type for a window's "opener" property. *)
  type win_opener =
    | No_opener
    | Win_opener of win_ref

(** The type of a window. *)
type win = {
    win_name: win_name;
    (** The window's name. *)
    win_opener: win_opener;
    (** The window's opener. If window is opened by user then no window_ref. Else reference to window from where window is opened*)
    win_page: page_ref;
    (** The window's page. *)
}




(* internal expressions and running browser states *)
(** A type for the static context of an expression. *)
type context={
    context_win: win_ref;
    context_act: act_ref;
}

(** A type for the additional constructs in the internal language of
    * expressions. *)

type inner=
| Scoped_expr of context * expr inner
      (** [Scoped_expr(cxt, e)] is a subexpression that executes in a different
          context than the enclosing expression. *)
| R of value
    (** A final result. *)


(* A type for the results of evaluating script expressions. *)

and value=
| Closure: context -> var -> list var -> expr inner->value
(** [Closure(cxt, param, locals, body)] is a function closure, where
          [locals] are the additional variable names that will be used in the
          body. *)
| Win_value: win_ref -> value

| Node_value: node_ref -> value

| Null_value: value

| Bool_value: bool -> value

| Int_value: int -> value

| String_value: string -> value

| Url_value: url -> value

| Type_value: typ -> value

| Code_value: expr False -> value

| Error: string -> value
      (** An error result (with an informative message). *)



(** The implementation of some primitive unary operations. *)
let prim1 (prim:string) (r: value)
  : value =
    match (prim, r) with
    | ("!", Bool_value(bl)) -> Bool_value(not bl)
    | ("-", Int_value(n)) -> Int_value(- n)
    | ("typeof", Null_value) -> Type_value(Null_type)
    | ("typeof", Bool_value(_)) -> Type_value(Bool_type)
    | ("typeof", Int_value(_)) -> Type_value(Int_type)
    | ("typeof", String_value(_)) -> Type_value(String_type)
    | ("typeof", Url_value(_)) -> Type_value(Url_type)
    | ("typeof", Type_value(_)) -> Type_value(Type_type)
    | ("typeof", Code_value(_)) -> Type_value(Code_type)
    | ("typeof", Win_value(_)) -> Type_value(Window_type)
    | ("typeof", Node_value(_)) -> Type_value(Node_type)
    | ("typeof", Closure _ _ _ _) -> Type_value(Function_type)
    | ("StringToInt", String_value(s)) -> Int_value(int_of_string(s)) 
  
    | ("IntToString", Int_value(i)) -> String_value(string_of_int(i))    
    | (_, Error(_) ) -> r
    | (_, _) ->
        Error (
          "primitive operation (" ^ prim ^ ") is not implemented on this type")

  (** The implementation of some primitive binary operations. *)
let prim2 (prim: string) (r1: value) (r2: value)
  : value =
    begin match (prim, r1, r2) with
    | ("==", _, _) -> Bool_value(r1 = r2)
    | ("||", Bool_value(bl1), Bool_value(bl2)) -> Bool_value(bl1 || bl2)
    | ("&&", Bool_value(bl1), Bool_value(bl2)) -> Bool_value(bl1 && bl2)
    | ("+", String_value(s1), String_value(s2)) -> String_value(s1 ^ s2)
    | ("*", Int_value(n1), Int_value(n2)) -> Int_value( (let open FStar.Mul in n1 * n2))
    | ("+", Int_value(n1), Int_value(n2)) -> Int_value(n1 + n2)
    | ("-", Int_value(n1), Int_value(n2)) -> Int_value(n1 - n2)
    | ("addtourl", Url_value(u), String_value(s)) -> Url_value
        ( match u with 
            | Blank_url -> Blank_url
            | Http_url (domain, {req_uri_path = path; req_uri_params = params}) ->
                 Http_url (domain, {req_uri_path = path;
                                    req_uri_params = params ^ "?t=" ^ s})
         )
    | (_, Error(_), _) -> r1
    | (_, _, Error(_)) -> r2
    | (_, _, _) ->
        Error (
          "primitive operation (" ^ prim ^
            ") is not implemented for these types")
    end

  (** The obvious coercion from source expressions to internal expresions. *)
  let rec to_inner_expr (e: expr unit)
  : expr inner =
    let f = to_inner_expr in
    begin match e with
    // ---TO DO--
    | X(_) ->  Null  (* unreachable case *)            // assert (False)    --TODO--
    | Null -> Null
    | Bool(b) -> Bool(b)
    | Int(n) -> Int(n)
    | String(s) -> String(s)
    | Url(u) -> Url(u)
    | Types(t) -> Types(t)
    | Code e -> Code e
    | Eval e -> Eval (f e)
    | Var(x) -> Var(x)
    | Function x xs e -> Function x xs (f e)
    | Apply e1 e2 -> Apply (f e1) (f e2)
    | Prim1 s e1 -> Prim1 s (f e1) 
    | Prim2 s e1 e2 -> Prim2 s (f e1) (f e2)
    | Alert e1 -> Alert (f e1)
    | If e1 e2 e3 -> If  (f e1) (f e2) (f e3)
    | While e1 e2 -> While (f e1) (f e2)
    | Set_var var e1 -> Set_var var (f e1)
    | Seq e1 e2 -> Seq (f e1) (f e2)
    | Get_cookie e1 e2 -> Get_cookie (f e1) (f e2)
    | Set_cookie e1 e2 e3 -> Set_cookie (f e1) (f e2) (f e3)
    | Xhr e1 e2 e3 -> Xhr (f e1) (f e2) (f e3)
    | Self_win -> Self_win
    | Named_win e1 -> Named_win (f e1)
    | Open_win e1 -> Open_win (f e1)
    | Open_named_win e1 e2 -> Open_named_win (f e1) (f e2)
    | Close_win e1 -> Close_win (f e1)
    | Navigate_win e1 e2 -> Navigate_win (f e1) (f e2)
    | Is_win_closed e1 -> Is_win_closed (f e1)
    | Get_win_opener e1 -> Get_win_opener (f e1)
    | Get_win_location e1 -> Get_win_location( f e1)
    | Get_win_name e1 -> Get_win_name (f e1)
    | Set_win_name e1 e2 -> Set_win_name (f e1) (f e2)
    | Get_win_root_node e1 -> Get_win_root_node (f e1)
    | Set_win_root_node e1 e2 -> Set_win_root_node (f e1) (f e2)
    | Get_win_var e1 var -> Get_win_var (f e1) var
    | Set_win_var e1 var e2 -> Set_win_var (f e1) var (f e2)
    | New_node e -> New_node (f e)
    | Get_node_type e -> Get_node_type (f e)
    | Get_node_contents e -> Get_node_contents (f e)
    | Set_node_contents e1 e2 -> Set_node_contents (f e1) (f e2)
    | Get_node_attr e1 e2 -> Get_node_attr (f e1) (f e2)
    | Set_node_attr e1 e2 e3 -> Set_node_attr (f e1) (f e2) (f e3)
    | Remove_handlers e -> Remove_handlers (f e)
    | Add_handler e1 e2 -> Add_handler (f e1) (f e2)
    | Get_parent e -> Get_parent (f e)
    | Get_child e1 e2 -> Get_child (f e1) (f e2)
    | Insert_node e1 e2 e3 -> Insert_node (f e1) (f e2) (f e3)
    | Remove_node e -> Remove_node(f e)
    end




(** A typf eor document node. *)

type node=
    | Para_node of option elt_id * string
      (** Represents a text node. *)
    | Link_node of option elt_id * url * string
      (** Represents a link node. *)
    | Textbox_node of option elt_id * string *  list value
      (** [Textbox_Node(id, text, handlers)] represents a text input box along
          with its current handlers. *)
    | Button_node of option elt_id * string * list value
      (** [Button_node(id, button_text, handlers)] represents a button along
          with its current handlers.  If a handler is a function closure, it
          will be run by applying the function to the button node with
          which it's associated.  If a handler is any other result, it will be a
          no-op. *)
    | Inl_script_node of option elt_id * expr unit * bool
      (** Represents a script node with an inline script, along with a flag
          that indicates whether or not the script has been queued for
          execution. *)
    | Rem_script_node of option elt_id * url * bool
      (** Represents a script node that references an external script, along
          with a flag that indicates whether or not the script has been
          requested. *)
    | Div_node of option elt_id * list node_ref
      (** Represents a sequence of nodes in the document. *)


(** The type of the objects in a page's queue of scripts to execute. *)
type queued_expr=
| Known_expr: expr inner -> queued_expr

| Unknown_expr: node_ref -> queued_expr


(** The type of a page, i.e., a window's contents at a particular point in
      time. *)
type page={
    page_location: url;

    page_document: option node_ref;

    page_environment: act_ref;

    page_script_queue: list queued_expr;
}

(** The type of an activation record in the scripting language. *)
type act = {
    act_parent: option act_ref;

    act_vars: list (var * value);
}

  type dst =
    | Xhr_dst of page_ref * value
      (** [Xhr_dst(pr, f)] represents a requested script that should be supplied
          as an argument to the closure [f] and run on the page referenced by
          [pr]. *)
    | Doc_dst of win_ref
      (** [Doc_dst(wr)] represents a requested document that should replace
          the contents of the window referenced by [wr]. *)
    | Script_dst of page_ref * node_ref
      (** [Script_dst(pr, dr)] represents a requested script that should be used
          to replace a marker in the [page_script_queue] of the page referenced
          by [pr]. *)


(** The type of a script expression queued for execution. *)
  type task = {
    task_win: win_ref;
    task_expr: expr inner;
  }

(** asserting that activation parent has lower reference number than
activation record **)
let b_act_ref_pred (r:act_ref) (a:act) : bool = 
    match a.act_parent with | None -> true | Some ap -> r > ap

let rec b_env_pred (l:list (act_ref * act)) : bool = 
  match l with 
  | [] -> true
  | (r, a)::tl -> b_act_ref_pred r a && (b_env_pred tl)

type b_env = l:(list (act_ref * act)){b_env_pred l}

(** asserting that if node is of type div, its list of node_ref has 
higher reference numbers **)
let rec node_list_check (l:list node_ref) (n: node_ref): bool=
  match l with 
  | [] -> true
  | r::tl -> r > n && (node_list_check tl n)

let b_node_ref_pred (r:node_ref) (n:node) : bool=
  match n with
  | Div_node (_,n) -> node_list_check n r
  | _ -> true

let rec b_node_pred (l:list (node_ref * node)) : bool=
  match l with 
  | [] -> true
  | (r,n):: t1 -> b_node_ref_pred r n && (b_node_pred t1)

type b_nodes= l: (list (node_ref * node)){b_node_pred l}

type b_conn= list (domain * req_uri * dst)
(** A typf eor the basic state of a browser. *)
type browser = {

    browser_windows: list (win_ref * win) ;
    
    browser_pages: list (page_ref * page);
   
    browser_nodes: b_nodes;
  
    browser_environments: b_env ;
   
    browser_cookies: list (cookie_id * string) ;
   
    browser_connections: b_conn;
   
}


(** {2 Internal browser implementation} *)

(** {3 Manipulating the browser's stores} *)  

(** {4 General map functions} *)
    

(** Adds or updates a binding in a mapping. *)
  let rec upd_assoc (#key:eqtype) (#data:eqtype) (k: key) (d:data) (map: list (key * data))
  : list (key * data) =
    begin match map with
    | [] -> [ (k, d) ]
    | (k', d') :: map1 -> if k' = k then ((k', d):: map1) else (k', d') :: upd_assoc k d map1
    end


(** Finds the position of a key in a mapping. *)
  let rec find_pos (#key:eqtype) (#data:eqtype) (k: key) (map: list (key * data))
  : int =
    begin match map with
    | [] -> 0
    | (k', _) :: map1 -> if k' = k then 0 else 1+ find_pos k map1
    end

(** remove a binding with key value k from mapping **)
  let rec remove_assoc (#key:eqtype) (#data:eqtype) (k: key) (map: list (key * data))
  : list (key * data) =
    begin match map with
    | [] -> []
    | (k', d') :: map1 -> if k' = k then  map1 else (k', d') :: remove_assoc k map1
    end
  

(** {4 Browser page store} *)

(** [page_valid pr b] returns [true] if [pr] is in the page store of [b]. *)
  let page_valid (pr: page_ref) (b: browser)
  : bool =
    match assoc pr b.browser_pages with 
  | None -> false
  | _ -> true

(** [page_assoc pr b] returns the [page] associated with [pr] in the page
      store of [b], if one exists. *)
  let page_assoc (pr: page_ref) (b: browser)
  : option page =
    assoc pr b.browser_pages

(** [page_assoc_valid pr b] returns the [page] associated with [pr] in the
      page store of [b]. *)
  let page_assoc_valid (pr: page_ref) (b: browser
    {(Some? (assoc pr b.browser_pages))}) : page =

  Some?.v (assoc pr b.browser_pages)

(** [page_update pr p b] associates [pr] with the new page [p] in [b]. *)
let page_update (pr: page_ref) (p: page) (b: browser)
  : browser =
    let pages' =  upd_assoc pr p b.browser_pages in
    { b with browser_pages = pages' }
  
  
  (** [page_new p b] adds [p] to the page store of [b] and returns its fresh
      key. *)
  let page_new (p: page) (b: browser)
  : page_ref * browser =
    let p_ref = 
  (if length (b.browser_pages) >=1 then (fst (last b.browser_pages))
  else 0) in
    let pr = fresh_page_ref (p_ref) in
    let pages'= upd_assoc pr p b.browser_pages in 
    let b' = { b with browser_pages = pages' } in
    (pr, b')

// (** [page_remove pr b] removes [pr] and its page from from [b]. *)
let page_remove (pr: page_ref) (b: browser)
  : browser =
    let pages' = remove_assoc pr b.browser_pages in
    { b with browser_pages = pages' }


  (** [page_win pr b] returns the [win_ref] that refers to the unique window
      that contains the page referenced by [pr]. *)  
  let page_win (pr: page_ref) (b: browser)
  : option win_ref =
    let has_pr (_, w) = (w.win_page = pr) in
    begin match filter has_pr b.browser_windows with
    | [ (wr, _) ] -> Some(wr)
    | _ -> None
    end


(** {4 Browser window store} *)

(** [win_valid wr b] returns [true] if [wr] is in the window store of [b]. *)
let win_valid (wr: win_ref) (b: browser)
: bool =
    match assoc wr b.browser_windows with 
  | None -> false
  | _ -> true


  (** [win_assoc wr b] returns the [win] associated with [wr] in the window
      store of [b], if one exists. *)
  let win_assoc (wr: win_ref) (b: browser)
  : option win =
    assoc wr b.browser_windows  

(** [win_assoc_valid wr b] returns the [win] associated with [wr] in the window
      store of [b]. *)
let win_assoc_valid (wr:win_ref) (b:browser
{(Some? (assoc wr b.browser_windows))}) : win =

  Some?.v (assoc wr b.browser_windows)

(** [win_update wr w b] associates [wr] with the new window [w] in [b]. *)
let win_update (wr: win_ref) (w: win) (b: browser)
  : browser =
    let windows' = upd_assoc wr w b.browser_windows in
    { b with browser_windows = windows' }

(** [win_new w b] adds [w] to the window store of [b] and returns its fresh
    key. *)
let win_new (w: win) (b: browser)
  : win_ref * browser =
  let w_ref = 
  (if length (b.browser_windows) >=1 then (fst (last b.browser_windows))
  else 0) in
    let wr = fresh_win_ref (w_ref) in
    let b' = win_update wr w b in
    (wr, b')

(** [win_remove wr b] removes [wr] and its window from from [b]. *)
let win_remove (wr: win_ref) (b: browser)
  : browser =
    begin match win_assoc wr b with
    | None -> b
    | Some(w) ->
        let b' = page_remove w.win_page b in
        let windows' = remove_assoc wr b'.browser_windows in
        { b' with browser_windows = windows' }
end

(** [win_from_win_name str b] returns the window reference corresponding
      to the window name [str] in [b]. *)

let win_from_win_name (str: string) (b: browser)
: option win_ref  =
  let has_name (_, w) = (w.win_name = Str_name(str)) in
    begin match filter has_name b.browser_windows with
    | [] -> None
    | (wr, _) :: _ -> Some(wr)
end

  (** [win_from_user_window uw b] returns a reference to the open window in [b]
      that corresponds to the domain and number given by [uw]. *)

    // --TO DO--
assume val win_from_user_window (u n: user_window) (b:browser) : option win_ref
// let win_from_user_window ((u, n): user_window) (b:browser)
//   : option win_ref =
//     let has_url (_, w) =
//       ((page_assoc_valid w.win_page b).page_location = u) in 
//   // page_assoc used instead of page_assoc_valid
//     let windows' = filter has_url b.browser_windows in          
//     begin if length windows' <= n then
//       None
//     else
//       Some (fst (index b.browser_windows n))
// end

  (** [win_to_user_window wr b] returns the user window description
      corresponding to the valid window reference [wr]. *)

// --- TO DO--
// assume val win_to_user_window (wr: win_ref) (b: browser) : user_window
// let win_to_user_window (wr: win_ref) (b: browser)
//   : user_window =
//     let w = win_assoc_valid wr b in
//     let u = (page_assoc_valid w.win_page b).page_location in
//     let has_url (_, w) =
//       (page_assoc_valid w.win_page b).page_location = u
//     in
//     let windows' = filter has_url b.browser_windows in
//     let rec find_pos wr ws n =
//       begin match ws with
//       | (wr', _) :: ws' -> if wr' = wr then n 
//       else find_pos wr ws' n+1
//       | _ -> admit()  // assert false   --TO DO--
//       end
//     in
//     (u, find_pos wr windows' 0)


(** {4 Browser node store} *)

  (** [node_valid dr b] returns [true] if [dr] is in the node store of
      [b]. *)
  let node_valid (dr: node_ref) (b: browser)
  : bool =
    match (assoc dr b.browser_nodes) with 
  | None -> false
  | _ -> true

(** [node_assoc_valid dr b] returns the [node] associated with [dr] in the
      node store of [b]. *)

// ---TO DO---
let rec node_assoc_valid_lemma (dr: node_ref) (b:b_nodes):
  Lemma (requires (Some? (assoc dr b)))
	(ensures (b_node_ref_pred dr (Some?.v (assoc dr b)))) =
  match b with
  | [] -> ()
  | (k,_)::tl -> if k = dr then () else node_assoc_valid_lemma dr tl

let node_assoc_valid (dr: node_ref) (b: browser {node_valid dr b}) : (n:node{b_node_ref_pred dr n}) =
  assert (Some? (assoc dr b.browser_nodes));
  node_assoc_valid_lemma dr b.browser_nodes;
  match (assoc dr b.browser_nodes) with 
  | Some v -> v

let rec node_update_lemma (dr: node_ref) (dn: node{b_node_ref_pred dr dn}) (b:b_nodes):
  Lemma (requires (b_node_ref_pred dr dn))
  (ensures (b_node_pred (upd_assoc dr dn b)))=
   begin match b with
    | [] -> ()
    | (k', d') :: b' -> if k' = dr then () else node_update_lemma dr dn b'
   end

  (** [node_update dr dn b] associates [dr] with the node [dn] in [b]. *)
let node_update (dr: node_ref) (dn: node{b_node_ref_pred dr dn}) (b: browser)
  : browser =
    let nodes' = upd_assoc dr dn b.browser_nodes in
    node_update_lemma dr dn b.browser_nodes;
    { b with browser_nodes = nodes' }

(** [node_new dn b] adds [dn] to the node store of [b] and returns its
      fresh key. *)
let node_new (dn: node) (b: browser{let n_ref = (if length (b.browser_nodes) >= 1 then (fst (last b.browser_nodes)) else 0) in
		        let nr = fresh_node_ref (n_ref) in
			b_node_ref_pred nr dn})
  : node_ref * browser =
  let n_ref = if length b.browser_nodes >= 1 then (fst (last b.browser_nodes)) else 0 in
  let nr = fresh_node_ref n_ref in
  let b' = node_update nr dn b in
  (nr, b')

(** The type of a node's parent object. *)
type node_parent_type =
    | No_parent
    | Page_parent of page_ref
    | Parent_node of node_ref

let rec node_parent_bpages (dr: node_ref) (l: list (page_ref * page)) =
  match l with 
  | [] -> None
  | (pr, p)::tl -> (match p.page_document with 
		 | Some dr1 -> if dr1 = dr then Some pr else node_parent_bpages dr tl
		 | _ -> node_parent_bpages dr tl)

let rec node_parent_dnode_lemma (dr:node_ref) (l:list node_ref) (nr:node_ref) : Lemma ((node_list_check l nr /\ mem dr l) ==> nr < dr) = 
  match l with 
  | [] -> ()
  | hd::tl -> node_parent_dnode_lemma dr tl nr

let rec node_parent_dnode (dr: node_ref) (l: b_nodes) : option (pr:node_ref{pr < dr}) =
  match l with 
  | [] -> None
  | (nr,n)::tl -> assert (b_node_ref_pred nr n);
		(match n with 
		| Div_node (_, children) -> 
		  assert (node_list_check children nr);
		  node_parent_dnode_lemma dr children nr;
		  if mem dr children then Some nr else node_parent_dnode dr tl
		| _ -> node_parent_dnode dr tl)

(** Finds the parent of a node. *)
let node_parent (dr: node_ref) (b: browser)
  : node_parent_type =
  begin match node_parent_bpages dr b.browser_pages with
    | Some pr -> Page_parent pr
    | None ->
        begin match node_parent_dnode dr b.browser_nodes with
        | Some nr -> assert (nr < dr); Parent_node nr
        | None -> No_parent
        end
  end

  (** Finds the page displaying a node if there is one. *)
let rec node_page (dr: node_ref) (b: browser)
  : option page_ref =
    begin match node_parent dr b with
    | No_parent -> None
    | Page_parent (pr) -> Some pr
    | Parent_node (dr1) -> node_page dr1 b
    end

(** {4 Browser activation record store and variables} *)

  (** [act_valid ar b] returns [true] if [ar] is in the variable store of
      [b]. *)
  let act_valid (ar: act_ref) (b: browser)
  : bool =
    match (assoc ar b.browser_environments) with 
  | None -> false
  | _ -> true
  
let rec act_assoc_valid_lemma (ar: act_ref) (b:b_env) : 
  Lemma (requires (Some? (assoc ar b))) 
	(ensures (b_act_ref_pred ar (Some?.v (assoc ar b)))) = 
  match b with 
  | [] -> ()
  | (k,_)::tl -> if k = ar then () else act_assoc_valid_lemma ar tl

(** [act_assoc_valid ar b] returns the activation record associated with [ar]
      in [b]. *)
let act_assoc_valid (ar: act_ref) (b: browser{(act_valid ar b)}) : (a:act{b_act_ref_pred ar a}) =
  act_assoc_valid_lemma ar b.browser_environments;
  match (assoc ar b.browser_environments) with 
  | Some v -> v

let rec act_update_lemma (ar: act_ref) (act: act{b_act_ref_pred ar act}) (b:b_env) : 
  Lemma (requires (b_act_ref_pred ar act)) 
	(ensures (b_env_pred (upd_assoc ar act b))) = 
    begin match b with
    | [] -> ()
    | (k', d') :: b' -> if k' = ar then () else act_update_lemma ar act b'
    end

(** [act_update ar l act b] associates [ar] with the new domain [l]
      and the new activation record [act] in [b]. *)
  let act_update (ar: act_ref) (act: act{b_act_ref_pred ar act}) (b: browser)
  : browser =
    let environments' = upd_assoc ar act b.browser_environments in
    act_update_lemma ar act b.browser_environments;
    { b with browser_environments = environments' }

(** [act_new l act b] adds [act], paired with [l], to the activation
      record store of [b] and returns its fresh key. *)
  let act_new (act: act) (b: browser{let a_ref = (if length (b.browser_environments) >= 1 then (fst (last b.browser_environments)) else 0) in
		        let ar = fresh_page_ref (a_ref) in
			b_act_ref_pred ar act})
  : act_ref * browser =
    let a_ref = (if length (b.browser_environments) >= 1 then (fst (last b.browser_environments)) else 0) in
    let ar = fresh_page_ref (a_ref) in
    let b' = act_update ar act b in
    (ar, b')

(** [get_var x ar b] gets the result associated with [x] in [b] in the scope
      defined by [ar], if [x] exists in that scope. *)

  let rec get_var (x: var) (ar: act_ref) (b: browser)
  : option value =
  if act_valid ar b then
    let act = act_assoc_valid ar b in
    assert (b_act_ref_pred ar act);
    begin match assoc x act.act_vars with 
    | None -> begin match act.act_parent with
      | None -> None
      | Some ar1 -> get_var x ar1 b
      end
    | _ ->  (assoc x act.act_vars)
    end
  else None

(** [create_var x r ar b] creates (or overwrites) the binding of [x]
      with the rslt [r] in the activation record referenced by [ar] in
      [b]. *)
      // --- TO DO --
  let create_var (x: var) (r: value) (ar: act_ref) (b: browser)
  : option browser =
   if (act_valid ar b) then
    let act = act_assoc_valid ar b in
    let data' = upd_assoc x r act.act_vars in
    let act' = { act with act_vars = data' } in
    Some (act_update ar act' b)
  else None /// --TO DO --

  (** [set_var x r ar b] updates the nearest enclosing binding of [x] (with
      the rslt [r]) under the scope defined by [ar] in [b].  If no current
      binding is found, a new binding is created in the outermost scope. *)

let rec set_var (x: var) (r: value) (ar: act_ref) (b: browser)
  : option browser =
   if (act_valid ar b) then
    let act = act_assoc_valid ar b in
    begin match assoc x act.act_vars with
    | None -> begin match act.act_parent with
      | None -> create_var x r ar b
      | Some(ar1) -> set_var x r ar1 b
    end
    | _ -> let data' = upd_assoc x r act.act_vars in
      let act' = { act with act_vars = data' } in
     Some(act_update ar act' b )
     end
  else None

// (** {4 Browser cookie store} *)

  (**/**)
  (** [prefix xs ys] returns [true] if [xs] is a prefix of [ys] and [false]
      otherwise. *)
  let rec prefix (#a:eqtype) (xs: list a) (ys: list a)
  : bool =
    begin match (xs, ys) with
    | ([], _) -> true
    | (x :: xs', y :: ys') -> x = y && prefix xs' ys'
    | (_, _) -> false
    end

  (** [get_site_cookies d p b] builds the mapping of cookie keys and values
      that is specific for the domain [d] and the path [p] in [b]. *)
  let get_site_cookies (d: domain) (p: path) (b: browser)
  : list (string * string)=
    let check (cid, _) =
      (cid.cookie_id_domain = d &&
        prefix cid.cookie_id_path p) ||
        (prefix cid.cookie_id_domain d &&
          cid.cookie_id_path = [])
    in
    let strip (cid, rslt) = (cid.cookie_id_key, rslt) in
    map strip (filter check b.browser_cookies)


(** [del_site_cookie d p k b] removes the mapping for of cookie key [k] for
      the domain [d] and path [p] in [b]. *)
  let del_site_cookie (d: domain) (p: path) (k: string) (b: browser)
  : browser =
    let cid = {
      cookie_id_domain = d;
      cookie_id_path = p;
      cookie_id_key = k;
    } in
    { b with browser_cookies = remove_assoc cid b.browser_cookies }

  (** [del_site_cookies d p ks b] removes the mapping of all of the cookies with
      keys in [ks] from the cookie store for the domain [d] and path [p] in
      [b]. *)
  let rec del_site_cookies (d: domain) (p: path) (ks: list string) (b: browser)
  : browser =
    begin match ks with
    | [] -> b
    | k :: ks1 ->
        let b' = del_site_cookie d p k b in
        del_site_cookies d p ks1 b'
    end

  (** [set_site_cookie d p (k, v) b] adds the mapping of the cookie key [k] to
      the rslt [v] for the domain [d] and path [p] in [b]. *)
  let set_site_cookie (d: domain) (p: path) ((k, v): string * string) (b: browser)
  : browser =
    let cid = {
      cookie_id_domain = d;
      cookie_id_path = p;
      cookie_id_key = k;
    } in
    { b with browser_cookies = upd_assoc cid v b.browser_cookies }

  (** [set_site_cookies d p pairs b] adds all of the key-rslt mappings in
      [pairs] to the cookie store for the domain [d] and path [p] in [b]. *)
  let rec set_site_cookies
    (d: domain) (p: path) (pairs: list (string * string)) (b: browser)
  : browser =
    begin match pairs with
    | [] -> b
    | pair :: pairs' ->
        let b' = set_site_cookie d p pair b in
        set_site_cookies d p pairs' b'
    end

  (** [upd_cookies d uri resp b] executes all of the cookie updates requested
      by [resp], which was received in response to a request to [d] for
      resource [req_uri]. *)
  let upd_cookies (d: domain) (uri: req_uri) (resp: resp) (b: browser)
  : browser =
    let b' = del_site_cookies d uri.req_uri_path resp.resp_del_cookies b in
    let b'' = set_site_cookies d uri.req_uri_path resp.resp_set_cookies b' in
    b''

//   (** {4 Browser network connections} *)

  (** [net_connection_domain_nth d n b] returns the [n]th [req_uri] and [dst]
      associated with [d] (based on the order in which they were opened) among
      the open network connections of [b], if one exists. *)
  let net_connection_domain_nth (d: domain) (n: nat) (b: browser)
  : option (req_uri * dst) =
    let test (d', _, _) = (d' = d) in
    let connections' = filter test b.browser_connections in
    begin if length connections' <= n then
      None
    else
      let (_, uri, dst) = index connections' n in
      Some((uri, dst))
    end

  // helper function
  let rec remove (d: domain)(n: nat) (cs:b_conn): Tot (b_conn)(decreases cs) =
      begin match cs with
      | [] -> []
      | (d',x,y) :: cs'-> if n = 0 && d' = d then cs' 
                            else if d' = d then (d',x,y):: remove d (n-1) cs'
                            else (d',x,y) :: remove d n cs'
      end

  (** [net_connection_domain_remove_nth d n b] removes the [n]th [req_uri] and
      [dst] associated with [d] (based on the order in which they were opened)
      among the open network connections of [b], if one exists. *)
  
  let net_connection_domain_remove_nth (d: domain) (n: nat) (b: browser)
  : browser =

    let connections' = remove d n b.browser_connections in
    { b with browser_connections = connections' }

  
 (** [http_send d uri body dst b] updates the appropriate browser structures
      and creates the necessary output events for sending a new HTTP request
      whose response will be received by [dst]. *)
  let http_send (d: domain) (uri: req_uri) (body: string) (dst: dst) (b: browser)
  : browser * output_event =
    let connections' = (d, uri, dst) :: b.browser_connections in
    let b' = {
      b with browser_connections = connections';} in
    let req = {
      req_uri = uri;
      req_cookies = get_site_cookies d uri.req_uri_path b;
      req_body = body;
    } in
    (b', Network_send_event(d, req))

//  (** {3 Rendering documents for user output} *)

//   // (**/**)
//   //  --- TO DO ---

//   assume val render_doc_as_list (dr: node_ref) (b: browser): list rendered_doc
//   let rec render_doc_as_list (dr: node_ref) (b: browser)
//   : list rendered_doc =
//     assert (node_valid dr b);
//     begin match node_assoc_valid dr b with
//     | Para_node ( _, txt) -> [ Para_rendered(txt) ]
//     | Link_node (_, u, txt) -> [ Link_rendered(u, txt) ]
//     | Textbox_node (_, txt, _) -> [ Textbox_rendered(txt) ]
//     | Button_node(_, txt, _) -> [ Button_rendered(txt) ]
//     | Inl_script_node(_, _, _) -> []
//     | Rem_script_node(_, _, _) -> []
//     | Div_node(_, drs) ->
//         let bd dr = render_doc_as_list dr b in
//         [ Div_rendered(flatten (map bd drs)) ]
//     end
//   (**/**)


//   // (** [render_page pr b] returns the rendered representation of the document in
//   //     [pr].  A result of [None] indicates there are no visible contents.  The
//   //     behavior of this function is undefined if [pr] is not a valid page
//   //     reference. *)
// assume val render_page (pr: page_ref) (b: browser)
//   : option rendered_doc 
//   // --- TO DO --
//   // let render_page (pr: page_ref) (b: b)
//   // : rendered_doc option =          
//   //   assert (page_valid pr b);    
//   //   begin match (page_assoc_valid pr b).page_document with
//   //   | None -> None
//   //   | Some(dr) ->
//   //       begin match render_doc_as_list dr b with
//   //       | [] -> None
//   //       | rd :: _ -> Some(rd)
//   //       end
//   //   end


//     (** [page_update_event pr b] returns a [UI_page_updated_event] for the page
//       reference [pr]. *)
// // --- TO DO ---
// assume val page_update_event (pr: page_ref) (b: browser): output_event
//   // let page_update_event (pr: page_ref) (b: browser)
//   // : output_event =
//   //   let wr =
//   //     begin match page_win pr b with
//   //     | None -> assert false
//   //     | Some(wr) -> wr
//   //     end
//   //   in
//   //   let uw = win_to_user_window wr b in
//   //   let rd_opt = render_page pr b in
//   //   UI_page_updated_event(uw, rd_opt)


//   (** [build_win wn u doc b] builds a new window structure with the window name
//       [wn].  A new page is created in [b] using [u] for the page's
//       location.  If the contents can be displayed immediately, then a new
//       activation record is also created for the window's environment. *)
//   let build_win
//     (wn: win_name) (u: url) (wo: win_opener) (doc: option node_ref) (b: browser)
//   : win * browser =
//     let a = {
//       act_parent = None;
//       act_vars = [];
//     } in
//     let (ar, b') = act_new a b in
//     let p = {
//       page_location = u;
//       page_document = doc;
//       page_environment = ar;
//       page_script_queue = [];
//     } in
//     let (pr, b'') = page_new p b' in
//     let w = {
//       win_name = wn;
//       win_opener = wo;
//       win_page = pr;
//     } in
//     (w, b'')

//   (** [fetch_url u wr b] performs whatever action is required by the URL [u]. *)
//   //  --- TO DO --
//   let fetch_url (u: url) (wr: win_ref) (b: browser)
//   : browser * list output_event =
//    assume (win_valid wr b);
//     assert (win_valid wr b);
//     begin match u with
//     | Blank_url ->
//         (b, [])
//     | Http_url(d, uri) ->
//         let dst = Doc_dst(wr) in
//         let (b', oe) = http_send d uri "" dst b in
//         (b', [ oe ])
//     end

//   (** [open_win wn u b] creates a new window in the browser with the window
//       name [wn], directed to the URL [u].  It returns a reference to the newly
//       created window. *)

//   let open_win (wn: win_name) (u: url) (wo: win_opener) (b: browser)
//   : win_ref * browser * list output_event =
//     let (w', b') = build_win wn Blank_url wo None b in
//     let (wr, b'') = win_new w' b' in
//     let (b''', oes) = fetch_url u wr b'' in
//     (wr, b''', UI_win_opened_event :: oes)


// (** [close_doc_request_connections wr b] removes any network connection
//       that is waiting for a document to load into the window referenced by
//       [wr]. *)
//   let close_doc_request_connections (wr: win_ref) (b: browser)
//   : browser =
// // -- to do --
//     assume (win_valid wr b);
//     assert (win_valid wr b);
//     let not_for_wr (_, _, dst) =
//       begin match dst with
//       | Doc_dst(wr') -> wr' <> wr
//       | _ -> true
//       end
//     in
//     let connections' = filter not_for_wr b.browser_connections in
//     { b with browser_connections = connections' }

//  (** [direct_win wr u b] directs the window referenced by [wr] to the URL
//       [u]. *)
//   let direct_win (wr: win_ref) (u: url) (b: browser)
//   : browser * list output_event =
// // -- to do --
//     assume (win_valid wr b);
//     assert (win_valid wr b);
//     let b' = close_doc_request_connections wr b in
//     begin match u with
//     | Blank_url ->
//         let uw = win_to_user_window wr b' in
//         let w = win_assoc_valid wr b' in
//         let b'1 = page_remove w.win_page b' in
//         let (w', b'2) = build_win w.win_name Blank_url w.win_opener None b'1 in
//         let b'3 = win_update wr w' b'2 in
//         let oe = UI_page_loaded_event(uw, Blank_url, None) in
//         (b'3, [ oe ])
//     | _ ->
//         fetch_url u wr b
//     end


//   (** {3 Node tree operations} *)

//   (** [build_node_tree doc] allocates all of the node references needed to
//       represent [doc] and returns the association list of these nodes with their
//       appropriate node data.  It also returns the reference to the root of the
//       node tree that was created. *)

//   // let rec build_node_tree (doc: doc)
//   // : node_ref * list (node_ref * node) =

//   // // ---TO DO Recursion error--

//   //   let dr = fresh_node_ref 0 in  // what to do give input? in fresh_node_ref()
//   //   match doc with
//   //   | Para(id, text) ->
//   //       (dr, [ (dr, Para_node(id, text)) ])
//   //   | Link(id, u, text) ->
//   //       (dr, [ (dr, Link_node(id, u, text)) ])
//   //   | Textbox(id, text) ->
//   //       (dr, [ (dr, Textbox_node(id, text, [])) ])
//   //   | Button(id, text) ->
//   //       (dr, [ (dr, Button_node(id, text, [])) ])
//   //   | Inl_script(id, e) ->
//   //       (dr, [ (dr, Inl_script_node(id, e, false)) ])
//   //   | Rem_script(id, u) ->
//   //       (dr, [ (dr, Rem_script_node(id, u, false)) ])
//   //   | Divi(id, subdocs) ->
//   //       let (drs, nhs) = split (map build_node_tree subdocs) in
//   //       (dr, (dr, Div_node(id, drs)) :: flatten nhs)


//   (** [split_queued_exprs qes] returns the prefix of [qes] that are known
//       scripts, as an [inner expr list], along with the remainder of [qes]. *)
  
//   // --- TO DO -- TYPE ERROR
  
//   // let split_queued_exprs (qes: list queued_expr)
//   // : (list expr inner) * (list queued_expr)  =
//   //   let rec take_ready qes =
//   //     begin match qes with
//   //     | Known_expr(e) :: ps1 -> e :: take_ready ps1
//   //     | _ -> []
//   //     end
//   //   in
//   //   let rec drop_ready qes =
//   //     begin match qes with
//   //     | Known_expr(e) :: ps1 -> drop_ready ps1
//   //     | _ -> qes
//   //     end
//   //   in
//   //   (take_ready qes, drop_ready qes)


//   // (**/**)
//   // let rec process_node_scripts_aux (pr: page_ref) (dr: node_ref) (b: browser)
//   // : browser * list queued_expr * list output_event =

//   // // -- TO DO --
//   //   assume (page_valid pr b);
//   //   assume (node_valid dr b);
//   //   assert (page_valid pr b);
//   //   assert (node_valid dr b);
//   //   begin match node_assoc_valid dr b with
//   //   | Para_node(_, _)
//   //   | Link_node(_, _, _)
//   //   | Textbox_node(_, _, _)
//   //   | Button_node(_, _, _)
//   //   | Inl_script_node(_, _, true)
//   //   | Rem_script_node(_, _, true)
//   //   | Rem_script_node(_, Blank_url, _) ->
//   //       (b, [], [])
//   //   | Inl_script_node(id, e, false) ->
//   //       let b' = node_update dr (Inl_script_node(id, e, true)) b in
//   //       (b', [ Known_expr(to_inner_expr e) ], [])
//   //   | Rem_script_node(id, Http_url(d, uri), false) ->
//   //       let b' = node_update dr (Rem_script_node(id, Http_url(d, uri), true)) b in
//   //       let (b'', oe) = http_send d uri "" (Script_dst(pr, dr)) b' in
//   //       (b'', [ Unknown_expr(dr) ], [ oe ])
//   //   | Div_node(_, drs) ->
//   //       process_node_scripts_list pr drs b
//   //   end

//   // and process_node_scripts_list (pr: page_ref) (drs: list node_ref) (b: browser)
//   // : browser * list queued_expr * list output_event =
//   //   begin match drs with
//   //   | [] -> (b, [], [])
//   //   | dr' :: drs' ->
//   //       let (b', pes1, oes1) = process_node_scripts_aux pr dr' b in
//   //       let (b'', pes2, oes2) = process_node_scripts_list pr drs' b' in
//   //       (b'', pes1 @ pes2, oes1 @ oes2)
//   //   end
//   // (**/**)

// //   (** [process_node_scripts pr dr b] prepares for execution all of the scripts
// //       that are found among the descendents of [dr] and have not yet been
// //       executed.  (Preparing them for execution means putting them in the proper
// //       queue.)  It also generates network requests for all of the remote
// //       scripts that have not been requested. *)
// //   let rec process_node_scripts (pr: page_ref) (dr: node_ref) (b: browser)
// //   : browser * list output_event * list task =
// //     assert (page_valid pr b);
// //     assert (node_valid dr b);
// //     let (b', qes, oes) = process_node_scripts_aux pr dr b in
// //     begin match page_win pr b with
// //     | None ->
// //         (b', oes, [])
// //     | Some(wr) ->
// //         let p = page_assoc_valid pr b' in
// //         let (exprs, qes') = split_queued_exprs (p.page_script_queue @ qes) in
// //         let p' = { p with page_script_queue = qes' } in
// //         let b'' = page_update pr p' b' in
// //         let task e = {
// //           task_win = wr;
// //           task_expr = e;
// //         } in
// //         (b'', oes, List.map task exprs)
// //     end

//   (**/**)
//   let rec textbox_handlers_in_tree dr b =
//     begin match node_assoc_valid dr b with
//     | Para_node(_, _)
//     | Link_node(_, _, _)
//     | Button_node(_, _, _)
//     | Inl_script_node(_, _, _)
//     | Rem_script_node(_, _, _) ->
//         []
//     | Textbox_node(_, _, handlers) ->
//         [ (dr, handlers) ]
//     | Div_node(_, drs) ->
//         flatten (map (fun dr -> textbox_handlers_in_tree dr b) drs)
//     end


//   let rec button_handlers_in_tree dr b =
//     begin match node_assoc_valid dr b with
//     | Para_node(_, _)
//     | Link_node(_, _, _)
//     | Textbox_node(_, _, _)
//     | Inl_script_node(_, _, _)
//     | Rem_script_node(_, _, _) ->
//         []
//     | Button_node(_, _, handlers) ->
//         [ (dr, handlers) ]
//     | Div_node(_, drs) ->
//         flatten (map (fun dr -> button_handlers_in_tree dr b) drs)
//     end

//   (** [textbox_handlers_in_pos wr pos b] returns the textbox handlers associated
//       with the textbox in position [pos] in the window referenced by [wr] in
//       [b], along with the corresponding [node_ref]. *)
// //   let textbox_handlers_in_pos (wr: win_ref) (posi: int) (b: browser)
// //   : option (node_ref * list value) =
// //     assert (win_valid wr b);
// //     begin match
// //       (page_assoc_valid (win_assoc_valid wr b).win_page b).page_document
// //     with
// //     | None -> None
// //     | Some(root) ->
// //         begin try
// //           Some(index (textbox_handlers_in_tree root b) posi)
// //         with
// //         | Failure("nth") -> None
// //         end
// //     end

//   (** [button_handlers_in_pos wr pos b] returns the button handlers associated
//       with the button in position [pos] in the window referenced by [wr] in
//       [b], along with the corresponding [node_ref]. *)
// //   let button_handlers_in_pos (wr: win_ref) (posi: int) (b: browser)
// //   : option (node_ref * list value) =
// //     assert (win_valid wr b);
// //     begin match
// //       (page_assoc_valid (win_assoc_valid wr b).win_page b).page_document
// //     with
// //     | None -> None
// //     | Some(root) ->
// //         begin try
// //           Some(index (button_handlers_in_tree root b) posi)
// //         with
// //         | Failure("nth") -> None
// //         end
// //     end

//   (** {3 Inserting and removing document nodes} *)

//   (** Removes a node from its location in its node tree or page. *)
//   let node_remove (dr: node_ref) (b: browser)
//   : b * output_event list =
//     begin match node_parent dr b with
//     | No_parent ->
//         (b, [])
//     | Page_parent(pr') ->
//         let p = page_assoc_valid pr' b in
//         let p' = { p with page_document = None } in
//         let b' = page_update pr' p' b in
//         (b', [ page_update_event pr' b' ])
//     | Parent_node(p) ->
//         begin match node_assoc_valid p b with
//         | Div_node(id, children) ->
//             let children' = List.filter (fun dr' -> dr' <> dr) children in
//             let b' = node_update p (Div_node(id, children')) b in
//             begin match node_page dr b with
//             | None -> (b', [])
//             | Some(pr') -> (b', [ page_update_event pr' b' ])
//             end
//         | _ -> assert false
//         end
//     end

//   (**/**)
//   (** Computes the set of all descendents of a node. *)
//   let rec node_descendents (strict: bool) (dr: node_ref) (b: browser)
//   : node_ref list =
//     assert (node_valid dr b);
//     begin match node_assoc_valid dr b with
//     | Div_node(_, drs) ->
//         let dd dr = node_descendents false dr b in
//         begin if strict then
//           List.flatten (List.map dd drs)
//         else
//           dr :: List.flatten (List.map dd drs)
//         end
//     | _ ->
//         if strict then [] else [ dr ]
//     end

//   (** [insert_in_list x xs k] inserts [x] as the [k]th element of [xs]. *)
//   let rec insert_in_list (x: 'a) (xs: 'a list) (k: int)
//   : 'a list =
//     begin match (xs, k) with
//     | (xs, 0) -> x :: xs
//     | (x' :: xs', k) when k > 0 -> x' :: insert_in_list x xs' (k-1)
//     | (_, _) -> failwith "insert_in_list"
//     end
//   (**/**)

//   (** [insert_node parent child pos b] inserts [child] into the children
//       of [parent] at position [pos]. *)
//   let node_insert (parent: node_ref) (child: node_ref) (pos: int) (b: browser)
//   : b * output_event list * task list =
//     assert (node_valid parent b);
//     assert (node_valid child b);
//     if List.mem parent (node_descendents true parent b) then
//       failwith "node_insert";
//     let (b', oes1) = node_remove child b in
//     begin match node_assoc_valid parent b' with
//     | Div_node(id, children) ->
//         let children' = insert_in_list child children pos in
//         let dn' = Div_node(id, children') in
//         let b'' = node_update parent dn' b' in
//         begin match node_page parent b'' with
//         | None -> (b'', oes1, [])
//         | Some(pr) ->
//             let (b''', oes2, ts) = process_node_scripts pr child b'' in
//             (b''', oes1 @ [ page_update_event pr b''' ] @ oes2, ts)
//         end
//     | _ -> failwith "node_insert"
//     end 

//   (** {3 Executing scripts} *)

//   (**/**)
//   (** Converts a [value] to a [elt_id], if possible. *)
//   let rslt_to_elt_it_opt (r: value)
//   : option elt_id =
//     begin match r with
//     | String_value(id) -> Some({ elt_id_value = id })
//     | _ -> None
//     end
//   (**/**)

//   (** Carries out a single step of executing a script expression. *)
//   let rec step_expr (ctx: context) (b: browser) (e: expr inner)
//   : browser * expr inner * list output_event * list task =
//     let step = step_expr ctx in
//     begin match e with

//     (* error propagation rules: an expression with an error in a subexression
//      * must step to an error *)

//     | Eval(X(R(Error(s))))
//     | Apply(X(R(Error(s))), _)
//     | Prim1(_, X(R(Error(s))))
//     | Prim2(_, X(R(Error(s))), _)
//     | Alert(X(R(Error(s))))
//     | If(X(R(Error(s))), _, _)
//     | Set_var(_, X(R(Error(s))))
//     | Seq(X(R(Error(s))), _)
//     | Get_cookie(X(R(Error(s))), _)
//     | Set_cookie(X(R(Error(s))), _, _)
//     | Xhr(X(R(Error(s))), _, _)
//     | Named_win(X(R(Error(s))))
//     | Open_win(X(R(Error(s))))
//     | Open_named_win(X(R(Error(s))), _)
//     | Close_win(X(R(Error(s))))
//     | Navigate_win(X(R(Error(s))), _)
//     | Is_win_closed(X(R(Error(s))))
//     | Get_win_opener(X(R(Error(s))))
//     | Get_win_location(X(R(Error(s))))
//     | Get_win_name(X(R(Error(s))))
//     | Set_win_name(X(R(Error(s))), _)
//     | Get_win_root_node(X(R(Error(s))))
//     | Set_win_root_node(X(R(Error(s))), _)
//     | Get_win_var(X(R(Error(s))), _)
//     | Set_win_var(X(R(Error(s))), _, _)
//     | New_node(X(R(Error(s))))
//     | Get_node_type(X(R(Error(s))))
//     | Get_node_contents(X(R(Error(s))))
//     | Set_node_contents(X(R(Error(s))), _)
//     | Get_node_attr(X(R(Error(s))), _)
//     | Set_node_attr(X(R(Error(s))), _, _)
//     | Remove_handlers(X(R(Error(s))))
//     | Add_handler(X(R(Error(s))), _)
//     | Get_parent(X(R(Error(s))))
//     | Get_child(X(R(Error(s))), _)
//     | Insert_node(X(R(Error(s))), _, _)
//     | Remove_node(X(R(Error(s)))) ->

//       (b, X(R(Error(s))), [], [])

//     | Apply(X(R(_)), X(R(Error(s))))
//     | Prim2(_, X(R(_)), X(R(Error(s))))
//     | Get_cookie(X(R(_)), X(R(Error(s))))
//     | Set_cookie(X(R(_)), X(R(Error(s))), _)
//     | Open_named_win(X(R(_)), X(R(Error(s))))
//     | Navigate_win(X(R(_)), X(R(Error(s))))
//     | Set_node_contents(X(R(_)), X(R(Error(s))))
//     | Get_node_attr(X(R(_)), X(R(Error(s))))
//     | Set_node_attr(X(R(_)), X(R(Error(s))), _)
//     | Set_win_name(X(R(_)), X(R(Error(s))))
//     | Set_win_root_node(X(R(_)), X(R(Error(s))))
//     | Set_win_var(X(R(_)), _, X(R(Error(s))))
//     | Add_handler(X(R(_)), X(R(Error(s))))
//     | Get_child(X(R(_)), X(R(Error(s))))
//     | Insert_node(X(R(_)), X(R(Error(s))), _)
//     | Xhr(X(R(_)), X(R(Error(s))), _) ->

//         (b, X(R(Error(s))), [], [])

//     | Set_cookie(X(R(_)), X(R(_)), X(R(Error(s))))
//     | Xhr(X(R(_)), X(R(_)), X(R(Error(s))))
//     | Set_node_attr(X(R(_)), X(R(_)), X(R(Error(s))))
//     | Insert_node(X(R(_)), X(R(_)), X(R(Error(s)))) ->

//         (b, X(R(Error(s))), [], [])

//     (* computational rules *)

//     | X(R(_)) ->
//         (* this should only be reached in the case of a runtime type error *)
//         let err = "run-time type error" in
//         (b, X(R(Error(err))), [], [])

//     | X(Scoped_expr(_, X(R(r1)))) ->
//         (b, X(R(r1)), [], [])
//     | X(Scoped_expr(ctx', e1)) ->
//         let (b', e1', oes, ts) = step_expr ctx' b e1 in
//         (b', X(Scoped_expr(ctx', e1')), oes, ts)

//     | Null ->
//         (b, X(R(Null_value)), [], [])
//     | Bool(bl) ->
//         (b, X(R(Bool_value(bl))), [], [])
//     | Int(n) ->
//         (b, X(R(Int_value(n))), [], [])
//     | String(s) ->
//         (b, X(R(String_value(s))), [], [])
//     | Url(u) ->
//         (b, X(R(Url_value(u))), [], [])
//     | Types(t) ->
//         (b, X(R(Type_value(t))), [], [])

//     | Code(e) ->
//         (b, X(R(Code_value(e))), [], [])
//     | Eval(X(R(Code_value(e1)))) ->
//         (b, to_inner_expr e1, [], [])

//     | Var(x) ->
//         begin match get_var x ctx.context_act b with
//         | None ->
//             let err = Printf.sprintf "variable %S not found" x.var_name in
//             (b, X(R(Error(err))), [], [])
//         | Some(r) ->
//             (b, X(R(r)), [], [])
//         end

//     | Function(x, locals, e1) ->
//         (b, X(R(Closure(ctx, x, locals, e1))), [], [])
//     | Apply(X(R(Closure(ctx1, x, locals, e1))), X(R(r2))) ->
//         let bot_null x = (x, Null_value) in
//         let act = {
//           act_parent = Some(ctx1.context_act);
//           act_vars = (x, r2) :: List.map bot_null locals;
//         } in
//         let (ar', b') = act_new act b in
//         let ctx2 = { ctx1 with context_act = ar' } in
//         (b', X(Scoped_expr(ctx2, e1)), [], [])

//     | Prim1(prim, X(R(r))) ->
//         (b, X(R(prim1 prim r)), [], [])
//     | Prim2(prim, X(R(r1)), X(R(r2))) ->
//         (b, X(R(prim2 prim r1 r2)), [], [])
//     | Alert(X(R(Null_value))) ->
//         (b, X(R(Null_value)), [ UI_alert("null") ], [])
//     | Alert(X(R(Bool_value(bl)))) ->
//         (b, X(R(Null_value)), [ UI_alert(Printf.sprintf "%B" bl) ], [])
//     | Alert(X(R(Int_value(n)))) ->
//         (b, X(R(Null_value)), [ UI_alert(Printf.sprintf "%N" n) ], [])
//     | Alert(X(R(String_value(s)))) ->
//         (b, X(R(Null_value)), [ UI_alert(s) ], [])
//     | Alert(X(R(Url_value(_)))) ->
//         (b, X(R(Null_value)), [ UI_alert("<URL>") ], [])
//     | Alert(X(R(Code_value(_)))) ->
//         (b, X(R(Null_value)), [ UI_alert("<code>") ], [])
//     | Alert(X(R(Win_value(_)))) ->
//         (b, X(R(Null_value)), [ UI_alert("<window>") ], [])
//     | Alert(X(R(Node_value(_)))) ->
//         (b, X(R(Null_value)), [ UI_alert("<node>") ], [])
//     | Alert(X(R(Closure(_, _, _, _)))) ->
//         (b, X(R(Null_value)), [ UI_alert("<function>") ], [])

//     | Set_var(x, X(R(r1))) ->
//         (set_var x r1 ctx.context_act b, X(R(r1)), [], [])
//     | If(X(R(Bool_value(true))), e2, e3) ->
//         (b, e2, [], [])
//     | If(X(R(Bool_value(false))), e2, e3) ->
//         (b, e3, [], [])
//     | While(e1, e2) ->
//         (b, If(e1, Seq(e2, While(e1, e2)), Null), [], [])
//     | Seq(X(R(_)), e2) ->
//         (b, e2, [], [])

//     // | Get_cookie(X(R(Url_value(Http_url(d, uri)))), X(R(String_value(ck)))) ->
//     //     let cs = get_site_cookies d uri.req_uri_path b in
//     //     begin try
//     //       (b, X(R(String_value(List.assoc ck cs))), [], [])
//     //     with
//     //     | Not_found -> (b, X(R(Null_value)), [], [])
//     //     end
//     | Set_cookie(
//           X(R(Url_value(Http_url(d, uri)))),
//           X(R(String_value(ck))),
//           X(R(Null_value))) ->
//         let b' = del_site_cookie d uri.req_uri_path ck b in
//         (b', X(R(Null_value)), [], [])
//     | Set_cookie(
//           X(R(Url_value(Http_url(d, uri)))),
//           X(R(String_value(ck))),
//           X(R(String_value(cv)))) ->
//         let b' = set_site_cookie d uri.req_uri_path (ck, cv) b in
//         (b', X(R(Null_value)), [], [])

//     | Xhr(X(R(Url_value(Blank_url))), X(R(_)), X(R(_))) ->
//         (b, X(R(Null_value)), [], [])
//         (* XXX: perhaps this should be an error, rather than a no-op *)

//     | Xhr(X(R(Url_value(Http_url(d, uri)))),
//           X(R(String_value(msg))), X(R(Closure(_, _, _, _)))) ->
//         begin if not (win_valid ctx.context_win b) then
//           let err = "window was closed---cannot make AJAX request" in
//           (b, X(R(Error(err))), [], [])
//         else
//           let w = win_assoc_valid ctx.context_win b in
//           let dst = Xhr_dst(w.win_page, Closure(_, _, _, _)) in
//           let (b', oe) = http_send d uri msg dst b in
//           (b', X(R(Null_value)), [ oe ], [])
//         end

//     | Self_win ->
//         (* In some browsers, it may be an error to evaluate "self" if the
//            original window is no longer open.  Here we let "self" evaluate to an
//            invalid window reference, but an error will occur if this reference
//            is ever used, except to test whether it has been closed. *)
//         (b, X(R(Win_value(ctx.context_win))), [], [])

//     | Named_win(X(R(String_value(wn)))) ->
//         begin match win_from_win_name wn b with
//         | None -> (b, X(R(Null_value)), [], [])
//         | Some(wr) -> (b, X(R(Win_value(wr))), [], [])
//         end

//     | Open_win(X(R(Url_value(u)))) ->
//         let wo = Win_opener(ctx.context_win) in
//         let (wr, b', oes) = open_win No_name u wo b in
//         (b', X(R(Win_value(wr))), oes, [])
//     | Open_named_win(X(R(Url_value(u))), X(R(String_value(str)))) ->
//         begin match win_from_win_name str b with
//         | None ->
//             let wo = Win_opener(ctx.context_win) in
//             let (wr, b', oes) = open_win (Str_name(str)) u wo b in
//             (b', X(R(Win_value(wr))), oes, [])
//         | Some(wr) ->
//             let (b', oes) = direct_win wr u b in
//             (b', X(R(Win_value(wr))), oes, [])
//         end
//     | Close_win(X(R(Win_value(wr)))) ->
//         let oes =
//           if win_valid wr b then [ UI_win_closed_event(win_to_user_window wr b) ]
//           else []
//         in
//         (win_remove wr b, X(R(Null_value)), oes, [])
//     | Navigate_win(
//           X(R(Win_value(wr))),
//           X(R(Url_value(url)))) ->
//         begin if win_valid wr b then
//           let (b', oes) = direct_win wr (url) b in
//           (b', X(R(Null_value)), oes, [])
//         else
//           let err = "window was closed---cannot set location" in
//           (b, X(R(Error(err))), [], [])
//         end

//     | Is_win_closed(X(R(Win_value(wr)))) ->
//         (b, X(R(Bool_value(not (win_valid wr b)))), [], [])
//     | Get_win_opener(X(R(Win_value(wr)))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err = "window was closed---cannot get opener" in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             begin match w.win_opener with
//             | No_opener -> (b, X(R(Null_value)), [], [])
//             | Win_opener(wr') -> (b, X(R(Win_value(wr))), [], [])
//             end
//         end
//     | Get_win_location(X(R(Win_value(wr)))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err = "window was closed---cannot get location" in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             let u =
//               (page_assoc_valid w.win_page b).page_location
//             in
//             (b, X(R(Url_value(u))), [], [])
//         end

//     | Get_win_name(X(R(Win_value(wr)))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err = "window was closed---cannot get name" in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             begin match w.win_name with
//             | No_name ->
//                 (b, X(R(Null_value)), [], [])
//             | Str_name(str) ->
//                 (b, X(R(String_value(str))), [], [])
//             end
//         end

//     | Set_win_name(X(R(Win_value(wr))), X(R(Null_value))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err = "window was closed---cannot unset name" in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             let w' = { w with win_name = No_name } in
//             let b' = win_update wr w' b in
//             (b', X(R(Null_value)), [], [])
//         end
//     | Set_win_name(X(R(Win_value(wr))), X(R(String_value(str)))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err = "window was closed---cannot set name" in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             let w' = { w with win_name = Str_name(str) } in
//             let b' = win_update wr w' b in
//             (b', X(R(Null_value)), [], [])
//         end

//     | Get_win_root_node(X(R(Win_value(wr)))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err = "window was closed---cannot get root node" in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             begin match (page_assoc_valid w.win_page b).page_document with
//             | None ->
//                 (b, X(R(Null_value)), [], [])
//             | Some(dr) ->
//                 (b, X(R(Node_value(dr))), [], [])
//             end
//         end
//     | Set_win_root_node(
//           X(R(Win_value(wr))),
//           X(R(Node_value(dr)))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err = "window was closed---cannot set root node" in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             let (b', oes1) = node_remove dr b in
//             let p = page_assoc_valid w.win_page b in
//             let p' = { p with page_document = Some(dr) } in
//             let b'' = page_update w.win_page p' b' in
//             let (b''', oes2, ts) = process_node_scripts w.win_page dr b'' in
//             let oes = oes1 @ [ page_update_event w.win_page b''' ] @ oes2 in
//             (b''', X(R(Null_value)), oes, ts)
//         end

//     | Get_win_var(X(R(Win_value(wr))), x) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err =
//               Printf.sprintf "window was closed---cannot get variable %S"
//                 x.var_name
//             in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             let ar = (page_assoc_valid w.win_page b).page_environment in
//             begin match get_var x ar b with
//             | None ->
//                 let err =
//                   Printf.sprintf "window variable %S not found" x.var_name
//                 in
//                 (b, X(R(Error(err))), [], [])
//             | Some(r) ->
//                 (b, X(R(r)), [], [])
//             end
//         end
//     | Set_win_var(X(R(Win_value(wr))), x, X(R(r2))) ->
//         begin match win_assoc wr b with
//         | None ->
//             let err =
//               Printf.sprintf "window was closed---cannot set variable %s"
//                 x.var_name
//             in
//             (b, X(R(Error(err))), [], [])
//         | Some(w) ->
//             let ar = (page_assoc_valid w.win_page b).page_environment in
//             let b' = set_var x r2 ar b in
//             (b', X(R(Null_value)), [], [])
//         end

//     | New_node(X(R(String_value("para")))) ->
//         let (dr, b') = node_new (Para_node(None, "")) b in
//         (b', X(R(Node_value(dr))), [], [])

//     | New_node(X(R(String_value("link")))) ->
//         let (dr, b') = node_new (Link_node(None, Blank_url, "")) b in
//         (b', X(R(Node_value(dr))), [], [])

//     | New_node(X(R(String_value("textbox")))) ->
//         let (dr, b') = node_new (Textbox_node(None, "", [])) b in
//         (b', X(R(Node_value(dr))), [], [])

//     | New_node(X(R(String_value("button")))) ->
//         let (dr, b') = node_new (Button_node(None, "", [])) b in
//         (b', X(R(Node_value(dr))), [], [])

//     | New_node(X(R(String_value("inl_script")))) ->
//         let (dr, b') = node_new (Inl_script_node(None, Null, false)) b in
//         (b', X(R(Node_value(dr))), [], [])

//     | New_node(X(R(String_value("rem_script")))) ->
//         let (dr, b') = node_new (Rem_script_node(None, Blank_url, false)) b in
//         (b', X(R(Node_value(dr))), [], [])

//     | New_node(X(R(String_value("div")))) ->
//         let (dr, b') = node_new (Div_node(None, [])) b in
//         (b', X(R(Node_value(dr))), [], [])

//     | New_node(X(R(String_value(_)))) ->
//         let err = "expected valid node type string" in
//         (b, X(R(Error(err))), [], [])

//     | Get_node_type(X(R(Node_value(dr)))) ->
//         begin match node_assoc_valid dr b with
//         | Para_node(_, _) ->
//             (b, X(R(String_value("para"))), [], [])
//         | Link_node(_, _, _) ->
//             (b, X(R(String_value("link"))), [], [])
//         | Textbox_node(_, _, _) ->
//             (b, X(R(String_value("textbox"))), [], [])
//         | Button_node(_, _, _) ->
//             (b, X(R(String_value("button"))), [], [])
//         | Inl_script_node(_, _, _) ->
//             (b, X(R(String_value("inl_script"))), [], [])
//         | Rem_script_node(_, _, _) ->
//             (b, X(R(String_value("rem_script"))), [], [])
//         | Div_node(_, _) ->
//             (b, X(R(String_value("div"))), [], [])
//         end

//     | Get_node_contents(X(R(Node_value(dr)))) ->
//         begin match node_assoc_valid dr b with
//         | Para_node(_, txt) ->
//             (b, X(R(String_value(txt))), [], [])
//         | Link_node(_, _, txt) ->
//             (b, X(R(String_value(txt))), [], [])
//         | Button_node(_, txt, _) ->
//             (b, X(R(String_value(txt))), [], [])
//         | Inl_script_node(_, e, _) ->
//             (b, X(R(Code_value(e))), [], [])
//         | _ ->
//             let err = "node has no contents" in
//             (b, X(R(Error(err))), [], [])
//         end
//     | Set_node_contents(X(R(Node_value(dr))), X(R(String_value(s)))) ->
//         begin match node_assoc_valid dr b with
//         | Para_node(oeid, _) ->
//             let b' = node_update dr (Para_node(oeid, s)) b in
//             (b', X(R(String_value(s))), [], [])
//         | Link_node(oeid, u, _) ->
//             let b' = node_update dr (Link_node(oeid, u, s)) b in
//             (b', X(R(String_value(s))), [], [])
//         | Button_node(oeid, _, hs) ->
//             let b' = node_update dr (Button_node(oeid, s, hs)) b in
//             (b', X(R(String_value(s))), [], [])
//         | _ ->
//             let err = "node has no string contents" in
//             (b, X(R(Error(err))), [], [])
//         end
//     | Set_node_contents(X(R(Node_value(dr))), X(R(Code_value(e)))) ->
//         begin match node_assoc_valid dr b with
//         | Inl_script_node(oeid, _, flag) ->
//             let b' = node_update dr (Inl_script_node(oeid, e, flag)) b in
//             (b', X(R(Code_value(e))), [], [])
//         | _ ->
//             let err = "node has no script contents" in
//             (b, X(R(Error(err))), [], [])
//         end

//     | Get_node_attr(X(R(Node_value(dr))), X(R(String_value("id")))) ->
//         begin match node_assoc_valid dr b with
//         | Para_node(Some({ elt_id_value = id }), _)
//         | Link_node(Some({ elt_id_value = id }), _, _)
//         | Textbox_node(Some({ elt_id_value = id }), _, _)
//         | Button_node(Some({ elt_id_value = id }), _, _)
//         | Inl_script_node(Some({ elt_id_value = id }), _, _)
//         | Rem_script_node(Some({ elt_id_value = id }), _, _)
//         | Div_node(Some({ elt_id_value = id }), _) ->
//             (b, X(R(String_value(id))), [], [])
//         | _ ->
//             (b, X(R(Null_value)), [], [])
//         end
//     | Set_node_attr(
//           X(R(Node_value(dr))),
//           X(R(String_value("id"))),
//           X(R(Null_value))) ->
//         begin match node_assoc_valid dr b with
//         | Para_node(_, text) ->
//             let b' = node_update dr (Para_node(None, text)) b in
//             (b', X(R(Null_value)), [], [])
//         | Link_node(_, u, text) ->
//             let b' = node_update dr (Link_node(None, u, text)) b in
//             (b', X(R(Null_value)), [], [])
//         | Textbox_node(_, s, hs) ->
//             let b' = node_update dr (Textbox_node(None, s, hs)) b in
//             (b', X(R(Null_value)), [], [])
//         | Button_node(_, s, hs) ->
//             let b' = node_update dr (Button_node(None, s, hs)) b in
//             (b', X(R(Null_value)), [], [])
//         | Inl_script_node(_, e, flag) ->
//             let b' = node_update dr (Inl_script_node(None, e, flag)) b in
//             (b', X(R(Null_value)), [], [])
//         | Rem_script_node(_, u, flag) ->
//             let b' = node_update dr (Rem_script_node(None, u, flag)) b in
//             (b', X(R(Null_value)), [], [])
//         | Div_node(_, ns) ->
//             let b' = node_update dr (Div_node(None, ns)) b in
//             (b', X(R(Null_value)), [], [])
//         end
//     | Set_node_attr(
//           X(R(Node_value(dr))),
//           X(R(String_value("id"))),
//           X(R(String_value(s)))) ->
//         let oeid = Some({ elt_id_value = s }) in
//         begin match node_assoc_valid dr b with
//         | Para_node(_, text) ->
//             let b' = node_update dr (Para_node(oeid, text)) b in
//             (b', X(R(Null_value)), [], [])
//         | Link_node(_, u, text) ->
//             let b' = node_update dr (Link_node(oeid, u, text)) b in
//             (b', X(R(Null_value)), [], [])
//         | Textbox_node(_, s, hs) ->
//             let b' = node_update dr (Textbox_node(oeid, s, hs)) b in
//             (b', X(R(Null_value)), [], [])
//         | Button_node(_, s, hs) ->
//             let b' = node_update dr (Button_node(oeid, s, hs)) b in
//             (b', X(R(Null_value)), [], [])
//         | Inl_script_node(_, e, flag) ->
//             let b' = node_update dr (Inl_script_node(oeid, e, flag)) b in
//             (b', X(R(Null_value)), [], [])
//         | Rem_script_node(_, u, flag) ->
//             let b' = node_update dr (Rem_script_node(oeid, u, flag)) b in
//             (b', X(R(Null_value)), [], [])
//         | Div_node(_, ns) ->
//             let b' = node_update dr (Div_node(oeid, ns)) b in
//             (b', X(R(Null_value)), [], [])
//         end

//     | Get_node_attr(X(R(Node_value(dr))), X(R(String_value("href")))) ->
//         begin match node_assoc_valid dr b with
//         | Link_node(_, u, _) ->
//             (b, X(R(Url_value(u))), [], [])
//         | _ ->
//             let err = "node has no 'href' attribute" in
//             (b, X(R(Error(err))), [], [])
//         end
//     | Set_node_attr(
//           X(R(Node_value(dr))),
//           X(R(String_value("href"))),
//           X(R(Url_value(u)))) ->
//         begin match node_assoc_valid dr b with
//         | Link_node(oeid, _, text) ->
//             let b' = node_update dr (Link_node(oeid, u, text)) b in
//             let oes =
//               begin match node_page dr b' with
//               | None -> []
//               | Some(pr) -> [ page_update_event pr b' ]
//               end
//             in
//             (b', X(R(Url_value(u))), oes, [])
//         | _ ->
//             let err = "node has no 'href' attribute" in
//             (b, X(R(Error(err))), [], [])
//         end

//     | Get_node_attr(X(R(Node_value(dr))), X(R(String_value("value")))) ->
//         begin match node_assoc_valid dr b with
//         | Textbox_node(_, s, _) ->                             
//             (b, X(R(String_value(s))), [], [])
//         | _ ->
//             let err = "node has no 'value' attribute" in
//             (b, X(R(Error(err))), [], [])
//         end
//     | Set_node_attr(
//           X(R(Node_value(dr))),
//           X(R(String_value("value"))),
//           X(R(String_value(s)))) ->
//         begin match node_assoc_valid dr b with
//         | Textbox_node(oeid, _, handlers) ->
//             let b' = node_update dr (Textbox_node(oeid, s, handlers)) b in
//             let oes =
//               begin match node_page dr b' with
//               | None -> []
//               | Some(pr) -> [ page_update_event pr b' ]
//               end
//             in
//             (b', X(R(String_value(s))), oes, [])
//         | _ ->
//             let err = "node has no 'value' attribute" in
//             (b, X(R(Error(err))), [], [])
//         end

//     | Get_node_attr(X(R(Node_value(dr))), X(R(String_value("src")))) ->
//         begin match node_assoc_valid dr b with
//         | Rem_script_node(_, u, _) ->
//             (b, X(R(Url_value(u))), [], [])
//         | _ ->
//             let err = "node has no 'src' attribute" in
//             (b, X(R(Error(err))), [], [])
//         end
//     | Set_node_attr(
//           X(R(Node_value(dr))),
//           X(R(String_value("src"))),
//           X(R(Url_value(u)))) ->
//         begin match node_assoc_valid dr b with
//         | Rem_script_node(oeid, _, flag) ->
//             let b' = node_update dr (Rem_script_node(oeid, u, flag)) b in
//             (b', X(R(Url_value(u))), [], [])
//         | _ ->
//             let err = "node has no 'src' attribute" in
//             (b, X(R(Error(err))), [], [])
//         end

//     | Remove_handlers(X(R(Node_value(dr)))) ->
//         begin match node_assoc_valid dr b with
//         | Textbox_node(id, s, _) ->
//             let dn' = Textbox_node(id, s, []) in
//             let b' = node_update dr dn' b in
//             (b', X(R(Null_value)), [], [])
//         | Button_node(id, s, _) ->
//             let dn' = Button_node(id, s, []) in
//             let b' = node_update dr dn' b in
//             (b', X(R(Null_value)), [], [])
//         | _ ->
//             let err = "expected textbox or button node" in
//             (b, X(R(Error(err))), [], [])
//         end
//     | Add_handler(X(R(Node_value(dr))), X(R(Closure(_, _, _, _)))) ->
//         begin match node_assoc_valid dr b with
//         | Textbox_node(oeid, str, hs) ->
//             let b' = node_update dr (Textbox_node(oeid, str, Closure(_, _, _, _) :: hs)) b in
//             (b', X(R(Null_value)), [], [])
//         | Button_node(oeid, str, hs) ->
//             let b' = node_update dr (Button_node(oeid, str, Closure(_, _, _, _) :: hs)) b in
//             (b', X(R(Null_value)), [], [])
//         | _ ->
//             let err = "expected textbox or button node" in
//             (b, X(R(Error(err))), [], [])
//         end

//     | Get_parent(X(R(Node_value(dr)))) ->
//         begin match node_parent dr b with
//         | Parent_node(parent) ->
//             (b, X(R(Node_value(parent))), [], [])
//         | _ ->
//             (b, X(R(Null_value)), [], [])
//         end
//     | Get_child(X(R(Node_value(dr))), X(R(Int_value(i)))) ->
//         begin match node_assoc_valid dr b with
//         | Div_node(_, drs) ->
//             begin if List.length drs <= i then
//               (b, X(R(Null_value)), [], [])
//             else
//               (b, X(R(Node_value(List.nth drs i))), [], [])
//             end
//         | _ ->
//             let err = "expected div node" in
//             (b, X(R(Error(err))), [], [])
//         end
//     | Insert_node(
//           X(R(Node_value(dr1))),
//           X(R(Node_value(dr2))),
//           X(R(Int_value(k)))) ->
//         begin try
//           let (b', oes, ts) = node_insert dr1 dr2 k b in
//           (b', X(R(Null_value)), oes, ts)
//         with
//         | Failure("node_insert") ->
//             let err = "parent node not a div node or a descendant of child" in
//             (b, X(R(Error(err))), [], [])
//         | Failure("insert_in_list") ->
//             (b, X(R(Error("div node has too few children"))), [], [])
//         end
//     | Remove_node(X(R(Node_value(dr)))) ->
//         let (b', oes) = node_remove dr b in
//         (b', X(R(Null_value)), oes, [])

//     (* congurence rules: evaluation in subexpressions *)

//     | Eval(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b, Eval(e1'), oes, ts)
//     | Apply(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Apply(X(R(_)), e2'), oes, ts)
//     | Apply(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Apply(e1', e2), oes, ts)
//     | Prim1(prim, e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Prim1(prim, e1'), oes, ts)
//     | Prim2(prim, X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Prim2(prim, X(R(_)), e2'), oes, ts)
//     | Prim2(prim, e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Prim2(prim, e1', e2), oes, ts)
//     | Alert(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Alert(e1'), oes, ts)
//     | If(e1, e2, e3) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', If(e1', e2, e3), oes, ts)
//     | Set_var(x, e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Set_var(x, e1'), oes, ts)
//     | Seq(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Seq(e1', e2), oes, ts)
//     | Get_cookie(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Get_cookie(X(R(_)), e2'), oes, ts)
//     | Get_cookie(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_cookie(e1', e2), oes, ts)
//     | Set_cookie((X(R(_))), (X(R(_))), e3) ->
//         let (b', e3', oes, ts) = step b e3 in
//         (b', Set_cookie(X(R(_)), X(R(_)), e3'), oes, ts)
//     | Set_cookie((X(R(_))), e2, e3) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Set_cookie(X(R(_)), e2', e3), oes, ts)
//     | Set_cookie(e1, e2, e3) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Set_cookie(e1', e2, e3), oes, ts)
//     | Xhr((X(R(_))), (X(R(_))), e3) ->
//         let (b', e3', oes, ts) = step b e3 in
//         (b', Xhr(X(R(_)), X(R(_)), e3'), oes, ts)
//     | Xhr((X(R(_))), e2, e3) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Xhr(X(R(_)), e2', e3), oes, ts)
//     | Xhr(e1, e2, e3) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Xhr(e1', e2, e3), oes, ts)
//     | Named_win(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Named_win(e1'), oes, ts)
//     | Open_win(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Open_win(e1'), oes, ts)
//     | Open_named_win(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Open_named_win(X(R(_)), e2'), oes, ts)
//     | Open_named_win(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Open_named_win(e1', e2), oes, ts)
//     | Close_win(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Close_win(e1'), oes, ts)
//     | Navigate_win(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Navigate_win(X(R(_)), e2'), oes, ts)
//     | Navigate_win(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Navigate_win(e1', e2), oes, ts)
//     | Is_win_closed(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Is_win_closed(e1'), oes, ts)
//     | Get_win_opener(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_win_opener(e1'), oes, ts)
//     | Get_win_location(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_win_location(e1'), oes, ts)
//     | Get_win_name(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_win_name(e1'), oes, ts)
//     | Set_win_name(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Set_win_name(X(R(_)), e2'), oes, ts)
//     | Set_win_name(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Set_win_name(e1', e2), oes, ts)
//     | Get_win_root_node(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_win_root_node(e1'), oes, ts)
//     | Set_win_root_node(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Set_win_root_node(X(R(_)), e2'), oes, ts)
//     | Set_win_root_node(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Set_win_root_node(e1', e2), oes, ts)
//     | Get_win_var(e1, x) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_win_var(e1', x), oes, ts)
//     | Set_win_var(X(R(_)), x, e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Set_win_var(X(R(_)), x, e2'), oes, ts)
//     | Set_win_var(e1, x, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Set_win_var(e1', x, e2), oes, ts)
//     | New_node(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', New_node(e1'), oes, ts)
//     | Get_node_type(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_node_type(e1'), oes, ts)
//     | Get_node_contents(e1) -> 
//        let (b', e1', oes, ts) = step b e1 in
//         (b', Get_node_contents(e1'), oes, ts)
//     | Set_node_contents(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Set_node_contents(X(R(_)), e2'), oes, ts)
//     | Set_node_contents(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Set_node_contents(e1', e2), oes, ts)
//     | Get_node_attr(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Get_node_attr(X(R(_)), e2'), oes, ts)
//     | Get_node_attr(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_node_attr(e1', e2), oes, ts)
//     | Set_node_attr((X(R(_))), (X(R(_))), e3) ->
//         let (b', e3', oes, ts) = step b e3 in
//         (b', Set_node_attr(X(R(_)), X(R(_)), e3'), oes, ts)
//     | Set_node_attr((X(R(_))), e2, e3) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Set_node_attr(X(R(_)), e2', e3), oes, ts)
//     | Set_node_attr(e1, e2, e3) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Set_node_attr(e1', e2, e3), oes, ts)
//     | Remove_handlers(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Remove_handlers(e1'), oes, ts)
//     | Add_handler((X(R(_))), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Add_handler(X(R(_)), e2'), oes, ts)
//     | Add_handler(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Add_handler(e1', e2), oes, ts)
//     | Get_child(X(R(_)), e2) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Get_child(X(R(_)), e2'), oes, ts)
//     | Get_parent(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_parent(e1'), oes, ts)
//     | Get_child(e1, e2) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Get_child(e1', e2), oes, ts)
//     | Insert_node((X(R(_))), (X(R(_))), e3) ->
//         let (b', e3', oes, ts) = step b e3 in
//         (b', Insert_node(X(R(_)), X(R(_)), e3'), oes, ts)
//     | Insert_node((X(R(_))), e2, e3) ->
//         let (b', e2', oes, ts) = step b e2 in
//         (b', Insert_node(X(R(_)), e2', e3), oes, ts)
//     | Insert_node(e1, e2, e3) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Insert_node(e1', e2, e3), oes, ts)
//     | Remove_node(e1) ->
//         let (b', e1', oes, ts) = step b e1 in
//         (b', Remove_node(e1'), oes, ts)

//     end
