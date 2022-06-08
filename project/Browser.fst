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
let fresh_node_ref (node:node_ref): node_ref = node + 1

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
    win_opener: win_ref;
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
| Scoped_expr: context -> expr inner-> inner
(** [Scoped_expr(cxt, e)] is a subexpression that executes in a different
          context than the enclosing expression. *)

| Value : value -> inner 


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
    // (let open FStar.Mul in n1 * n2)
    | ("+", Int_value(n1), Int_value(n2)) -> Int_value(n1 + n2)
    | ("-", Int_value(n1), Int_value(n2)) -> Int_value(n1 - n2)
    | ("addtourl", Url_value(u), String_value(s)) -> Url_value
        ( match u with 
            | Blank_url -> Blank_url
            | Http_url domain ({req_uri_path = path; req_uri_params = params}) ->
                 Http_url domain ({req_uri_path = path;
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
| Para_node: option elt_id -> string -> node

| Link_node: option elt_id -> url -> string -> node

| Text_node: option elt_id -> string -> list value -> node

| Button_node: option elt_id -> string -> list value -> node

| Remote_script_node: option elt_id -> url -> bool -> node 

| Div_node: elt_id -> list node_ref -> node 
(** Represents a sequence of nodes in the document. list contains references to the children*)

| Inline_script_node: option elt_id -> expr False -> bool -> node


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

    page_script: list queued_expr;
}

(** The type of an activation record in the scripting language. *)
type act = {
    act_parent: option act_ref;

    act_vars: list (var * value);
}

(* typf eor open network connection *)
type open_connection=

| Doc_connection: domain -> req_uri -> win_ref -> open_connection

| Script_connection: domain -> req_uri -> page_ref -> node_ref -> open_connection

| Xhr_connection: domain -> req_uri -> page_ref -> value -> open_connection


(** The type of a script expression queued for execution. *)
  type task = {
    task_win: win_ref;
    task_expr: expr inner;
  }

let b_act_ref_pred (r:act_ref) (a:act) : bool = 
    match a.act_parent with | None -> true | Some ap -> r > ap

let rec b_env_pred (l:list (act_ref * act)) : bool = 
  match l with 
  | [] -> true
  | (r, a)::tl -> b_act_ref_pred r a && (b_env_pred tl)

type b_env = l:(list (act_ref * act)){b_env_pred l}

  (** A typf eor the basic state of a browser. *)
type browser = {

    browser_windows: list (win_ref * win) ;
    
    browser_pages: list (page_ref * page);
   
    browser_nodes: list (node_ref * node);
  
    browser_environments: b_env ;
   
    browser_cookies: list (cookie_id * string) ;
   
    browser_connections: list open_connection ;
   
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

(** redundant **)

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
    List.assoc wr b.browser_windows  

(** redundant **)

  // (** [win_assoc_valid wr b] returns the [win] associated with [wr] in the window
  //     store of [b]. *)
  // let win_assoc_valid (wr: win_ref) (b: browser)
  // : win =
  //     begin try List.assoc wr b.browser_windows with
  //     | None -> assert false
  //     end

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

      // need to implement 
// let win_from_user_window ((u, n): user_window) (b:browser
// {(Some? (assoc w.win_page b.browser_windows))})
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

   
// nned to implement

  // let win_to_user_window (wr: win_ref) (b: b)
  // : user_window =
  //   let w = win_assoc_valid wr b in
  //   let u = (page_assoc_valid w.win_page b).page_location in
  //   let has_url (_, w) =
  //     (page_assoc_valid w.win_page b).page_location = u
  //   in
  //   let windows' = List.filter has_url b.browser_windows in
  //   let rec find wr ws n =
  //     begin match ws with
  //     | (wr', _) :: _ when wr' = wr -> n
  //     | _ :: ws' -> find wr ws' (succ n)
  //     | _ -> assert false
  //     end
  //   in
  //   User_window(u, find wr windows' 0)


(** {4 Browser node store} *)

  (** [node_valid dr b] returns [true] if [dr] is in the node store of
      [b]. *)
  let node_valid (dr: node_ref) (b: browser)
  : bool =
    match (assoc dr b.browser_windows) with 
  | None -> false
  | _ -> true

(** [node_assoc_valid dr b] returns the [node] associated with [dr] in the
      node store of [b]. *)

      // need to implement
  let node_assoc_valid (dr: node_ref) (b: browser
  {(Some? (assoc dr b.browser_nodes))}) : node =

  Some?.v (assoc dr b.browser_nodes)

  (** [node_update dr dn b] associates [dr] with the node [dn] in [b]. *)
  let node_update (dr: node_ref) (dn: node) (b: browser)
  : browser =
    let nodes' = upd_assoc dr dn b.browser_nodes in
    { b with browser_nodes = nodes' }

(** [node_new dn b] adds [dn] to the node store of [b] and returns its
      fresh key. *)
  let node_new (dn: node) (b: browser)
  : node_ref * browser =
  let n_ref = 
  (if length (b.browser_nodes) >=1 then (fst (last b.browser_nodes))
  else 0) in
    let nr = fresh_win_ref (n_ref) in
    let b' = node_update nr dn b in
    (nr, b')

(** The type of a node's parent object. *)
type node_parent_type =
    | No_parent
    | Page_parent of page_ref
    | Parent_node of node_ref



(** Finds the parent of a node. *)
let node_parent (dr: node_ref) (b: browser)
  : node_parent_type =
    let is_page_parent (_, p) = 
    begin match p.page_document with
    | Some dr1 -> if (dr1 = dr) then true else false
    | _ -> false
    end
     in
    let is_node_parent (_, dn) =
      begin match dn with
      | Div_node _ children -> mem dr children
      | _ -> false
      end
    in
    begin match filter is_page_parent b.browser_pages with
    | (pr, _) :: _ -> Page_parent(pr)
    | [] ->
        begin match filter is_node_parent b.browser_nodes with
        | (dr1, _) :: _ -> Parent_node(dr1)
        | [] -> No_parent
        end
  end

  (** Finds the page displaying a node if there is one. *)
  // ---TO DO --
  // let rec node_page (dr: node_ref) (b: browser)
  // : option page_ref =
  //   begin match node_parent dr b with
  //   | No_parent -> None
  //   | Page_parent (pr) -> Some pr
  //   | Parent_node (dr1) -> node_page dr1 b
  //   end


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


