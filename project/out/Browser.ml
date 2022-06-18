open Prims
type win_ref = Prims.nat
let (fresh_win_ref : win_ref -> win_ref) = fun win -> win + Prims.int_one
type page_ref = Prims.nat
let (fresh_page_ref : page_ref -> page_ref) =
  fun page -> page + Prims.int_one
type node_ref = Prims.nat
let (fresh_node_ref : node_ref -> node_ref) =
  fun nref -> nref + Prims.int_one
type act_ref = Prims.nat
let (fresh_act_ref : act_ref -> act_ref) = fun act -> act + Prims.int_one
type cookie_id =
  {
  cookie_id_domain: BrowserIO.domain ;
  cookie_id_path: BrowserIO.path ;
  cookie_id_key: Prims.string }
let (__proj__Mkcookie_id__item__cookie_id_domain :
  cookie_id -> BrowserIO.domain) =
  fun projectee ->
    match projectee with
    | { cookie_id_domain; cookie_id_path; cookie_id_key;_} ->
        cookie_id_domain
let (__proj__Mkcookie_id__item__cookie_id_path : cookie_id -> BrowserIO.path)
  =
  fun projectee ->
    match projectee with
    | { cookie_id_domain; cookie_id_path; cookie_id_key;_} -> cookie_id_path
let (__proj__Mkcookie_id__item__cookie_id_key : cookie_id -> Prims.string) =
  fun projectee ->
    match projectee with
    | { cookie_id_domain; cookie_id_path; cookie_id_key;_} -> cookie_id_key
type win_name =
  | No_name 
  | Str_name of Prims.string 
let (uu___is_No_name : win_name -> Prims.bool) =
  fun projectee -> match projectee with | No_name -> true | uu___ -> false
let (uu___is_Str_name : win_name -> Prims.bool) =
  fun projectee ->
    match projectee with | Str_name _0 -> true | uu___ -> false
let (__proj__Str_name__item___0 : win_name -> Prims.string) =
  fun projectee -> match projectee with | Str_name _0 -> _0
type win_opener =
  | No_opener 
  | Win_opener of win_ref 
let (uu___is_No_opener : win_opener -> Prims.bool) =
  fun projectee -> match projectee with | No_opener -> true | uu___ -> false
let (uu___is_Win_opener : win_opener -> Prims.bool) =
  fun projectee ->
    match projectee with | Win_opener _0 -> true | uu___ -> false
let (__proj__Win_opener__item___0 : win_opener -> win_ref) =
  fun projectee -> match projectee with | Win_opener _0 -> _0
type win = {
  win_name: win_name ;
  win_opener: win_opener ;
  win_page: page_ref }
let (__proj__Mkwin__item__win_name : win -> win_name) =
  fun projectee ->
    match projectee with
    | { win_name = win_name1; win_opener = win_opener1; win_page;_} ->
        win_name1
let (__proj__Mkwin__item__win_opener : win -> win_opener) =
  fun projectee ->
    match projectee with
    | { win_name = win_name1; win_opener = win_opener1; win_page;_} ->
        win_opener1
let (__proj__Mkwin__item__win_page : win -> page_ref) =
  fun projectee ->
    match projectee with
    | { win_name = win_name1; win_opener = win_opener1; win_page;_} ->
        win_page
type context = {
  context_win: win_ref ;
  context_act: act_ref }
let (__proj__Mkcontext__item__context_win : context -> win_ref) =
  fun projectee ->
    match projectee with | { context_win; context_act;_} -> context_win
let (__proj__Mkcontext__item__context_act : context -> act_ref) =
  fun projectee ->
    match projectee with | { context_win; context_act;_} -> context_act
type inner =
  | Scoped_expr of (context * inner BrowserIO.expr) 
  | R of value 
and value =
  | Closure of context * BrowserIO.var * BrowserIO.var Prims.list * inner
  BrowserIO.expr 
  | Win_value of win_ref 
  | Node_value of node_ref 
  | Null_value 
  | Bool_value of Prims.bool 
  | Int_value of Prims.int 
  | String_value of Prims.string 
  | Url_value of BrowserIO.url 
  | Type_value of BrowserIO.typ 
  | Code_value of unit BrowserIO.expr 
  | Error of Prims.string 
let (uu___is_Scoped_expr : inner -> Prims.bool) =
  fun projectee ->
    match projectee with | Scoped_expr _0 -> true | uu___ -> false
let (__proj__Scoped_expr__item___0 :
  inner -> (context * inner BrowserIO.expr)) =
  fun projectee -> match projectee with | Scoped_expr _0 -> _0
let (uu___is_R : inner -> Prims.bool) =
  fun projectee -> match projectee with | R _0 -> true | uu___ -> false
let (__proj__R__item___0 : inner -> value) =
  fun projectee -> match projectee with | R _0 -> _0
let (uu___is_Closure : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Closure (_0, _1, _2, _3) -> true | uu___ -> false
let (__proj__Closure__item___0 : value -> context) =
  fun projectee -> match projectee with | Closure (_0, _1, _2, _3) -> _0
let (__proj__Closure__item___1 : value -> BrowserIO.var) =
  fun projectee -> match projectee with | Closure (_0, _1, _2, _3) -> _1
let (__proj__Closure__item___2 : value -> BrowserIO.var Prims.list) =
  fun projectee -> match projectee with | Closure (_0, _1, _2, _3) -> _2
let (__proj__Closure__item___3 : value -> inner BrowserIO.expr) =
  fun projectee -> match projectee with | Closure (_0, _1, _2, _3) -> _3
let (uu___is_Win_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Win_value _0 -> true | uu___ -> false
let (__proj__Win_value__item___0 : value -> win_ref) =
  fun projectee -> match projectee with | Win_value _0 -> _0
let (uu___is_Node_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Node_value _0 -> true | uu___ -> false
let (__proj__Node_value__item___0 : value -> node_ref) =
  fun projectee -> match projectee with | Node_value _0 -> _0
let (uu___is_Null_value : value -> Prims.bool) =
  fun projectee -> match projectee with | Null_value -> true | uu___ -> false
let (uu___is_Bool_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Bool_value _0 -> true | uu___ -> false
let (__proj__Bool_value__item___0 : value -> Prims.bool) =
  fun projectee -> match projectee with | Bool_value _0 -> _0
let (uu___is_Int_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Int_value _0 -> true | uu___ -> false
let (__proj__Int_value__item___0 : value -> Prims.int) =
  fun projectee -> match projectee with | Int_value _0 -> _0
let (uu___is_String_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | String_value _0 -> true | uu___ -> false
let (__proj__String_value__item___0 : value -> Prims.string) =
  fun projectee -> match projectee with | String_value _0 -> _0
let (uu___is_Url_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Url_value _0 -> true | uu___ -> false
let (__proj__Url_value__item___0 : value -> BrowserIO.url) =
  fun projectee -> match projectee with | Url_value _0 -> _0
let (uu___is_Type_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Type_value _0 -> true | uu___ -> false
let (__proj__Type_value__item___0 : value -> BrowserIO.typ) =
  fun projectee -> match projectee with | Type_value _0 -> _0
let (uu___is_Code_value : value -> Prims.bool) =
  fun projectee ->
    match projectee with | Code_value _0 -> true | uu___ -> false
let (__proj__Code_value__item___0 : value -> Prims.l_False BrowserIO.expr) =
  fun projectee -> match projectee with | Code_value _0 -> _0
let (uu___is_Error : value -> Prims.bool) =
  fun projectee -> match projectee with | Error _0 -> true | uu___ -> false
let (__proj__Error__item___0 : value -> Prims.string) =
  fun projectee -> match projectee with | Error _0 -> _0
let (prim1 : Prims.string -> value -> value) =
  fun prim ->
    fun r ->
      match (prim, r) with
      | ("!", Bool_value bl) -> Bool_value (Prims.op_Negation bl)
      | ("-", Int_value n) -> Int_value (- n)
      | ("typeof", Null_value) -> Type_value BrowserIO.Null_type
      | ("typeof", Bool_value uu___) -> Type_value BrowserIO.Bool_type
      | ("typeof", Int_value uu___) -> Type_value BrowserIO.Int_type
      | ("typeof", String_value uu___) -> Type_value BrowserIO.String_type
      | ("typeof", Url_value uu___) -> Type_value BrowserIO.Url_type
      | ("typeof", Type_value uu___) -> Type_value BrowserIO.Type_type
      | ("typeof", Code_value uu___) -> Type_value BrowserIO.Code_type
      | ("typeof", Win_value uu___) -> Type_value BrowserIO.Window_type
      | ("typeof", Node_value uu___) -> Type_value BrowserIO.Node_type
      | ("typeof", Closure (uu___, uu___1, uu___2, uu___3)) ->
          Type_value BrowserIO.Function_type
      | ("StringToInt", String_value s) ->
          Int_value (Assumed.int_of_string s)
      | ("IntToString", Int_value i) -> String_value (Prims.string_of_int i)
      | (uu___, Error uu___1) -> r
      | (uu___, uu___1) ->
          Error
            (Prims.strcat "primitive operation ("
               (Prims.strcat prim ") is not implemented on this type"))
let (prim2 : Prims.string -> value -> value -> value) =
  fun prim ->
    fun r1 ->
      fun r2 ->
        match (prim, r1, r2) with
        | ("==", uu___, uu___1) -> Bool_value (r1 = r2)
        | ("||", Bool_value bl1, Bool_value bl2) -> Bool_value (bl1 || bl2)
        | ("&&", Bool_value bl1, Bool_value bl2) -> Bool_value (bl1 && bl2)
        | ("+", String_value s1, String_value s2) ->
            String_value (Prims.strcat s1 s2)
        | ("*", Int_value n1, Int_value n2) -> Int_value (n1 * n2)
        | ("+", Int_value n1, Int_value n2) -> Int_value (n1 + n2)
        | ("-", Int_value n1, Int_value n2) -> Int_value (n1 - n2)
        | ("addtourl", Url_value u, String_value s) ->
            Url_value
              ((match u with
                | BrowserIO.Blank_url -> BrowserIO.Blank_url
                | BrowserIO.Http_url
                    (domain,
                     { BrowserIO.req_uri_path = path;
                       BrowserIO.req_uri_params = params;_})
                    ->
                    BrowserIO.Http_url
                      (domain,
                        {
                          BrowserIO.req_uri_path = path;
                          BrowserIO.req_uri_params =
                            (Prims.strcat params (Prims.strcat "?t=" s))
                        })))
        | (uu___, Error uu___1, uu___2) -> r1
        | (uu___, uu___1, Error uu___2) -> r2
        | (uu___, uu___1, uu___2) ->
            Error
              (Prims.strcat "primitive operation ("
                 (Prims.strcat prim ") is not implemented for these types"))
let rec (to_inner_expr : unit BrowserIO.expr -> inner BrowserIO.expr) =
  fun e ->
    let f = to_inner_expr in
    match e with
    | BrowserIO.X uu___ -> BrowserIO.Null
    | BrowserIO.Null -> BrowserIO.Null
    | BrowserIO.Bool b -> BrowserIO.Bool b
    | BrowserIO.Int n -> BrowserIO.Int n
    | BrowserIO.String s -> BrowserIO.String s
    | BrowserIO.Url u -> BrowserIO.Url u
    | BrowserIO.Types t -> BrowserIO.Types t
    | BrowserIO.Code e1 -> BrowserIO.Code e1
    | BrowserIO.Eval e1 -> BrowserIO.Eval (f e1)
    | BrowserIO.Var x -> BrowserIO.Var x
    | BrowserIO.Function (x, xs, e1) -> BrowserIO.Function (x, xs, (f e1))
    | BrowserIO.Apply (e1, e2) -> BrowserIO.Apply ((f e1), (f e2))
    | BrowserIO.Prim1 (s, e1) -> BrowserIO.Prim1 (s, (f e1))
    | BrowserIO.Prim2 (s, e1, e2) -> BrowserIO.Prim2 (s, (f e1), (f e2))
    | BrowserIO.Alert e1 -> BrowserIO.Alert (f e1)
    | BrowserIO.If (e1, e2, e3) -> BrowserIO.If ((f e1), (f e2), (f e3))
    | BrowserIO.While (e1, e2) -> BrowserIO.While ((f e1), (f e2))
    | BrowserIO.Set_var (var, e1) -> BrowserIO.Set_var (var, (f e1))
    | BrowserIO.Seq (e1, e2) -> BrowserIO.Seq ((f e1), (f e2))
    | BrowserIO.Get_cookie (e1, e2) -> BrowserIO.Get_cookie ((f e1), (f e2))
    | BrowserIO.Set_cookie (e1, e2, e3) ->
        BrowserIO.Set_cookie ((f e1), (f e2), (f e3))
    | BrowserIO.Xhr (e1, e2, e3) -> BrowserIO.Xhr ((f e1), (f e2), (f e3))
    | BrowserIO.Self_win -> BrowserIO.Self_win
    | BrowserIO.Named_win e1 -> BrowserIO.Named_win (f e1)
    | BrowserIO.Open_win e1 -> BrowserIO.Open_win (f e1)
    | BrowserIO.Open_named_win (e1, e2) ->
        BrowserIO.Open_named_win ((f e1), (f e2))
    | BrowserIO.Close_win e1 -> BrowserIO.Close_win (f e1)
    | BrowserIO.Navigate_win (e1, e2) ->
        BrowserIO.Navigate_win ((f e1), (f e2))
    | BrowserIO.Is_win_closed e1 -> BrowserIO.Is_win_closed (f e1)
    | BrowserIO.Get_win_opener e1 -> BrowserIO.Get_win_opener (f e1)
    | BrowserIO.Get_win_location e1 -> BrowserIO.Get_win_location (f e1)
    | BrowserIO.Get_win_name e1 -> BrowserIO.Get_win_name (f e1)
    | BrowserIO.Set_win_name (e1, e2) ->
        BrowserIO.Set_win_name ((f e1), (f e2))
    | BrowserIO.Get_win_root_node e1 -> BrowserIO.Get_win_root_node (f e1)
    | BrowserIO.Set_win_root_node (e1, e2) ->
        BrowserIO.Set_win_root_node ((f e1), (f e2))
    | BrowserIO.Get_win_var (e1, var) -> BrowserIO.Get_win_var ((f e1), var)
    | BrowserIO.Set_win_var (e1, var, e2) ->
        BrowserIO.Set_win_var ((f e1), var, (f e2))
    | BrowserIO.New_node e1 -> BrowserIO.New_node (f e1)
    | BrowserIO.Get_node_type e1 -> BrowserIO.Get_node_type (f e1)
    | BrowserIO.Get_node_contents e1 -> BrowserIO.Get_node_contents (f e1)
    | BrowserIO.Set_node_contents (e1, e2) ->
        BrowserIO.Set_node_contents ((f e1), (f e2))
    | BrowserIO.Get_node_attr (e1, e2) ->
        BrowserIO.Get_node_attr ((f e1), (f e2))
    | BrowserIO.Set_node_attr (e1, e2, e3) ->
        BrowserIO.Set_node_attr ((f e1), (f e2), (f e3))
    | BrowserIO.Remove_handlers e1 -> BrowserIO.Remove_handlers (f e1)
    | BrowserIO.Add_handler (e1, e2) ->
        BrowserIO.Add_handler ((f e1), (f e2))
    | BrowserIO.Get_parent e1 -> BrowserIO.Get_parent (f e1)
    | BrowserIO.Get_child (e1, e2) -> BrowserIO.Get_child ((f e1), (f e2))
    | BrowserIO.Insert_node (e1, e2, e3) ->
        BrowserIO.Insert_node ((f e1), (f e2), (f e3))
    | BrowserIO.Remove_node e1 -> BrowserIO.Remove_node (f e1)
type node =
  | Para_node of (BrowserIO.elt_id FStar_Pervasives_Native.option *
  Prims.string) 
  | Link_node of (BrowserIO.elt_id FStar_Pervasives_Native.option *
  BrowserIO.url * Prims.string) 
  | Textbox_node of (BrowserIO.elt_id FStar_Pervasives_Native.option *
  Prims.string * value Prims.list) 
  | Button_node of (BrowserIO.elt_id FStar_Pervasives_Native.option *
  Prims.string * value Prims.list) 
  | Inl_script_node of (BrowserIO.elt_id FStar_Pervasives_Native.option *
  unit BrowserIO.expr * Prims.bool) 
  | Rem_script_node of (BrowserIO.elt_id FStar_Pervasives_Native.option *
  BrowserIO.url * Prims.bool) 
  | Div_node of (BrowserIO.elt_id FStar_Pervasives_Native.option * node_ref
  Prims.list) 
let (uu___is_Para_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Para_node _0 -> true | uu___ -> false
let (__proj__Para_node__item___0 :
  node -> (BrowserIO.elt_id FStar_Pervasives_Native.option * Prims.string)) =
  fun projectee -> match projectee with | Para_node _0 -> _0
let (uu___is_Link_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Link_node _0 -> true | uu___ -> false
let (__proj__Link_node__item___0 :
  node ->
    (BrowserIO.elt_id FStar_Pervasives_Native.option * BrowserIO.url *
      Prims.string))
  = fun projectee -> match projectee with | Link_node _0 -> _0
let (uu___is_Textbox_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Textbox_node _0 -> true | uu___ -> false
let (__proj__Textbox_node__item___0 :
  node ->
    (BrowserIO.elt_id FStar_Pervasives_Native.option * Prims.string * value
      Prims.list))
  = fun projectee -> match projectee with | Textbox_node _0 -> _0
let (uu___is_Button_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Button_node _0 -> true | uu___ -> false
let (__proj__Button_node__item___0 :
  node ->
    (BrowserIO.elt_id FStar_Pervasives_Native.option * Prims.string * value
      Prims.list))
  = fun projectee -> match projectee with | Button_node _0 -> _0
let (uu___is_Inl_script_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Inl_script_node _0 -> true | uu___ -> false
let (__proj__Inl_script_node__item___0 :
  node ->
    (BrowserIO.elt_id FStar_Pervasives_Native.option * unit BrowserIO.expr *
      Prims.bool))
  = fun projectee -> match projectee with | Inl_script_node _0 -> _0
let (uu___is_Rem_script_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Rem_script_node _0 -> true | uu___ -> false
let (__proj__Rem_script_node__item___0 :
  node ->
    (BrowserIO.elt_id FStar_Pervasives_Native.option * BrowserIO.url *
      Prims.bool))
  = fun projectee -> match projectee with | Rem_script_node _0 -> _0
let (uu___is_Div_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Div_node _0 -> true | uu___ -> false
let (__proj__Div_node__item___0 :
  node ->
    (BrowserIO.elt_id FStar_Pervasives_Native.option * node_ref Prims.list))
  = fun projectee -> match projectee with | Div_node _0 -> _0
type queued_expr =
  | Known_expr of inner BrowserIO.expr 
  | Unknown_expr of node_ref 
let (uu___is_Known_expr : queued_expr -> Prims.bool) =
  fun projectee ->
    match projectee with | Known_expr _0 -> true | uu___ -> false
let (__proj__Known_expr__item___0 : queued_expr -> inner BrowserIO.expr) =
  fun projectee -> match projectee with | Known_expr _0 -> _0
let (uu___is_Unknown_expr : queued_expr -> Prims.bool) =
  fun projectee ->
    match projectee with | Unknown_expr _0 -> true | uu___ -> false
let (__proj__Unknown_expr__item___0 : queued_expr -> node_ref) =
  fun projectee -> match projectee with | Unknown_expr _0 -> _0
type page =
  {
  page_location: BrowserIO.url ;
  page_document: node_ref FStar_Pervasives_Native.option ;
  page_environment: act_ref ;
  page_script_queue: queued_expr Prims.list }
let (__proj__Mkpage__item__page_location : page -> BrowserIO.url) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script_queue;_}
        -> page_location
let (__proj__Mkpage__item__page_document :
  page -> node_ref FStar_Pervasives_Native.option) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script_queue;_}
        -> page_document
let (__proj__Mkpage__item__page_environment : page -> act_ref) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script_queue;_}
        -> page_environment
let (__proj__Mkpage__item__page_script_queue :
  page -> queued_expr Prims.list) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script_queue;_}
        -> page_script_queue
type act =
  {
  act_parent: act_ref FStar_Pervasives_Native.option ;
  act_vars: (BrowserIO.var * value) Prims.list }
let (__proj__Mkact__item__act_parent :
  act -> act_ref FStar_Pervasives_Native.option) =
  fun projectee ->
    match projectee with | { act_parent; act_vars;_} -> act_parent
let (__proj__Mkact__item__act_vars :
  act -> (BrowserIO.var * value) Prims.list) =
  fun projectee ->
    match projectee with | { act_parent; act_vars;_} -> act_vars
type dst =
  | Xhr_dst of (page_ref * value) 
  | Doc_dst of win_ref 
  | Script_dst of (page_ref * node_ref) 
let (uu___is_Xhr_dst : dst -> Prims.bool) =
  fun projectee -> match projectee with | Xhr_dst _0 -> true | uu___ -> false
let (__proj__Xhr_dst__item___0 : dst -> (page_ref * value)) =
  fun projectee -> match projectee with | Xhr_dst _0 -> _0
let (uu___is_Doc_dst : dst -> Prims.bool) =
  fun projectee -> match projectee with | Doc_dst _0 -> true | uu___ -> false
let (__proj__Doc_dst__item___0 : dst -> win_ref) =
  fun projectee -> match projectee with | Doc_dst _0 -> _0
let (uu___is_Script_dst : dst -> Prims.bool) =
  fun projectee ->
    match projectee with | Script_dst _0 -> true | uu___ -> false
let (__proj__Script_dst__item___0 : dst -> (page_ref * node_ref)) =
  fun projectee -> match projectee with | Script_dst _0 -> _0
type task = {
  task_win: win_ref ;
  task_expr: inner BrowserIO.expr }
let (__proj__Mktask__item__task_win : task -> win_ref) =
  fun projectee ->
    match projectee with | { task_win; task_expr;_} -> task_win
let (__proj__Mktask__item__task_expr : task -> inner BrowserIO.expr) =
  fun projectee ->
    match projectee with | { task_win; task_expr;_} -> task_expr
let (b_act_ref_pred : act_ref -> act -> Prims.bool) =
  fun r ->
    fun a ->
      match a.act_parent with
      | FStar_Pervasives_Native.None -> true
      | FStar_Pervasives_Native.Some ap -> r > ap
let rec (b_env_pred : (act_ref * act) Prims.list -> Prims.bool) =
  fun l ->
    match l with
    | [] -> true
    | (r, a)::tl -> (b_act_ref_pred r a) && (b_env_pred tl)
type b_env = (act_ref * act) Prims.list
let rec (node_list_check : node_ref Prims.list -> node_ref -> Prims.bool) =
  fun l ->
    fun n ->
      match l with | [] -> true | r::tl -> (r > n) && (node_list_check tl n)
let (b_node_ref_pred : node_ref -> node -> Prims.bool) =
  fun r ->
    fun n ->
      match n with
      | Div_node (uu___, n1) -> node_list_check n1 r
      | uu___ -> true
let rec (b_node_pred : (node_ref * node) Prims.list -> Prims.bool) =
  fun l ->
    match l with
    | [] -> true
    | (r, n)::t1 -> (b_node_ref_pred r n) && (b_node_pred t1)
type b_nodes = (node_ref * node) Prims.list
type b_conn = (BrowserIO.domain * BrowserIO.req_uri * dst) Prims.list
type browser =
  {
  browser_windows: (win_ref * win) Prims.list ;
  browser_pages: (page_ref * page) Prims.list ;
  browser_nodes: b_nodes ;
  browser_environments: b_env ;
  browser_cookies: (cookie_id * Prims.string) Prims.list ;
  browser_connections: b_conn }
let (__proj__Mkbrowser__item__browser_windows :
  browser -> (win_ref * win) Prims.list) =
  fun projectee ->
    match projectee with
    | { browser_windows; browser_pages; browser_nodes; browser_environments;
        browser_cookies; browser_connections;_} -> browser_windows
let (__proj__Mkbrowser__item__browser_pages :
  browser -> (page_ref * page) Prims.list) =
  fun projectee ->
    match projectee with
    | { browser_windows; browser_pages; browser_nodes; browser_environments;
        browser_cookies; browser_connections;_} -> browser_pages
let (__proj__Mkbrowser__item__browser_nodes : browser -> b_nodes) =
  fun projectee ->
    match projectee with
    | { browser_windows; browser_pages; browser_nodes; browser_environments;
        browser_cookies; browser_connections;_} -> browser_nodes
let (__proj__Mkbrowser__item__browser_environments : browser -> b_env) =
  fun projectee ->
    match projectee with
    | { browser_windows; browser_pages; browser_nodes; browser_environments;
        browser_cookies; browser_connections;_} -> browser_environments
let (__proj__Mkbrowser__item__browser_cookies :
  browser -> (cookie_id * Prims.string) Prims.list) =
  fun projectee ->
    match projectee with
    | { browser_windows; browser_pages; browser_nodes; browser_environments;
        browser_cookies; browser_connections;_} -> browser_cookies
let (__proj__Mkbrowser__item__browser_connections : browser -> b_conn) =
  fun projectee ->
    match projectee with
    | { browser_windows; browser_pages; browser_nodes; browser_environments;
        browser_cookies; browser_connections;_} -> browser_connections
let rec upd_assoc :
  'key 'data .
    'key -> 'data -> ('key * 'data) Prims.list -> ('key * 'data) Prims.list
  =
  fun k ->
    fun d ->
      fun map ->
        match map with
        | [] -> [(k, d)]
        | (k', d')::map1 ->
            if k' = k
            then (k', d) :: map1
            else (k', d') :: (upd_assoc k d map1)
let rec find_pos :
  'key 'data . 'key -> ('key * 'data) Prims.list -> Prims.int =
  fun k ->
    fun map ->
      match map with
      | [] -> Prims.int_zero
      | (k', uu___)::map1 ->
          if k' = k
          then Prims.int_zero
          else Prims.int_one + (find_pos k map1)
let rec remove_assoc :
  'key 'data . 'key -> ('key * 'data) Prims.list -> ('key * 'data) Prims.list
  =
  fun k ->
    fun map ->
      match map with
      | [] -> []
      | (k', d')::map1 ->
          if k' = k then map1 else (k', d') :: (remove_assoc k map1)
let (page_valid : page_ref -> browser -> Prims.bool) =
  fun pr ->
    fun b ->
      match FStar_List_Tot_Base.assoc pr b.browser_pages with
      | FStar_Pervasives_Native.None -> false
      | uu___ -> true
let (page_assoc : page_ref -> browser -> page FStar_Pervasives_Native.option)
  = fun pr -> fun b -> FStar_List_Tot_Base.assoc pr b.browser_pages
let (page_assoc_valid : page_ref -> browser -> page) =
  fun pr ->
    fun b ->
      FStar_Pervasives_Native.__proj__Some__item__v
        (FStar_List_Tot_Base.assoc pr b.browser_pages)
let (page_update : page_ref -> page -> browser -> browser) =
  fun pr ->
    fun p ->
      fun b ->
        let pages' = upd_assoc pr p b.browser_pages in
        {
          browser_windows = (b.browser_windows);
          browser_pages = pages';
          browser_nodes = (b.browser_nodes);
          browser_environments = (b.browser_environments);
          browser_cookies = (b.browser_cookies);
          browser_connections = (b.browser_connections)
        }
let (page_new : page -> browser -> (page_ref * browser)) =
  fun p ->
    fun b ->
      let p_ref =
        if (FStar_List_Tot_Base.length b.browser_pages) >= Prims.int_one
        then
          FStar_Pervasives_Native.fst
            (FStar_List_Tot_Base.last b.browser_pages)
        else Prims.int_zero in
      let pr = fresh_page_ref p_ref in
      let pages' = upd_assoc pr p b.browser_pages in
      let b' =
        {
          browser_windows = (b.browser_windows);
          browser_pages = pages';
          browser_nodes = (b.browser_nodes);
          browser_environments = (b.browser_environments);
          browser_cookies = (b.browser_cookies);
          browser_connections = (b.browser_connections)
        } in
      (pr, b')
let (page_remove : page_ref -> browser -> browser) =
  fun pr ->
    fun b ->
      let pages' = remove_assoc pr b.browser_pages in
      {
        browser_windows = (b.browser_windows);
        browser_pages = pages';
        browser_nodes = (b.browser_nodes);
        browser_environments = (b.browser_environments);
        browser_cookies = (b.browser_cookies);
        browser_connections = (b.browser_connections)
      }
let (page_win :
  page_ref -> browser -> win_ref FStar_Pervasives_Native.option) =
  fun pr ->
    fun b ->
      let has_pr uu___ = match uu___ with | (uu___1, w) -> w.win_page = pr in
      match FStar_List_Tot_Base.filter has_pr b.browser_windows with
      | (wr, uu___)::[] -> FStar_Pervasives_Native.Some wr
      | uu___ -> FStar_Pervasives_Native.None
let (win_valid : win_ref -> browser -> Prims.bool) =
  fun wr ->
    fun b ->
      match FStar_List_Tot_Base.assoc wr b.browser_windows with
      | FStar_Pervasives_Native.None -> false
      | uu___ -> true
let (win_assoc : win_ref -> browser -> win FStar_Pervasives_Native.option) =
  fun wr -> fun b -> FStar_List_Tot_Base.assoc wr b.browser_windows
let (win_assoc_valid : win_ref -> browser -> win) =
  fun wr ->
    fun b ->
      FStar_Pervasives_Native.__proj__Some__item__v
        (FStar_List_Tot_Base.assoc wr b.browser_windows)
let (win_update : win_ref -> win -> browser -> browser) =
  fun wr ->
    fun w ->
      fun b ->
        let windows' = upd_assoc wr w b.browser_windows in
        {
          browser_windows = windows';
          browser_pages = (b.browser_pages);
          browser_nodes = (b.browser_nodes);
          browser_environments = (b.browser_environments);
          browser_cookies = (b.browser_cookies);
          browser_connections = (b.browser_connections)
        }
let (win_new : win -> browser -> (win_ref * browser)) =
  fun w ->
    fun b ->
      let w_ref =
        if (FStar_List_Tot_Base.length b.browser_windows) >= Prims.int_one
        then
          FStar_Pervasives_Native.fst
            (FStar_List_Tot_Base.last b.browser_windows)
        else Prims.int_zero in
      let wr = fresh_win_ref w_ref in let b' = win_update wr w b in (wr, b')
let (win_remove : win_ref -> browser -> browser) =
  fun wr ->
    fun b ->
      match win_assoc wr b with
      | FStar_Pervasives_Native.None -> b
      | FStar_Pervasives_Native.Some w ->
          let b' = page_remove w.win_page b in
          let windows' = remove_assoc wr b'.browser_windows in
          {
            browser_windows = windows';
            browser_pages = (b'.browser_pages);
            browser_nodes = (b'.browser_nodes);
            browser_environments = (b'.browser_environments);
            browser_cookies = (b'.browser_cookies);
            browser_connections = (b'.browser_connections)
          }
let (win_from_win_name :
  Prims.string -> browser -> win_ref FStar_Pervasives_Native.option) =
  fun str ->
    fun b ->
      let has_name uu___ =
        match uu___ with | (uu___1, w) -> w.win_name = (Str_name str) in
      match FStar_List_Tot_Base.filter has_name b.browser_windows with
      | [] -> FStar_Pervasives_Native.None
      | (wr, uu___)::uu___1 -> FStar_Pervasives_Native.Some wr
let rec (user_window_sub :
  BrowserIO.url ->
    (page_ref * page) Prims.list ->
      (win_ref * win) Prims.list -> (win_ref * win) Prims.list)
  =
  fun u ->
    fun bp ->
      fun l ->
        match l with
        | [] -> []
        | (wr, w)::tl ->
            (match FStar_List_Tot_Base.assoc w.win_page bp with
             | FStar_Pervasives_Native.None -> user_window_sub u bp tl
             | FStar_Pervasives_Native.Some p ->
                 if p.page_location = u
                 then (wr, w) :: (user_window_sub u bp tl)
                 else user_window_sub u bp tl)
let (win_from_user_window :
  BrowserIO.user_window -> browser -> win_ref FStar_Pervasives_Native.option)
  =
  fun uu___ ->
    fun b ->
      match uu___ with
      | (u, n) ->
          let windows' = user_window_sub u b.browser_pages b.browser_windows in
          if
            ((FStar_List_Tot_Base.length windows') <= n) ||
              (n = Prims.int_zero)
          then FStar_Pervasives_Native.None
          else
            FStar_Pervasives_Native.Some
              (FStar_Pervasives_Native.fst
                 (FStar_List_Tot_Base.index windows' (n - Prims.int_one)))
let rec (find_win_pos :
  win_ref -> (win_ref * win) Prims.list -> Prims.nat -> Prims.nat) =
  fun wr ->
    fun ws ->
      fun n ->
        match ws with
        | (wr', uu___)::ws' ->
            if wr' = wr then n else find_win_pos wr ws' (n + Prims.int_one)
        | [] -> Prims.int_zero
let (win_to_user_window : win_ref -> browser -> BrowserIO.user_window) =
  fun wr ->
    fun b ->
      let w = win_assoc_valid wr b in
      let u = (page_assoc_valid w.win_page b).page_location in
      let windows' = user_window_sub u b.browser_pages b.browser_windows in
      (u, (find_win_pos wr windows' Prims.int_zero))
let (node_valid : node_ref -> browser -> Prims.bool) =
  fun dr ->
    fun b ->
      match FStar_List_Tot_Base.assoc dr b.browser_nodes with
      | FStar_Pervasives_Native.None -> false
      | uu___ -> true

let (node_assoc_valid : node_ref -> browser -> node) =
  fun dr ->
    fun b ->
      match FStar_List_Tot_Base.assoc dr b.browser_nodes with
      | FStar_Pervasives_Native.Some v -> v

let (node_update : node_ref -> node -> browser -> browser) =
  fun dr ->
    fun dn ->
      fun b ->
        let nodes' = upd_assoc dr dn b.browser_nodes in
        {
          browser_windows = (b.browser_windows);
          browser_pages = (b.browser_pages);
          browser_nodes = nodes';
          browser_environments = (b.browser_environments);
          browser_cookies = (b.browser_cookies);
          browser_connections = (b.browser_connections)
        }
let (node_new : node -> browser -> (node_ref * browser)) =
  fun dn ->
    fun b ->
      let n_ref =
        if (FStar_List_Tot_Base.length b.browser_nodes) >= Prims.int_one
        then
          FStar_Pervasives_Native.fst
            (FStar_List_Tot_Base.last b.browser_nodes)
        else Prims.int_zero in
      let nr = fresh_node_ref n_ref in
      let b' = node_update nr dn b in (nr, b')
type node_parent_type =
  | No_parent 
  | Page_parent of page_ref 
  | Parent_node of node_ref 
let (uu___is_No_parent : node_parent_type -> Prims.bool) =
  fun projectee -> match projectee with | No_parent -> true | uu___ -> false
let (uu___is_Page_parent : node_parent_type -> Prims.bool) =
  fun projectee ->
    match projectee with | Page_parent _0 -> true | uu___ -> false
let (__proj__Page_parent__item___0 : node_parent_type -> page_ref) =
  fun projectee -> match projectee with | Page_parent _0 -> _0
let (uu___is_Parent_node : node_parent_type -> Prims.bool) =
  fun projectee ->
    match projectee with | Parent_node _0 -> true | uu___ -> false
let (__proj__Parent_node__item___0 : node_parent_type -> node_ref) =
  fun projectee -> match projectee with | Parent_node _0 -> _0
let rec (node_parent_bpages :
  node_ref ->
    (page_ref * page) Prims.list -> page_ref FStar_Pervasives_Native.option)
  =
  fun dr ->
    fun l ->
      match l with
      | [] -> FStar_Pervasives_Native.None
      | (pr, p)::tl ->
          (match p.page_document with
           | FStar_Pervasives_Native.Some dr1 ->
               if dr1 = dr
               then FStar_Pervasives_Native.Some pr
               else node_parent_bpages dr tl
           | uu___ -> node_parent_bpages dr tl)

let rec (node_parent_dnode :
  node_ref -> b_nodes -> node_ref FStar_Pervasives_Native.option) =
  fun dr ->
    fun l ->
      match l with
      | [] -> FStar_Pervasives_Native.None
      | (nr, n)::tl ->
          (match n with
           | Div_node (uu___, children) ->
               if FStar_List_Tot_Base.mem dr children
               then FStar_Pervasives_Native.Some nr
               else node_parent_dnode dr tl
           | uu___ -> node_parent_dnode dr tl)
let (node_parent : node_ref -> browser -> node_parent_type) =
  fun dr ->
    fun b ->
      match node_parent_bpages dr b.browser_pages with
      | FStar_Pervasives_Native.Some pr -> Page_parent pr
      | FStar_Pervasives_Native.None ->
          (match node_parent_dnode dr b.browser_nodes with
           | FStar_Pervasives_Native.Some nr -> Parent_node nr
           | FStar_Pervasives_Native.None -> No_parent)
let rec (node_page :
  node_ref -> browser -> page_ref FStar_Pervasives_Native.option) =
  fun dr ->
    fun b ->
      match node_parent dr b with
      | No_parent -> FStar_Pervasives_Native.None
      | Page_parent pr -> FStar_Pervasives_Native.Some pr
      | Parent_node dr1 -> node_page dr1 b
let (act_valid : act_ref -> browser -> Prims.bool) =
  fun ar ->
    fun b ->
      match FStar_List_Tot_Base.assoc ar b.browser_environments with
      | FStar_Pervasives_Native.None -> false
      | uu___ -> true

let (act_assoc_valid : act_ref -> browser -> act) =
  fun ar ->
    fun b ->
      match FStar_List_Tot_Base.assoc ar b.browser_environments with
      | FStar_Pervasives_Native.Some v -> v

let (act_update : act_ref -> act -> browser -> browser) =
  fun ar ->
    fun act1 ->
      fun b ->
        let environments' = upd_assoc ar act1 b.browser_environments in
        {
          browser_windows = (b.browser_windows);
          browser_pages = (b.browser_pages);
          browser_nodes = (b.browser_nodes);
          browser_environments = environments';
          browser_cookies = (b.browser_cookies);
          browser_connections = (b.browser_connections)
        }
let (act_new : act -> browser -> (act_ref * browser)) =
  fun act1 ->
    fun b ->
      let a_ref =
        if
          (FStar_List_Tot_Base.length b.browser_environments) >=
            Prims.int_one
        then
          FStar_Pervasives_Native.fst
            (FStar_List_Tot_Base.last b.browser_environments)
        else Prims.int_zero in
      let ar = fresh_page_ref a_ref in
      let b' = act_update ar act1 b in (ar, b')
let rec (get_var :
  BrowserIO.var -> act_ref -> browser -> value FStar_Pervasives_Native.option)
  =
  fun x ->
    fun ar ->
      fun b ->
        if act_valid ar b
        then
          let act1 = act_assoc_valid ar b in
          match FStar_List_Tot_Base.assoc x act1.act_vars with
          | FStar_Pervasives_Native.None ->
              (match act1.act_parent with
               | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
               | FStar_Pervasives_Native.Some ar1 -> get_var x ar1 b)
          | uu___ -> FStar_List_Tot_Base.assoc x act1.act_vars
        else FStar_Pervasives_Native.None
let (create_var :
  BrowserIO.var ->
    value -> act_ref -> browser -> browser FStar_Pervasives_Native.option)
  =
  fun x ->
    fun r ->
      fun ar ->
        fun b ->
          if act_valid ar b
          then
            let act1 = act_assoc_valid ar b in
            let data' = upd_assoc x r act1.act_vars in
            let act' = { act_parent = (act1.act_parent); act_vars = data' } in
            FStar_Pervasives_Native.Some (act_update ar act' b)
          else FStar_Pervasives_Native.None
let rec (set_var :
  BrowserIO.var ->
    value -> act_ref -> browser -> browser FStar_Pervasives_Native.option)
  =
  fun x ->
    fun r ->
      fun ar ->
        fun b ->
          if act_valid ar b
          then
            let act1 = act_assoc_valid ar b in
            match FStar_List_Tot_Base.assoc x act1.act_vars with
            | FStar_Pervasives_Native.None ->
                (match act1.act_parent with
                 | FStar_Pervasives_Native.None -> create_var x r ar b
                 | FStar_Pervasives_Native.Some ar1 -> set_var x r ar1 b)
            | uu___ ->
                let data' = upd_assoc x r act1.act_vars in
                let act' =
                  { act_parent = (act1.act_parent); act_vars = data' } in
                FStar_Pervasives_Native.Some (act_update ar act' b)
          else FStar_Pervasives_Native.None
let rec prefix : 'a . 'a Prims.list -> 'a Prims.list -> Prims.bool =
  fun xs ->
    fun ys ->
      match (xs, ys) with
      | ([], uu___) -> true
      | (x::xs', y::ys') -> (x = y) && (prefix xs' ys')
      | (uu___, uu___1) -> false
let (get_site_cookies :
  BrowserIO.domain ->
    BrowserIO.path -> browser -> (Prims.string * Prims.string) Prims.list)
  =
  fun d ->
    fun p ->
      fun b ->
        let check uu___ =
          match uu___ with
          | (cid, uu___1) ->
              ((cid.cookie_id_domain = d) && (prefix cid.cookie_id_path p))
                ||
                ((prefix cid.cookie_id_domain d) && (cid.cookie_id_path = [])) in
        let strip uu___ =
          match uu___ with | (cid, rslt) -> ((cid.cookie_id_key), rslt) in
        FStar_List_Tot_Base.map strip
          (FStar_List_Tot_Base.filter check b.browser_cookies)
let (del_site_cookie :
  BrowserIO.domain -> BrowserIO.path -> Prims.string -> browser -> browser) =
  fun d ->
    fun p ->
      fun k ->
        fun b ->
          let cid =
            { cookie_id_domain = d; cookie_id_path = p; cookie_id_key = k } in
          {
            browser_windows = (b.browser_windows);
            browser_pages = (b.browser_pages);
            browser_nodes = (b.browser_nodes);
            browser_environments = (b.browser_environments);
            browser_cookies = (remove_assoc cid b.browser_cookies);
            browser_connections = (b.browser_connections)
          }
let rec (del_site_cookies :
  BrowserIO.domain ->
    BrowserIO.path -> Prims.string Prims.list -> browser -> browser)
  =
  fun d ->
    fun p ->
      fun ks ->
        fun b ->
          match ks with
          | [] -> b
          | k::ks1 ->
              let b' = del_site_cookie d p k b in del_site_cookies d p ks1 b'
let (set_site_cookie :
  BrowserIO.domain ->
    BrowserIO.path -> (Prims.string * Prims.string) -> browser -> browser)
  =
  fun d ->
    fun p ->
      fun uu___ ->
        fun b ->
          match uu___ with
          | (k, v) ->
              let cid =
                { cookie_id_domain = d; cookie_id_path = p; cookie_id_key = k
                } in
              {
                browser_windows = (b.browser_windows);
                browser_pages = (b.browser_pages);
                browser_nodes = (b.browser_nodes);
                browser_environments = (b.browser_environments);
                browser_cookies = (upd_assoc cid v b.browser_cookies);
                browser_connections = (b.browser_connections)
              }
let rec (set_site_cookies :
  BrowserIO.domain ->
    BrowserIO.path ->
      (Prims.string * Prims.string) Prims.list -> browser -> browser)
  =
  fun d ->
    fun p ->
      fun pairs ->
        fun b ->
          match pairs with
          | [] -> b
          | pair::pairs' ->
              let b' = set_site_cookie d p pair b in
              set_site_cookies d p pairs' b'
let (upd_cookies :
  BrowserIO.domain ->
    BrowserIO.req_uri -> BrowserIO.resp -> browser -> browser)
  =
  fun d ->
    fun uri ->
      fun resp ->
        fun b ->
          let b' =
            del_site_cookies d uri.BrowserIO.req_uri_path
              resp.BrowserIO.resp_del_cookies b in
          let b'' =
            set_site_cookies d uri.BrowserIO.req_uri_path
              resp.BrowserIO.resp_set_cookies b' in
          b''
let (net_connection_domain_nth :
  BrowserIO.domain ->
    Prims.nat ->
      browser -> (BrowserIO.req_uri * dst) FStar_Pervasives_Native.option)
  =
  fun d ->
    fun n ->
      fun b ->
        let test uu___ = match uu___ with | (d', uu___1, uu___2) -> d' = d in
        let connections' =
          FStar_List_Tot_Base.filter test b.browser_connections in
        if (FStar_List_Tot_Base.length connections') <= n
        then FStar_Pervasives_Native.None
        else
          (let uu___1 = FStar_List_Tot_Base.index connections' n in
           match uu___1 with
           | (uu___2, uri, dst1) -> FStar_Pervasives_Native.Some (uri, dst1))
let rec (remove : BrowserIO.domain -> Prims.nat -> b_conn -> b_conn) =
  fun d ->
    fun n ->
      fun cs ->
        match cs with
        | [] -> []
        | (d', x, y)::cs' ->
            if (n = Prims.int_zero) && (d' = d)
            then cs'
            else
              if d' = d
              then (d', x, y) :: (remove d (n - Prims.int_one) cs')
              else (d', x, y) :: (remove d n cs')
let (net_connection_domain_remove_nth :
  BrowserIO.domain -> Prims.nat -> browser -> browser) =
  fun d ->
    fun n ->
      fun b ->
        let connections' = remove d n b.browser_connections in
        {
          browser_windows = (b.browser_windows);
          browser_pages = (b.browser_pages);
          browser_nodes = (b.browser_nodes);
          browser_environments = (b.browser_environments);
          browser_cookies = (b.browser_cookies);
          browser_connections = connections'
        }
let (http_send :
  BrowserIO.domain ->
    BrowserIO.req_uri ->
      Prims.string -> dst -> browser -> (browser * BrowserIO.output_event))
  =
  fun d ->
    fun uri ->
      fun body ->
        fun dst1 ->
          fun b ->
            let connections' = (d, uri, dst1) :: (b.browser_connections) in
            let b' =
              {
                browser_windows = (b.browser_windows);
                browser_pages = (b.browser_pages);
                browser_nodes = (b.browser_nodes);
                browser_environments = (b.browser_environments);
                browser_cookies = (b.browser_cookies);
                browser_connections = connections'
              } in
            let req =
              {
                BrowserIO.req_uri = uri;
                BrowserIO.req_cookies =
                  (get_site_cookies d uri.BrowserIO.req_uri_path b);
                BrowserIO.req_body = body
              } in
            (b', (BrowserIO.Network_send_event (d, req)))
let rec (render_doc_as_list :
  node_ref -> browser -> BrowserIO.rendered_doc Prims.list) =
  fun dr ->
    fun b ->
      if node_valid dr b
      then
        match node_assoc_valid dr b with
        | Para_node (uu___, txt) -> [BrowserIO.Para_rendered txt]
        | Link_node (uu___, u, txt) -> [BrowserIO.Link_rendered (u, txt)]
        | Textbox_node (uu___, txt, uu___1) ->
            [BrowserIO.Textbox_rendered txt]
        | Button_node (uu___, txt, uu___1) -> [BrowserIO.Button_rendered txt]
        | Inl_script_node (uu___, uu___1, uu___2) -> []
        | Rem_script_node (uu___, uu___1, uu___2) -> []
        | Div_node (uu___, drs) -> [BrowserIO.Div_rendered []]
      else []
let (render_page :
  page_ref ->
    browser -> BrowserIO.rendered_doc FStar_Pervasives_Native.option)
  =
  fun pr ->
    fun b ->
      if page_valid pr b
      then
        match (page_assoc_valid pr b).page_document with
        | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
        | FStar_Pervasives_Native.Some dr ->
            (match render_doc_as_list dr b with
             | [] -> FStar_Pervasives_Native.None
             | rd::uu___ -> FStar_Pervasives_Native.Some rd)
      else FStar_Pervasives_Native.None
let (page_update_event : page_ref -> browser -> BrowserIO.output_event) =
  fun pr ->
    fun b ->
      match page_win pr b with
      | FStar_Pervasives_Native.None ->
          BrowserIO.UI_error "page reference not found in browser"
      | FStar_Pervasives_Native.Some wr ->
          if
            (win_valid wr b) &&
              (page_valid (win_assoc_valid wr b).win_page b)
          then
            let uw = win_to_user_window wr b in
            let rd_opt = render_page pr b in
            BrowserIO.UI_page_updated_event (uw, rd_opt)
          else BrowserIO.UI_error "invalid win_ref or page_ref"
let (build_win :
  win_name ->
    BrowserIO.url ->
      win_opener ->
        node_ref FStar_Pervasives_Native.option -> browser -> (win * browser))
  =
  fun wn ->
    fun u ->
      fun wo ->
        fun doc ->
          fun b ->
            let a =
              { act_parent = FStar_Pervasives_Native.None; act_vars = [] } in
            let uu___ = act_new a b in
            match uu___ with
            | (ar, b') ->
                let p =
                  {
                    page_location = u;
                    page_document = doc;
                    page_environment = ar;
                    page_script_queue = []
                  } in
                let uu___1 = page_new p b' in
                (match uu___1 with
                 | (pr, b'') ->
                     let w =
                       { win_name = wn; win_opener = wo; win_page = pr } in
                     (w, b''))
let (fetch_url :
  BrowserIO.url ->
    win_ref -> browser -> (browser * BrowserIO.output_event Prims.list))
  =
  fun u ->
    fun wr ->
      fun b ->
        if win_valid wr b
        then
          match u with
          | BrowserIO.Blank_url -> (b, [])
          | BrowserIO.Http_url (d, uri) ->
              let dst1 = Doc_dst wr in
              let uu___ = http_send d uri "" dst1 b in
              (match uu___ with | (b', oe) -> (b', [oe]))
        else (b, [BrowserIO.UI_error "invalid win_ref"])
let (open_win :
  win_name ->
    BrowserIO.url ->
      win_opener ->
        browser -> (win_ref * browser * BrowserIO.output_event Prims.list))
  =
  fun wn ->
    fun u ->
      fun wo ->
        fun b ->
          let uu___ =
            build_win wn BrowserIO.Blank_url wo FStar_Pervasives_Native.None
              b in
          match uu___ with
          | (w', b') ->
              let uu___1 = win_new w' b' in
              (match uu___1 with
               | (wr, b'') ->
                   let uu___2 = fetch_url u wr b'' in
                   (match uu___2 with
                    | (b''', oes) ->
                        (wr, b''', (BrowserIO.UI_win_opened_event :: oes))))
let (close_doc_request_connections : win_ref -> browser -> browser) =
  fun wr ->
    fun b ->
      if win_valid wr b
      then
        let not_for_wr uu___ =
          match uu___ with
          | (uu___1, uu___2, dst1) ->
              (match dst1 with | Doc_dst wr' -> wr' <> wr | uu___3 -> true) in
        let connections' =
          FStar_List_Tot_Base.filter not_for_wr b.browser_connections in
        {
          browser_windows = (b.browser_windows);
          browser_pages = (b.browser_pages);
          browser_nodes = (b.browser_nodes);
          browser_environments = (b.browser_environments);
          browser_cookies = (b.browser_cookies);
          browser_connections = connections'
        }
      else b
let (direct_win :
  win_ref ->
    BrowserIO.url -> browser -> (browser * BrowserIO.output_event Prims.list))
  =
  fun wr ->
    fun u ->
      fun b ->
        if win_valid wr b
        then
          let b' = close_doc_request_connections wr b in
          match u with
          | BrowserIO.Blank_url ->
              (if
                 (win_valid wr b) &&
                   (page_valid (win_assoc_valid wr b).win_page b)
               then
                 let uw = win_to_user_window wr b' in
                 let w = win_assoc_valid wr b' in
                 let b'1 = page_remove w.win_page b' in
                 let uu___ =
                   build_win w.win_name BrowserIO.Blank_url w.win_opener
                     FStar_Pervasives_Native.None b'1 in
                 match uu___ with
                 | (w', b'2) ->
                     let b'3 = win_update wr w' b'2 in
                     let oe =
                       BrowserIO.UI_page_loaded_event
                         (uw, BrowserIO.Blank_url,
                           FStar_Pervasives_Native.None) in
                     (b'3, [oe])
               else (b, [BrowserIO.UI_error "invalid win_ref or page_ref"]))
          | uu___ -> fetch_url u wr b
        else (b, [BrowserIO.UI_error "invalid win_ref"])
let rec (build_node_tree :
  BrowserIO.doc -> browser -> (node_ref * (node_ref * node) Prims.list)) =
  fun doc ->
    fun b ->
      let n_ref =
        if (FStar_List_Tot_Base.length b.browser_nodes) >= Prims.int_one
        then
          FStar_Pervasives_Native.fst
            (FStar_List_Tot_Base.last b.browser_nodes)
        else Prims.int_zero in
      let dr = fresh_node_ref n_ref in
      match doc with
      | BrowserIO.Para (id, text) -> (dr, [(dr, (Para_node (id, text)))])
      | BrowserIO.Link (id, u, text) ->
          (dr, [(dr, (Link_node (id, u, text)))])
      | BrowserIO.Textbox (id, text) ->
          (dr, [(dr, (Textbox_node (id, text, [])))])
      | BrowserIO.Button (id, text) ->
          (dr, [(dr, (Button_node (id, text, [])))])
      | BrowserIO.Inl_script (id, e) ->
          (dr, [(dr, (Inl_script_node (id, e, false)))])
      | BrowserIO.Rem_script (id, u) ->
          (dr, [(dr, (Rem_script_node (id, u, false)))])
      | BrowserIO.Divi (id, subdocs) -> (dr, [(dr, (Div_node (id, [])))])
let rec (take_ready :
  queued_expr Prims.list -> inner BrowserIO.expr Prims.list) =
  fun qes ->
    match qes with
    | (Known_expr e)::ps1 -> e :: (take_ready ps1)
    | uu___ -> []
let rec (drop_ready : queued_expr Prims.list -> queued_expr Prims.list) =
  fun qes ->
    match qes with | (Known_expr e)::ps1 -> drop_ready ps1 | uu___ -> qes
let (split_queued_exprs :
  queued_expr Prims.list ->
    (inner BrowserIO.expr Prims.list * queued_expr Prims.list))
  = fun qes -> ((take_ready qes), (drop_ready qes))
let (node_remove :
  node_ref -> browser -> (browser * BrowserIO.output_event Prims.list)) =
  fun dr ->
    fun b ->
      match node_parent dr b with
      | No_parent -> (b, [])
      | Page_parent pr' ->
          if
            FStar_Pervasives_Native.uu___is_Some
              (FStar_List_Tot_Base.assoc pr' b.browser_pages)
          then
            let p = page_assoc_valid pr' b in
            let p' =
              {
                page_location = (p.page_location);
                page_document = FStar_Pervasives_Native.None;
                page_environment = (p.page_environment);
                page_script_queue = (p.page_script_queue)
              } in
            let b' = page_update pr' p' b in (b', [page_update_event pr' b'])
          else (b, [BrowserIO.UI_error "invalid page_ref"])
      | Parent_node p ->
          if node_valid p b
          then
            (match node_assoc_valid p b with
             | Div_node (id, children) ->
                 let children' =
                   FStar_List_Tot_Base.filter (fun dr' -> dr' <> dr) children in
                 if b_node_ref_pred p (Div_node (id, children'))
                 then
                   let b' = node_update p (Div_node (id, children')) b in
                   (match node_page dr b with
                    | FStar_Pervasives_Native.None -> (b', [])
                    | FStar_Pervasives_Native.Some pr' ->
                        (b', [page_update_event pr' b']))
                 else (b, [])
             | uu___ -> (b, [BrowserIO.UI_error "error"]))
          else (b, [BrowserIO.UI_error "invalid node_ref"])
let rec insert_in_list :
  'a . 'a -> 'a Prims.list -> Prims.int -> 'a Prims.list =
  fun x ->
    fun xs ->
      fun k ->
        match (xs, k) with
        | (xs1, uu___) when uu___ = Prims.int_zero -> x :: xs1
        | (x'::xs', k1) ->
            if k1 > Prims.int_zero
            then x' :: (insert_in_list x xs' (k1 - Prims.int_one))
            else []
        | (uu___, uu___1) -> []
let (rslt_to_elt_it_opt :
  value -> BrowserIO.elt_id FStar_Pervasives_Native.option) =
  fun r ->
    match r with
    | String_value id -> FStar_Pervasives_Native.Some id
    | uu___ -> FStar_Pervasives_Native.None