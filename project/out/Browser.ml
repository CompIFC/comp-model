open Prims
type win_ref = Prims.int
let (fresh_win_ref : win_ref -> win_ref) = fun win -> win + Prims.int_one
type page_ref = Prims.int
let (fresh_page_ref : page_ref -> page_ref) =
  fun page -> page + Prims.int_one
type node_ref = Prims.int
let (fresh_node_ref : node_ref -> node_ref) =
  fun node -> node + Prims.int_one
type act_ref = Prims.int
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
  win_opener: win_ref ;
  win_page: page_ref }
let (__proj__Mkwin__item__win_name : win -> win_name) =
  fun projectee ->
    match projectee with
    | { win_name = win_name1; win_opener = win_opener1; win_page;_} ->
        win_name1
let (__proj__Mkwin__item__win_opener : win -> win_ref) =
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
  | Scoped_expr of context * inner BrowserIO.expr 
  | Value of value 
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
    match projectee with | Scoped_expr (_0, _1) -> true | uu___ -> false
let (__proj__Scoped_expr__item___0 : inner -> context) =
  fun projectee -> match projectee with | Scoped_expr (_0, _1) -> _0
let (__proj__Scoped_expr__item___1 : inner -> inner BrowserIO.expr) =
  fun projectee -> match projectee with | Scoped_expr (_0, _1) -> _1
let (uu___is_Value : inner -> Prims.bool) =
  fun projectee -> match projectee with | Value _0 -> true | uu___ -> false
let (__proj__Value__item___0 : inner -> value) =
  fun projectee -> match projectee with | Value _0 -> _0
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
  | Para_node of BrowserIO.elt_id FStar_Pervasives_Native.option *
  Prims.string 
  | Link_node of BrowserIO.elt_id FStar_Pervasives_Native.option *
  BrowserIO.url * Prims.string 
  | Text_node of BrowserIO.elt_id FStar_Pervasives_Native.option *
  Prims.string * value Prims.list 
  | Button_node of BrowserIO.elt_id FStar_Pervasives_Native.option *
  Prims.string * value Prims.list 
  | Remote_script_node of BrowserIO.elt_id FStar_Pervasives_Native.option *
  BrowserIO.url * Prims.bool 
  | Div_node of BrowserIO.elt_id * node_ref Prims.list 
  | Inline_script_node of BrowserIO.elt_id FStar_Pervasives_Native.option *
  unit BrowserIO.expr * Prims.bool 
let (uu___is_Para_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Para_node (_0, _1) -> true | uu___ -> false
let (__proj__Para_node__item___0 :
  node -> BrowserIO.elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Para_node (_0, _1) -> _0
let (__proj__Para_node__item___1 : node -> Prims.string) =
  fun projectee -> match projectee with | Para_node (_0, _1) -> _1
let (uu___is_Link_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Link_node (_0, _1, _2) -> true | uu___ -> false
let (__proj__Link_node__item___0 :
  node -> BrowserIO.elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Link_node (_0, _1, _2) -> _0
let (__proj__Link_node__item___1 : node -> BrowserIO.url) =
  fun projectee -> match projectee with | Link_node (_0, _1, _2) -> _1
let (__proj__Link_node__item___2 : node -> Prims.string) =
  fun projectee -> match projectee with | Link_node (_0, _1, _2) -> _2
let (uu___is_Text_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Text_node (_0, _1, _2) -> true | uu___ -> false
let (__proj__Text_node__item___0 :
  node -> BrowserIO.elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Text_node (_0, _1, _2) -> _0
let (__proj__Text_node__item___1 : node -> Prims.string) =
  fun projectee -> match projectee with | Text_node (_0, _1, _2) -> _1
let (__proj__Text_node__item___2 : node -> value Prims.list) =
  fun projectee -> match projectee with | Text_node (_0, _1, _2) -> _2
let (uu___is_Button_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Button_node (_0, _1, _2) -> true | uu___ -> false
let (__proj__Button_node__item___0 :
  node -> BrowserIO.elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Button_node (_0, _1, _2) -> _0
let (__proj__Button_node__item___1 : node -> Prims.string) =
  fun projectee -> match projectee with | Button_node (_0, _1, _2) -> _1
let (__proj__Button_node__item___2 : node -> value Prims.list) =
  fun projectee -> match projectee with | Button_node (_0, _1, _2) -> _2
let (uu___is_Remote_script_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with
    | Remote_script_node (_0, _1, _2) -> true
    | uu___ -> false
let (__proj__Remote_script_node__item___0 :
  node -> BrowserIO.elt_id FStar_Pervasives_Native.option) =
  fun projectee ->
    match projectee with | Remote_script_node (_0, _1, _2) -> _0
let (__proj__Remote_script_node__item___1 : node -> BrowserIO.url) =
  fun projectee ->
    match projectee with | Remote_script_node (_0, _1, _2) -> _1
let (__proj__Remote_script_node__item___2 : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Remote_script_node (_0, _1, _2) -> _2
let (uu___is_Div_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Div_node (_0, _1) -> true | uu___ -> false
let (__proj__Div_node__item___0 : node -> BrowserIO.elt_id) =
  fun projectee -> match projectee with | Div_node (_0, _1) -> _0
let (__proj__Div_node__item___1 : node -> node_ref Prims.list) =
  fun projectee -> match projectee with | Div_node (_0, _1) -> _1
let (uu___is_Inline_script_node : node -> Prims.bool) =
  fun projectee ->
    match projectee with
    | Inline_script_node (_0, _1, _2) -> true
    | uu___ -> false
let (__proj__Inline_script_node__item___0 :
  node -> BrowserIO.elt_id FStar_Pervasives_Native.option) =
  fun projectee ->
    match projectee with | Inline_script_node (_0, _1, _2) -> _0
let (__proj__Inline_script_node__item___1 :
  node -> Prims.l_False BrowserIO.expr) =
  fun projectee ->
    match projectee with | Inline_script_node (_0, _1, _2) -> _1
let (__proj__Inline_script_node__item___2 : node -> Prims.bool) =
  fun projectee ->
    match projectee with | Inline_script_node (_0, _1, _2) -> _2
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
  page_script: queued_expr Prims.list }
let (__proj__Mkpage__item__page_location : page -> BrowserIO.url) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script;_} ->
        page_location
let (__proj__Mkpage__item__page_document :
  page -> node_ref FStar_Pervasives_Native.option) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script;_} ->
        page_document
let (__proj__Mkpage__item__page_environment : page -> act_ref) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script;_} ->
        page_environment
let (__proj__Mkpage__item__page_script : page -> queued_expr Prims.list) =
  fun projectee ->
    match projectee with
    | { page_location; page_document; page_environment; page_script;_} ->
        page_script
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
type open_connection =
  | Doc_connection of BrowserIO.domain * BrowserIO.req_uri * win_ref 
  | Script_connection of BrowserIO.domain * BrowserIO.req_uri * page_ref *
  node_ref 
  | Xhr_connection of BrowserIO.domain * BrowserIO.req_uri * page_ref * value 
let (uu___is_Doc_connection : open_connection -> Prims.bool) =
  fun projectee ->
    match projectee with
    | Doc_connection (_0, _1, _2) -> true
    | uu___ -> false
let (__proj__Doc_connection__item___0 : open_connection -> BrowserIO.domain)
  = fun projectee -> match projectee with | Doc_connection (_0, _1, _2) -> _0
let (__proj__Doc_connection__item___1 : open_connection -> BrowserIO.req_uri)
  = fun projectee -> match projectee with | Doc_connection (_0, _1, _2) -> _1
let (__proj__Doc_connection__item___2 : open_connection -> win_ref) =
  fun projectee -> match projectee with | Doc_connection (_0, _1, _2) -> _2
let (uu___is_Script_connection : open_connection -> Prims.bool) =
  fun projectee ->
    match projectee with
    | Script_connection (_0, _1, _2, _3) -> true
    | uu___ -> false
let (__proj__Script_connection__item___0 :
  open_connection -> BrowserIO.domain) =
  fun projectee ->
    match projectee with | Script_connection (_0, _1, _2, _3) -> _0
let (__proj__Script_connection__item___1 :
  open_connection -> BrowserIO.req_uri) =
  fun projectee ->
    match projectee with | Script_connection (_0, _1, _2, _3) -> _1
let (__proj__Script_connection__item___2 : open_connection -> page_ref) =
  fun projectee ->
    match projectee with | Script_connection (_0, _1, _2, _3) -> _2
let (__proj__Script_connection__item___3 : open_connection -> node_ref) =
  fun projectee ->
    match projectee with | Script_connection (_0, _1, _2, _3) -> _3
let (uu___is_Xhr_connection : open_connection -> Prims.bool) =
  fun projectee ->
    match projectee with
    | Xhr_connection (_0, _1, _2, _3) -> true
    | uu___ -> false
let (__proj__Xhr_connection__item___0 : open_connection -> BrowserIO.domain)
  =
  fun projectee ->
    match projectee with | Xhr_connection (_0, _1, _2, _3) -> _0
let (__proj__Xhr_connection__item___1 : open_connection -> BrowserIO.req_uri)
  =
  fun projectee ->
    match projectee with | Xhr_connection (_0, _1, _2, _3) -> _1
let (__proj__Xhr_connection__item___2 : open_connection -> page_ref) =
  fun projectee ->
    match projectee with | Xhr_connection (_0, _1, _2, _3) -> _2
let (__proj__Xhr_connection__item___3 : open_connection -> value) =
  fun projectee ->
    match projectee with | Xhr_connection (_0, _1, _2, _3) -> _3
type task = {
  task_win: win_ref ;
  task_expr: inner BrowserIO.expr }
let (__proj__Mktask__item__task_win : task -> win_ref) =
  fun projectee ->
    match projectee with | { task_win; task_expr;_} -> task_win
let (__proj__Mktask__item__task_expr : task -> inner BrowserIO.expr) =
  fun projectee ->
    match projectee with | { task_win; task_expr;_} -> task_expr
type browser =
  {
  browser_windows: (win_ref * win) Prims.list ;
  browser_pages: (page_ref * page) Prims.list ;
  browser_nodes: (node_ref * node) Prims.list ;
  browser_environments: (act_ref * act) Prims.list ;
  browser_cookies: (cookie_id * Prims.string) Prims.list ;
  browser_connections: open_connection Prims.list }
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
let (__proj__Mkbrowser__item__browser_nodes :
  browser -> (node_ref * node) Prims.list) =
  fun projectee ->
    match projectee with
    | { browser_windows; browser_pages; browser_nodes; browser_environments;
        browser_cookies; browser_connections;_} -> browser_nodes
let (__proj__Mkbrowser__item__browser_environments :
  browser -> (act_ref * act) Prims.list) =
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
let (__proj__Mkbrowser__item__browser_connections :
  browser -> open_connection Prims.list) =
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
let (node_valid : node_ref -> browser -> Prims.bool) =
  fun dr ->
    fun b ->
      match FStar_List_Tot_Base.assoc dr b.browser_windows with
      | FStar_Pervasives_Native.None -> false
      | uu___ -> true
let (node_assoc_valid : node_ref -> browser -> node) =
  fun dr ->
    fun b ->
      FStar_Pervasives_Native.__proj__Some__item__v
        (FStar_List_Tot_Base.assoc dr b.browser_nodes)
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
      let nr = fresh_win_ref n_ref in
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
let (node_parent : node_ref -> browser -> node_parent_type) =
  fun dr ->
    fun b ->
      let is_page_parent uu___ =
        match uu___ with
        | (uu___1, p) ->
            (match p.page_document with
             | FStar_Pervasives_Native.Some dr1 ->
                 if dr1 = dr then true else false
             | uu___2 -> false) in
      let is_node_parent uu___ =
        match uu___ with
        | (uu___1, dn) ->
            (match dn with
             | Div_node (uu___2, children) ->
                 FStar_List_Tot_Base.mem dr children
             | uu___2 -> false) in
      match FStar_List_Tot_Base.filter is_page_parent b.browser_pages with
      | (pr, uu___)::uu___1 -> Page_parent pr
      | [] ->
          (match FStar_List_Tot_Base.filter is_node_parent b.browser_nodes
           with
           | (dr1, uu___)::uu___1 -> Parent_node dr1
           | [] -> No_parent)
let (act_valid : act_ref -> browser -> Prims.bool) =
  fun ar ->
    fun b ->
      match FStar_List_Tot_Base.assoc ar b.browser_windows with
      | FStar_Pervasives_Native.None -> false
      | uu___ -> true
let (act_assoc_valid : act_ref -> browser -> act) =
  fun ar ->
    fun b ->
      FStar_Pervasives_Native.__proj__Some__item__v
        (FStar_List_Tot_Base.assoc ar b.browser_environments)
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