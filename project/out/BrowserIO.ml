open Prims
type domain = Prims.string Prims.list
type path = Prims.string Prims.list
type req_uri = {
  req_uri_path: path ;
  req_uri_params: Prims.string }
let (__proj__Mkreq_uri__item__req_uri_path : req_uri -> path) =
  fun projectee ->
    match projectee with | { req_uri_path; req_uri_params;_} -> req_uri_path
let (__proj__Mkreq_uri__item__req_uri_params : req_uri -> Prims.string) =
  fun projectee ->
    match projectee with
    | { req_uri_path; req_uri_params;_} -> req_uri_params
type url =
  | Blank_url 
  | Http_url of domain * req_uri 
let (uu___is_Blank_url : url -> Prims.bool) =
  fun projectee -> match projectee with | Blank_url -> true | uu___ -> false
let (uu___is_Http_url : url -> Prims.bool) =
  fun projectee ->
    match projectee with | Http_url (_0, _1) -> true | uu___ -> false
let (__proj__Http_url__item___0 : url -> domain) =
  fun projectee -> match projectee with | Http_url (_0, _1) -> _0
let (__proj__Http_url__item___1 : url -> req_uri) =
  fun projectee -> match projectee with | Http_url (_0, _1) -> _1
type typ =
  | Null_type 
  | Bool_type 
  | Int_type 
  | String_type 
  | Url_type 
  | Type_type 
  | Function_type 
  | Code_type 
  | Window_type 
  | Node_type 
let (uu___is_Null_type : typ -> Prims.bool) =
  fun projectee -> match projectee with | Null_type -> true | uu___ -> false
let (uu___is_Bool_type : typ -> Prims.bool) =
  fun projectee -> match projectee with | Bool_type -> true | uu___ -> false
let (uu___is_Int_type : typ -> Prims.bool) =
  fun projectee -> match projectee with | Int_type -> true | uu___ -> false
let (uu___is_String_type : typ -> Prims.bool) =
  fun projectee ->
    match projectee with | String_type -> true | uu___ -> false
let (uu___is_Url_type : typ -> Prims.bool) =
  fun projectee -> match projectee with | Url_type -> true | uu___ -> false
let (uu___is_Type_type : typ -> Prims.bool) =
  fun projectee -> match projectee with | Type_type -> true | uu___ -> false
let (uu___is_Function_type : typ -> Prims.bool) =
  fun projectee ->
    match projectee with | Function_type -> true | uu___ -> false
let (uu___is_Code_type : typ -> Prims.bool) =
  fun projectee -> match projectee with | Code_type -> true | uu___ -> false
let (uu___is_Window_type : typ -> Prims.bool) =
  fun projectee ->
    match projectee with | Window_type -> true | uu___ -> false
let (uu___is_Node_type : typ -> Prims.bool) =
  fun projectee -> match projectee with | Node_type -> true | uu___ -> false
type var = Prims.string
type 'a expr =
  | X of 'a 
  | Null 
  | Bool of Prims.bool 
  | Int of Prims.int 
  | String of Prims.string 
  | Url of url 
  | Types of typ 
  | Seq of 'a expr * 'a expr 
  | If of 'a expr * 'a expr * 'a expr 
  | While of 'a expr * 'a expr 
  | Prim1 of Prims.string * 'a expr 
  | Prim2 of Prims.string * 'a expr * 'a expr 
  | Alert of 'a expr 
  | Function of var * var Prims.list * 'a expr 
  | Apply of 'a expr 
  | Code of unit expr 
  | Eval of 'a expr 
  | Var of var 
  | Set_var of var * 'a expr 
  | Get_cookie of 'a expr * 'a expr 
  | Set_cookie of 'a expr * 'a expr * 'a expr 
  | Xhr of 'a expr * 'a expr * 'a expr 
  | Self_win 
  | Named_win of 'a expr 
  | Open_win of 'a expr 
  | Open_named_win of 'a expr * 'a expr 
  | Close_win of 'a expr 
  | Navigate_win of 'a expr * 'a expr 
  | Is_win_closed of 'a expr 
  | Get_win_opener of 'a expr 
  | Get_win_location of 'a expr 
  | Get_win_name of 'a expr 
  | Set_win_name of 'a expr * 'a expr 
  | Get_win_root_node of 'a expr 
  | Set_win_root_node of 'a expr * 'a expr 
  | Get_win_var of 'a expr * var 
  | Set_win_var of 'a expr * var * 'a expr 
  | New_node of 'a expr 
  | Get_node_type of 'a expr 
  | Get_node_contents of 'a expr 
  | Set_node_contents of 'a expr * 'a expr 
  | Get_node_attr of 'a expr * 'a expr 
  | Set_node_attr of 'a expr * 'a expr * 'a expr 
  | Remove_handlers of 'a expr 
  | Add_handler of 'a expr * 'a expr 
  | Get_parent of 'a expr 
  | Get_child of 'a expr 
  | Insert_node of 'a expr * 'a expr * 'a expr 
  | Remove_node of 'a expr 
let uu___is_X : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | X _0 -> true | uu___ -> false
let __proj__X__item___0 : 'a . 'a expr -> 'a =
  fun projectee -> match projectee with | X _0 -> _0
let uu___is_Null : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Null -> true | uu___ -> false
let uu___is_Bool : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Bool _0 -> true | uu___ -> false
let __proj__Bool__item___0 : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Bool _0 -> _0
let uu___is_Int : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Int _0 -> true | uu___ -> false
let __proj__Int__item___0 : 'a . 'a expr -> Prims.int =
  fun projectee -> match projectee with | Int _0 -> _0
let uu___is_String : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | String _0 -> true | uu___ -> false
let __proj__String__item___0 : 'a . 'a expr -> Prims.string =
  fun projectee -> match projectee with | String _0 -> _0
let uu___is_Url : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Url _0 -> true | uu___ -> false
let __proj__Url__item___0 : 'a . 'a expr -> url =
  fun projectee -> match projectee with | Url _0 -> _0
let uu___is_Types : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Types _0 -> true | uu___ -> false
let __proj__Types__item___0 : 'a . 'a expr -> typ =
  fun projectee -> match projectee with | Types _0 -> _0
let uu___is_Seq : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Seq (_0, _1) -> true | uu___ -> false
let __proj__Seq__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Seq (_0, _1) -> _0
let __proj__Seq__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Seq (_0, _1) -> _1
let uu___is_If : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | If (_0, _1, _2) -> true | uu___ -> false
let __proj__If__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | If (_0, _1, _2) -> _0
let __proj__If__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | If (_0, _1, _2) -> _1
let __proj__If__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | If (_0, _1, _2) -> _2
let uu___is_While : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | While (_0, _1) -> true | uu___ -> false
let __proj__While__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | While (_0, _1) -> _0
let __proj__While__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | While (_0, _1) -> _1
let uu___is_Prim1 : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Prim1 (_0, _1) -> true | uu___ -> false
let __proj__Prim1__item___0 : 'a . 'a expr -> Prims.string =
  fun projectee -> match projectee with | Prim1 (_0, _1) -> _0
let __proj__Prim1__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Prim1 (_0, _1) -> _1
let uu___is_Prim2 : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Prim2 (_0, _1, _2) -> true | uu___ -> false
let __proj__Prim2__item___0 : 'a . 'a expr -> Prims.string =
  fun projectee -> match projectee with | Prim2 (_0, _1, _2) -> _0
let __proj__Prim2__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Prim2 (_0, _1, _2) -> _1
let __proj__Prim2__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Prim2 (_0, _1, _2) -> _2
let uu___is_Alert : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Alert _0 -> true | uu___ -> false
let __proj__Alert__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Alert _0 -> _0
let uu___is_Function : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Function (_0, _1, _2) -> true | uu___ -> false
let __proj__Function__item___0 : 'a . 'a expr -> var =
  fun projectee -> match projectee with | Function (_0, _1, _2) -> _0
let __proj__Function__item___1 : 'a . 'a expr -> var Prims.list =
  fun projectee -> match projectee with | Function (_0, _1, _2) -> _1
let __proj__Function__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Function (_0, _1, _2) -> _2
let uu___is_Apply : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Apply _0 -> true | uu___ -> false
let __proj__Apply__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Apply _0 -> _0
let uu___is_Code : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Code _0 -> true | uu___ -> false
let __proj__Code__item___0 : 'a . 'a expr -> Prims.l_False expr =
  fun projectee -> match projectee with | Code _0 -> _0
let uu___is_Eval : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Eval _0 -> true | uu___ -> false
let __proj__Eval__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Eval _0 -> _0
let uu___is_Var : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Var _0 -> true | uu___ -> false
let __proj__Var__item___0 : 'a . 'a expr -> var =
  fun projectee -> match projectee with | Var _0 -> _0
let uu___is_Set_var : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Set_var (_0, _1) -> true | uu___ -> false
let __proj__Set_var__item___0 : 'a . 'a expr -> var =
  fun projectee -> match projectee with | Set_var (_0, _1) -> _0
let __proj__Set_var__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_var (_0, _1) -> _1
let uu___is_Get_cookie : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_cookie (_0, _1) -> true | uu___ -> false
let __proj__Get_cookie__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_cookie (_0, _1) -> _0
let __proj__Get_cookie__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_cookie (_0, _1) -> _1
let uu___is_Set_cookie : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Set_cookie (_0, _1, _2) -> true | uu___ -> false
let __proj__Set_cookie__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_cookie (_0, _1, _2) -> _0
let __proj__Set_cookie__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_cookie (_0, _1, _2) -> _1
let __proj__Set_cookie__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_cookie (_0, _1, _2) -> _2
let uu___is_Xhr : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Xhr (_0, _1, _2) -> true | uu___ -> false
let __proj__Xhr__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Xhr (_0, _1, _2) -> _0
let __proj__Xhr__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Xhr (_0, _1, _2) -> _1
let __proj__Xhr__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Xhr (_0, _1, _2) -> _2
let uu___is_Self_win : 'a . 'a expr -> Prims.bool =
  fun projectee -> match projectee with | Self_win -> true | uu___ -> false
let uu___is_Named_win : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Named_win _0 -> true | uu___ -> false
let __proj__Named_win__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Named_win _0 -> _0
let uu___is_Open_win : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Open_win _0 -> true | uu___ -> false
let __proj__Open_win__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Open_win _0 -> _0
let uu___is_Open_named_win : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Open_named_win (_0, _1) -> true | uu___ -> false
let __proj__Open_named_win__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Open_named_win (_0, _1) -> _0
let __proj__Open_named_win__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Open_named_win (_0, _1) -> _1
let uu___is_Close_win : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Close_win _0 -> true | uu___ -> false
let __proj__Close_win__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Close_win _0 -> _0
let uu___is_Navigate_win : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Navigate_win (_0, _1) -> true | uu___ -> false
let __proj__Navigate_win__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Navigate_win (_0, _1) -> _0
let __proj__Navigate_win__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Navigate_win (_0, _1) -> _1
let uu___is_Is_win_closed : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Is_win_closed _0 -> true | uu___ -> false
let __proj__Is_win_closed__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Is_win_closed _0 -> _0
let uu___is_Get_win_opener : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_win_opener _0 -> true | uu___ -> false
let __proj__Get_win_opener__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_win_opener _0 -> _0
let uu___is_Get_win_location : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_win_location _0 -> true | uu___ -> false
let __proj__Get_win_location__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_win_location _0 -> _0
let uu___is_Get_win_name : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_win_name _0 -> true | uu___ -> false
let __proj__Get_win_name__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_win_name _0 -> _0
let uu___is_Set_win_name : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Set_win_name (_0, _1) -> true | uu___ -> false
let __proj__Set_win_name__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_win_name (_0, _1) -> _0
let __proj__Set_win_name__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_win_name (_0, _1) -> _1
let uu___is_Get_win_root_node : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_win_root_node _0 -> true | uu___ -> false
let __proj__Get_win_root_node__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_win_root_node _0 -> _0
let uu___is_Set_win_root_node : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with
    | Set_win_root_node (_0, _1) -> true
    | uu___ -> false
let __proj__Set_win_root_node__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_win_root_node (_0, _1) -> _0
let __proj__Set_win_root_node__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_win_root_node (_0, _1) -> _1
let uu___is_Get_win_var : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_win_var (_0, _1) -> true | uu___ -> false
let __proj__Get_win_var__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_win_var (_0, _1) -> _0
let __proj__Get_win_var__item___1 : 'a . 'a expr -> var =
  fun projectee -> match projectee with | Get_win_var (_0, _1) -> _1
let uu___is_Set_win_var : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Set_win_var (_0, _1, _2) -> true | uu___ -> false
let __proj__Set_win_var__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_win_var (_0, _1, _2) -> _0
let __proj__Set_win_var__item___1 : 'a . 'a expr -> var =
  fun projectee -> match projectee with | Set_win_var (_0, _1, _2) -> _1
let __proj__Set_win_var__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_win_var (_0, _1, _2) -> _2
let uu___is_New_node : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | New_node _0 -> true | uu___ -> false
let __proj__New_node__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | New_node _0 -> _0
let uu___is_Get_node_type : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_node_type _0 -> true | uu___ -> false
let __proj__Get_node_type__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_node_type _0 -> _0
let uu___is_Get_node_contents : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_node_contents _0 -> true | uu___ -> false
let __proj__Get_node_contents__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_node_contents _0 -> _0
let uu___is_Set_node_contents : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with
    | Set_node_contents (_0, _1) -> true
    | uu___ -> false
let __proj__Set_node_contents__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_node_contents (_0, _1) -> _0
let __proj__Set_node_contents__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_node_contents (_0, _1) -> _1
let uu___is_Get_node_attr : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_node_attr (_0, _1) -> true | uu___ -> false
let __proj__Get_node_attr__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_node_attr (_0, _1) -> _0
let __proj__Get_node_attr__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_node_attr (_0, _1) -> _1
let uu___is_Set_node_attr : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with
    | Set_node_attr (_0, _1, _2) -> true
    | uu___ -> false
let __proj__Set_node_attr__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_node_attr (_0, _1, _2) -> _0
let __proj__Set_node_attr__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_node_attr (_0, _1, _2) -> _1
let __proj__Set_node_attr__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Set_node_attr (_0, _1, _2) -> _2
let uu___is_Remove_handlers : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Remove_handlers _0 -> true | uu___ -> false
let __proj__Remove_handlers__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Remove_handlers _0 -> _0
let uu___is_Add_handler : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Add_handler (_0, _1) -> true | uu___ -> false
let __proj__Add_handler__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Add_handler (_0, _1) -> _0
let __proj__Add_handler__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Add_handler (_0, _1) -> _1
let uu___is_Get_parent : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_parent _0 -> true | uu___ -> false
let __proj__Get_parent__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_parent _0 -> _0
let uu___is_Get_child : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Get_child _0 -> true | uu___ -> false
let __proj__Get_child__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Get_child _0 -> _0
let uu___is_Insert_node : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Insert_node (_0, _1, _2) -> true | uu___ -> false
let __proj__Insert_node__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Insert_node (_0, _1, _2) -> _0
let __proj__Insert_node__item___1 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Insert_node (_0, _1, _2) -> _1
let __proj__Insert_node__item___2 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Insert_node (_0, _1, _2) -> _2
let uu___is_Remove_node : 'a . 'a expr -> Prims.bool =
  fun projectee ->
    match projectee with | Remove_node _0 -> true | uu___ -> false
let __proj__Remove_node__item___0 : 'a . 'a expr -> 'a expr =
  fun projectee -> match projectee with | Remove_node _0 -> _0
type elt_id = Prims.string
type doc =
  | Para of elt_id FStar_Pervasives_Native.option * Prims.string 
  | Link of elt_id FStar_Pervasives_Native.option * url * Prims.string 
  | Text of elt_id FStar_Pervasives_Native.option * Prims.string 
  | Button of elt_id FStar_Pervasives_Native.option * Prims.string 
  | Remote_script of elt_id FStar_Pervasives_Native.option * url 
  | Divi of elt_id FStar_Pervasives_Native.option * doc Prims.list 
  | Inline_script of elt_id FStar_Pervasives_Native.option * unit expr 
let (uu___is_Para : doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Para (_0, _1) -> true | uu___ -> false
let (__proj__Para__item___0 : doc -> elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Para (_0, _1) -> _0
let (__proj__Para__item___1 : doc -> Prims.string) =
  fun projectee -> match projectee with | Para (_0, _1) -> _1
let (uu___is_Link : doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Link (_0, _1, _2) -> true | uu___ -> false
let (__proj__Link__item___0 : doc -> elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Link (_0, _1, _2) -> _0
let (__proj__Link__item___1 : doc -> url) =
  fun projectee -> match projectee with | Link (_0, _1, _2) -> _1
let (__proj__Link__item___2 : doc -> Prims.string) =
  fun projectee -> match projectee with | Link (_0, _1, _2) -> _2
let (uu___is_Text : doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Text (_0, _1) -> true | uu___ -> false
let (__proj__Text__item___0 : doc -> elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Text (_0, _1) -> _0
let (__proj__Text__item___1 : doc -> Prims.string) =
  fun projectee -> match projectee with | Text (_0, _1) -> _1
let (uu___is_Button : doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Button (_0, _1) -> true | uu___ -> false
let (__proj__Button__item___0 : doc -> elt_id FStar_Pervasives_Native.option)
  = fun projectee -> match projectee with | Button (_0, _1) -> _0
let (__proj__Button__item___1 : doc -> Prims.string) =
  fun projectee -> match projectee with | Button (_0, _1) -> _1
let (uu___is_Remote_script : doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Remote_script (_0, _1) -> true | uu___ -> false
let (__proj__Remote_script__item___0 :
  doc -> elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Remote_script (_0, _1) -> _0
let (__proj__Remote_script__item___1 : doc -> url) =
  fun projectee -> match projectee with | Remote_script (_0, _1) -> _1
let (uu___is_Divi : doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Divi (_0, _1) -> true | uu___ -> false
let (__proj__Divi__item___0 : doc -> elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Divi (_0, _1) -> _0
let (__proj__Divi__item___1 : doc -> doc Prims.list) =
  fun projectee -> match projectee with | Divi (_0, _1) -> _1
let (uu___is_Inline_script : doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Inline_script (_0, _1) -> true | uu___ -> false
let (__proj__Inline_script__item___0 :
  doc -> elt_id FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | Inline_script (_0, _1) -> _0
let (__proj__Inline_script__item___1 : doc -> Prims.l_False expr) =
  fun projectee -> match projectee with | Inline_script (_0, _1) -> _1
type user_window =
  | Window of Prims.nat 
let (uu___is_Window : user_window -> Prims.bool) = fun projectee -> true
let (__proj__Window__item___0 : user_window -> Prims.nat) =
  fun projectee -> match projectee with | Window _0 -> _0
type user_input =
  | Load_in_new_window of url 
  | Load_in_window of user_window * url 
  | Link_to_new_window of user_window * url 
  | Close_window of user_window 
  | Input_text of user_window * Prims.nat * Prims.string 
  | Click_button of user_window * Prims.nat 
let (uu___is_Load_in_new_window : user_input -> Prims.bool) =
  fun projectee ->
    match projectee with | Load_in_new_window _0 -> true | uu___ -> false
let (__proj__Load_in_new_window__item___0 : user_input -> url) =
  fun projectee -> match projectee with | Load_in_new_window _0 -> _0
let (uu___is_Load_in_window : user_input -> Prims.bool) =
  fun projectee ->
    match projectee with | Load_in_window (_0, _1) -> true | uu___ -> false
let (__proj__Load_in_window__item___0 : user_input -> user_window) =
  fun projectee -> match projectee with | Load_in_window (_0, _1) -> _0
let (__proj__Load_in_window__item___1 : user_input -> url) =
  fun projectee -> match projectee with | Load_in_window (_0, _1) -> _1
let (uu___is_Link_to_new_window : user_input -> Prims.bool) =
  fun projectee ->
    match projectee with
    | Link_to_new_window (_0, _1) -> true
    | uu___ -> false
let (__proj__Link_to_new_window__item___0 : user_input -> user_window) =
  fun projectee -> match projectee with | Link_to_new_window (_0, _1) -> _0
let (__proj__Link_to_new_window__item___1 : user_input -> url) =
  fun projectee -> match projectee with | Link_to_new_window (_0, _1) -> _1
let (uu___is_Close_window : user_input -> Prims.bool) =
  fun projectee ->
    match projectee with | Close_window _0 -> true | uu___ -> false
let (__proj__Close_window__item___0 : user_input -> user_window) =
  fun projectee -> match projectee with | Close_window _0 -> _0
let (uu___is_Input_text : user_input -> Prims.bool) =
  fun projectee ->
    match projectee with | Input_text (_0, _1, _2) -> true | uu___ -> false
let (__proj__Input_text__item___0 : user_input -> user_window) =
  fun projectee -> match projectee with | Input_text (_0, _1, _2) -> _0
let (__proj__Input_text__item___1 : user_input -> Prims.nat) =
  fun projectee -> match projectee with | Input_text (_0, _1, _2) -> _1
let (__proj__Input_text__item___2 : user_input -> Prims.string) =
  fun projectee -> match projectee with | Input_text (_0, _1, _2) -> _2
let (uu___is_Click_button : user_input -> Prims.bool) =
  fun projectee ->
    match projectee with | Click_button (_0, _1) -> true | uu___ -> false
let (__proj__Click_button__item___0 : user_input -> user_window) =
  fun projectee -> match projectee with | Click_button (_0, _1) -> _0
let (__proj__Click_button__item___1 : user_input -> Prims.nat) =
  fun projectee -> match projectee with | Click_button (_0, _1) -> _1
type rendered_doc =
  | Para_rendered of Prims.string 
  | Link_rendered of url * Prims.string 
  | Textbox_rendered of Prims.string 
  | Button_rendered of Prims.string 
  | Div_rendered of rendered_doc Prims.list 
let (uu___is_Para_rendered : rendered_doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Para_rendered _0 -> true | uu___ -> false
let (__proj__Para_rendered__item___0 : rendered_doc -> Prims.string) =
  fun projectee -> match projectee with | Para_rendered _0 -> _0
let (uu___is_Link_rendered : rendered_doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Link_rendered (_0, _1) -> true | uu___ -> false
let (__proj__Link_rendered__item___0 : rendered_doc -> url) =
  fun projectee -> match projectee with | Link_rendered (_0, _1) -> _0
let (__proj__Link_rendered__item___1 : rendered_doc -> Prims.string) =
  fun projectee -> match projectee with | Link_rendered (_0, _1) -> _1
let (uu___is_Textbox_rendered : rendered_doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Textbox_rendered _0 -> true | uu___ -> false
let (__proj__Textbox_rendered__item___0 : rendered_doc -> Prims.string) =
  fun projectee -> match projectee with | Textbox_rendered _0 -> _0
let (uu___is_Button_rendered : rendered_doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Button_rendered _0 -> true | uu___ -> false
let (__proj__Button_rendered__item___0 : rendered_doc -> Prims.string) =
  fun projectee -> match projectee with | Button_rendered _0 -> _0
let (uu___is_Div_rendered : rendered_doc -> Prims.bool) =
  fun projectee ->
    match projectee with | Div_rendered _0 -> true | uu___ -> false
let (__proj__Div_rendered__item___0 :
  rendered_doc -> rendered_doc Prims.list) =
  fun projectee -> match projectee with | Div_rendered _0 -> _0
type user_output =
  | Window_opened 
  | Window_closed of user_window 
  | Page_loaded of user_window * url * rendered_doc 
  | Page_updated of user_window * rendered_doc 
let (uu___is_Window_opened : user_output -> Prims.bool) =
  fun projectee ->
    match projectee with | Window_opened -> true | uu___ -> false
let (uu___is_Window_closed : user_output -> Prims.bool) =
  fun projectee ->
    match projectee with | Window_closed _0 -> true | uu___ -> false
let (__proj__Window_closed__item___0 : user_output -> user_window) =
  fun projectee -> match projectee with | Window_closed _0 -> _0
let (uu___is_Page_loaded : user_output -> Prims.bool) =
  fun projectee ->
    match projectee with | Page_loaded (_0, _1, _2) -> true | uu___ -> false
let (__proj__Page_loaded__item___0 : user_output -> user_window) =
  fun projectee -> match projectee with | Page_loaded (_0, _1, _2) -> _0
let (__proj__Page_loaded__item___1 : user_output -> url) =
  fun projectee -> match projectee with | Page_loaded (_0, _1, _2) -> _1
let (__proj__Page_loaded__item___2 : user_output -> rendered_doc) =
  fun projectee -> match projectee with | Page_loaded (_0, _1, _2) -> _2
let (uu___is_Page_updated : user_output -> Prims.bool) =
  fun projectee ->
    match projectee with | Page_updated (_0, _1) -> true | uu___ -> false
let (__proj__Page_updated__item___0 : user_output -> user_window) =
  fun projectee -> match projectee with | Page_updated (_0, _1) -> _0
let (__proj__Page_updated__item___1 : user_output -> rendered_doc) =
  fun projectee -> match projectee with | Page_updated (_0, _1) -> _1
type cookies = {
  cookie_list: (Prims.string * Prims.string) Prims.list }
let (__proj__Mkcookies__item__cookie_list :
  cookies -> (Prims.string * Prims.string) Prims.list) =
  fun projectee -> match projectee with | { cookie_list;_} -> cookie_list
type cookie_updates =
  {
  del_cookies: Prims.string Prims.list ;
  set_cookies: (Prims.string * Prims.string) Prims.list }
let (__proj__Mkcookie_updates__item__del_cookies :
  cookie_updates -> Prims.string Prims.list) =
  fun projectee ->
    match projectee with | { del_cookies; set_cookies;_} -> del_cookies
let (__proj__Mkcookie_updates__item__set_cookies :
  cookie_updates -> (Prims.string * Prims.string) Prims.list) =
  fun projectee ->
    match projectee with | { del_cookies; set_cookies;_} -> set_cookies
type request =
  | Request of req_uri * cookies * Prims.string 
let (uu___is_Request : request -> Prims.bool) = fun projectee -> true
let (__proj__Request__item___0 : request -> req_uri) =
  fun projectee -> match projectee with | Request (_0, _1, _2) -> _0
let (__proj__Request__item___1 : request -> cookies) =
  fun projectee -> match projectee with | Request (_0, _1, _2) -> _1
let (__proj__Request__item___2 : request -> Prims.string) =
  fun projectee -> match projectee with | Request (_0, _1, _2) -> _2
type network_ouput =
  | Send of domain * request 
let (uu___is_Send : network_ouput -> Prims.bool) = fun projectee -> true
let (__proj__Send__item___0 : network_ouput -> domain) =
  fun projectee -> match projectee with | Send (_0, _1) -> _0
let (__proj__Send__item___1 : network_ouput -> request) =
  fun projectee -> match projectee with | Send (_0, _1) -> _1
type response =
  | Doc_response of cookie_updates * doc 
  | Script_response of cookie_updates * unit expr 
let (uu___is_Doc_response : response -> Prims.bool) =
  fun projectee ->
    match projectee with | Doc_response (_0, _1) -> true | uu___ -> false
let (__proj__Doc_response__item___0 : response -> cookie_updates) =
  fun projectee -> match projectee with | Doc_response (_0, _1) -> _0
let (__proj__Doc_response__item___1 : response -> doc) =
  fun projectee -> match projectee with | Doc_response (_0, _1) -> _1
let (uu___is_Script_response : response -> Prims.bool) =
  fun projectee ->
    match projectee with | Script_response (_0, _1) -> true | uu___ -> false
let (__proj__Script_response__item___0 : response -> cookie_updates) =
  fun projectee -> match projectee with | Script_response (_0, _1) -> _0
let (__proj__Script_response__item___1 : response -> Prims.l_False expr) =
  fun projectee -> match projectee with | Script_response (_0, _1) -> _1
type network_connection =
  | Connection of domain * Prims.nat 
let (uu___is_Connection : network_connection -> Prims.bool) =
  fun projectee -> true
let (__proj__Connection__item___0 : network_connection -> domain) =
  fun projectee -> match projectee with | Connection (_0, _1) -> _0
let (__proj__Connection__item___1 : network_connection -> Prims.nat) =
  fun projectee -> match projectee with | Connection (_0, _1) -> _1
type network_input =
  | Receive of network_connection * response 
let (uu___is_Receive : network_input -> Prims.bool) = fun projectee -> true
let (__proj__Receive__item___0 : network_input -> network_connection) =
  fun projectee -> match projectee with | Receive (_0, _1) -> _0
let (__proj__Receive__item___1 : network_input -> response) =
  fun projectee -> match projectee with | Receive (_0, _1) -> _1