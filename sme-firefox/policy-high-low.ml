(** High/Low policy *)

open Level ;;
open Policyfun ;;
open Io ;;

(** [label_input input] assigns a security level to an input event [input] *)
let label_input input =
        match input with 
        | User_load_in_new_win_event(url) -> L
        | User_load_in_win_event(user_window, url) -> L
        | User_link_to_new_win_event(user_window, url) -> L
        | User_link_to_named_win_event(user_window, name, url) -> L
        | User_close_win_event(user_window) -> L
        | User_input_text_event(user_textbox, text, _) -> H
        | User_click_button_event(user_button) -> L
        | Network_response_event(net_connection, resp) ->  L        

(** [label_output output] assigns a security level to output event [output] *)
let label_output output = 
        match output with 
        | UI_win_opened_event -> H
        | UI_win_closed_event(user_window) -> H
        | UI_page_loaded_event(user_winndow, url, rendered_doc) -> H 
        | UI_page_updated_event(user_window, rendered_doc) -> H
        | UI_alert(text)-> H
        | UI_error(text) -> H
        | Network_send_event (domain, req) -> L
        
(** [pi label input] projection function of input event [input] to a level [label]*)           
let pi label input = 
        if (leq (label_input input) label) then 
                (Event input)
        else Suppress           

(** [ro input] defines a of additional levels for input [input] *)
let ro input = []        
