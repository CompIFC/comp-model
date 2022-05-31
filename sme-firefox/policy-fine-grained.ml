(** Fine-grained origin separation policy *)

open Level ;;
open Policyfun ;;
open Io ;;

(** [label_input input] assigns a security level to an input event [input] *)
let label_input input =
        match input with 
        | User_load_in_new_win_event(url) -> L
        | User_load_in_win_event(user_window, url) -> L
        | User_link_to_new_win_event(user_window, url) -> level_of_url url
        | User_link_to_named_win_event(user_window, name, url) -> level_of_url url
        | User_close_win_event(user_window) -> L
        | User_input_text_event(user_textbox, text, level_option) -> 
             begin match level_option with 
             | None ->             
               begin match user_textbox with 
               | User_textbox(user_window, nat) ->                              
                     match user_window with 
                     | User_window(url, nat2) -> level_of_url url
               end      
             | Some(level) -> level
             end
        | User_click_button_event(user_button) -> H
        | Network_response_event(net_conn, resp) ->
                        M(domain_of_net_connection net_conn)               
        

(** [label_output output] assigns a security level to output event [output] *)
let label_output output = 
        match output with 
        | UI_win_opened_event -> H
        | UI_win_closed_event(user_window) -> H
        | UI_page_loaded_event(user_winndow, url, rendered_doc) -> H
        | UI_page_updated_event(user_window, rendered_doc) -> H
        | UI_alert(text)-> H
        | UI_error(text) -> H
        | Network_send_event (domain, req) -> M(domain)     
               
(** [pi label input] projection function of input event [input] to a level [label]*)      
let pi label input =         
        match input with 
        | User_load_in_new_win_event( _ )  
        | User_load_in_win_event(_, _) 
        | User_link_to_new_win_event(_,_) 
        | User_link_to_named_win_event(_,_,_)
        | User_close_win_event(_)
        | User_click_button_event(_) ->        
              if (leq (label_input input) label) then                 
                        (Event input)                
              else Suppress  

        | User_input_text_event(user_textbox, text, level_option) ->
              begin match level_option with 
              | None -> if (leq (label_input input) label) then                 
                                (Event input)                
                         else Suppress  
              | Some(level) -> if (leq level label) then 
                                (Event input)         
                               else Suppress         
              end

        | Network_response_event(net_conn, {resp_del_cookies=_; resp_set_cookies=_;
        resp_body = body}) ->          
                let dom = (domain_of_net_connection net_conn) in                    
                match label with 
                | L -> Suppress             
                | H -> Event(input)
                | M(dom') -> 
                        if (dom=dom') then Event(input)
                        else
                             let body'= (proj dom' body) in
                             Event(Network_response_event(net_conn, 
                                  {resp_del_cookies=[]; 
                                   resp_set_cookies=[];
                                   resp_body = body'} ))                                          

(** [ro input] defines a of additional levels for input [input] *)
let ro input = match input with 
        | User_load_in_new_win_event( _ )  
        | User_load_in_win_event(_, _) 
        | User_link_to_new_win_event(_,_) 
        | User_link_to_named_win_event(_,_,_)
        | User_close_win_event(_)
        | User_input_text_event(_, _, _)
        | User_click_button_event(_) ->
                []
        | Network_response_event(_, {resp_del_cookies=_; resp_set_cookies=_;
        resp_body = body}) ->
                let new_domains = (build_new_domains body) in 
                levels_from_domains new_domains    
                  


