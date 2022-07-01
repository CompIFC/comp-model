module Top_level

open BrowserIO
open Assumed
open Browser
open FStar.List.Tot.Base
open FStar.All

(** {3 Top-level browser functionality} *)

  (** Handling a [User_load_in_new_win_event]. *)
  let handle_load_in_new_win_event
    (wo: win_opener) (wn: win_name) (u: url) (b: browser)
  : state * list output_event =
    let (_, b', oes) = open_win wn u wo b in
    let r = {
      running_state = b';
      running_task_queue = [];
    } in
    (Running(r), oes)

  (** Handling a [User_load_in_win_event]. *)
  let handle_load_in_win_event (wr: win_ref) (u: url) (b: browser)
  : state * list output_event =
    if (win_valid wr b) then
    let (b', oes) = direct_win wr u b in
    let r = {
      running_state = b';
      running_task_queue = [];
    } in
    (Running(r), oes)
    else (Waiting(b), [UI_error "invalid win_ref"])

  (** Handling a [User_close_win_event]. *)
  let handle_close_win_event (wr: win_ref) (b: browser)
  : state * list output_event =
    if (win_valid wr b && page_valid (win_assoc_valid wr b).win_page b) then
    let oe = UI_win_closed_event(win_to_user_window wr b) in
    let w' = win_remove wr b  in
    (Waiting(w'), [ oe ])
    else (Waiting(b), [UI_error "invalid win_ref"])

  (** Handling an [User_input_text_event]. *)
  let handle_input_text_event (wr: win_ref) (box_pos: nat) (str: string) (b: browser)
  : state * list output_event =                                    
    if (win_valid wr b) then
    begin match textbox_handlers_in_pos wr box_pos b with
    | None ->           
        (Waiting(b), [])
    | Some(dr, vs) ->
    if node_valid dr b then
      let task v = {
          task_win = wr;
          task_expr = Apply(X(R(v)), X(R(Node_value(dr))));
        } in
        let n' = node_assoc_valid dr b
        in
        let b' = node_update dr n' b in
        let r = {
          running_state = b';
          running_task_queue = map task vs;
        } in
        (Running(r), [])
    else (Waiting(b), [UI_error " invalid text event"])
    end
    else (Waiting(b), [UI_error "invalid win_ref"])

  (** Handling a [User_click_button_event]. *)
  let handle_click_button_event (wr: win_ref) (but_pos: nat) (b: browser)
  : state * list output_event=
    if (win_valid wr b) then
    begin match button_handlers_in_pos wr but_pos b with
    | None -> (Waiting(b), [])
    | Some(dr, vs) ->
        let task v = {
          task_win = wr;
          task_expr = Apply(X(R(v)), X(R(Node_value(dr))));
        } in
        let r = {
          running_state = b;
          running_task_queue = map task vs;
        } in
        (Running(r), [])
    end
    else (Waiting(b), [UI_error "invalid win_ref"])

  (** Return the known expressions at the head of the script queue for the page
      in the window referenced by [wr] as browser tasks, and remove them from
      the page's script queue. *)
  let get_ready_exprs (wr: win_ref) (b: browser)
  : list task * browser =
    if (win_valid wr b && page_valid (win_assoc_valid wr b).win_page b) then
    let task e = {
      task_win = wr;
      task_expr = e;
    } in
    let w = win_assoc_valid wr b in
    let p = page_assoc_valid w.win_page b in
    let (pes_ready, pes') = split_queued_exprs p.page_script_queue in
    let p' = {
      p with
      page_script_queue = pes';
    } in
    let b' = page_update w.win_page p' b in
    (map task pes_ready, b')
    else 
    ([],b)


  (** Handling a [Network_response_event]. *)
  let handle_network_response_event
    (nc: net_connection) (resp: resp) (b: browser)
  : All (state * list output_event) 
    (requires (fun h0 ->  True))
    (ensures (fun h0 r h1 -> True))
  =
    let (d,n) = nc in 
    begin match net_connection_domain_nth d n b with
    | None ->
      (Waiting(b), [])
    | Some((uri, dst)) ->
        let b' = net_connection_domain_remove_nth d n b in
        let b'1 = upd_cookies d uri resp b' in
        begin match (dst, resp.resp_body) with

        | (Xhr_dst(pr, v), Script_file(e))  -> 
            begin match page_win pr b'1 with
              | Some(w) -> 
                let wr = w   
              in
              let t = {
                task_win = wr;
                task_expr = Apply(X(R(v)), X(R(Code_value(e))));
              } in
              let r = {
                running_state = b'1;
                running_task_queue = [ t ];
              } in
              (Running(r), [])

              | None -> (Waiting(b), [])
              end
           

        | (Doc_dst(wr), Html_file(docs)) -> if (win_valid wr b'1 && page_valid (win_assoc_valid wr b'1).win_page b'1 ) then
            let uw = win_to_user_window wr b'1 in
            let w = win_assoc_valid wr b'1 in
            let b'2 = page_remove w.win_page b'1 in
            let nd_ref= if length b'2.browser_nodes >= 1 then (fst (last b'2.browser_nodes)) else 0 in
            let (dr, node_ext) = build_node_tree (Divi(None, docs)) (nd_ref) in
            let new_nodes = b'2.browser_nodes @ node_ext in 
            let b'3 = 
              if (b_node_pred new_nodes) then
              {b'2 with
              browser_nodes = new_nodes}
              else 
              failwith "b_node_pred is not satisfied"
            in
            let (w', b'4) =
              build_win
                w.win_name (Http_url(d, uri)) w.win_opener (Some(dr)) b'3
            in
            let b'5 = win_update wr w' b'4 in
            let rd_opt = render_page w'.win_page b'4 in
            let oe = UI_page_loaded_event(uw, Http_url(d, uri), rd_opt) in
            let (b'6, oes1, ts) = process_node_scripts w'.win_page dr b'5 in
            let r = {
              running_state = b'6;
              running_task_queue = ts;
            } in
            (Running(r), [ oe ] @ oes1)

            else (Waiting(b'1), [])

        | (Doc_dst(wr), _)  -> if (win_valid wr b'1 && page_valid (win_assoc_valid wr b'1).win_page b'1 ) then
            let uw = win_to_user_window wr b'1 in
            let w = win_assoc_valid wr b'1 in
            let b'3 = page_remove w.win_page b'1 in
            let (w', b'4) =
              build_win w.win_name (Http_url(d, uri)) w.win_opener None b'3
            in
            let b'5 = win_update wr w' b'4 in
            let rd_opt = render_page w'.win_page b'5 in
            let oe = UI_page_loaded_event(uw, Http_url(d, uri), rd_opt) in
            (Waiting(b'5), [ oe ])

            else (Waiting(b'1), [])

        | (Script_dst(pr, dr), Script_file(e)) ->
              if (page_valid pr b'1) then
              begin match page_win pr b'1 with
              | Some(w) -> 
                let wr = w
                  in
                let p = page_assoc_valid pr b'1 in
                let q = p.page_script_queue in
                let q' =
                  let upd p =
                    begin match p with
                    | Unknown_expr(dr1)  -> if dr1 = dr then Known_expr(to_inner_expr e)
                    else p
                    | _ -> p
                    end
                  in
                  map upd q
                in
                let p' = { p with page_script_queue = q' } in
                let b'2 = page_update pr p' b'1 in
                let (ts, b'3) = get_ready_exprs wr b'2 in
                let r = {
                  running_state = b'3;
                  running_task_queue = ts;
                } in
                (Running(r), [])

              | None -> (Waiting(b'1), [])
              end
              else failwith "page not in browser"
            

        | _ ->
          (Waiting(b'1), [])

        end
    end

  (** Respond to an incoming event. *)
  let receive (ie: input_event) (w: waiting) =
    begin match ie with

    | User_load_in_new_win_event(u) ->
        handle_load_in_new_win_event No_opener No_name u w

    | User_load_in_win_event(uw, u) ->
        begin match win_from_user_window uw w with
        | None -> (Waiting(w), [])
        | Some(wr) -> handle_load_in_win_event wr u w
        end

    | User_link_to_new_win_event(uw, u) ->
        let wo =
          begin match win_from_user_window uw w with
          | None -> No_opener
          | Some(wr) -> Win_opener(wr)
          end
        in
        handle_load_in_new_win_event wo No_name u w

    | User_link_to_named_win_event(uw, str, u) ->
        let wo =
          begin match win_from_user_window uw w with
          | None -> No_opener
          | Some(wr) -> Win_opener(wr)
          end
        in
        begin match win_from_win_name str w with
        | None ->
            handle_load_in_new_win_event wo (Str_name(str)) u w
        | Some(wr) ->
            handle_load_in_win_event wr u w
        end

    | User_close_win_event(uw) ->
        begin match win_from_user_window uw w with
        | None -> (Waiting(w), [])
        | Some(wr) -> handle_close_win_event wr w
        end

    | User_input_text_event((uw, box_pos), str) ->
        begin match win_from_user_window uw w with
        | None -> (Waiting(w), [])
        | Some(wr) -> handle_input_text_event wr box_pos str w
        end

    | User_click_button_event((uw, but_pos)) ->
        begin match win_from_user_window uw w with
        | None -> (Waiting(w), [])
        | Some(wr) -> handle_click_button_event wr but_pos w
        end

    | Network_response_event(nr, resp) ->
        handle_network_response_event nr resp w

    end

  (** Advance the browser state one step. *)
  let continue (r: running) =

    let current t = win_valid t.task_win r.running_state in
    begin match r.running_task_queue with
    | [] ->
        (Waiting(r.running_state), [])
    | t :: ts -> 
      if (not(current t)) then 
        let r' = { r with running_task_queue = ts } in
        (Running(r'), [])
      else 
        begin match t.task_expr with 
        | X(R(Error(msg))) ->
          let r' = { r with running_task_queue = ts } in
          (Running(r'), [ UI_error(msg) ])
        | X(R(_)) ->
          let r' = { r with running_task_queue = ts } in
          (Running(r'), [])
        | _ -> (* the task is current and not finished *)
        let b = r.running_state in
        let w = win_assoc_valid t.task_win b in
        if (page_valid w.win_page b) then
        let p = page_assoc_valid w.win_page b in
        let ctx = {
          context_win = t.task_win;
          context_act = p.page_environment;
        } in
        let (b', e', oes, ts') = step_expr ctx b t.task_expr in
        let r' = {
          running_state = b';
          running_task_queue = { t with task_expr = e' } :: ts @ ts';
        } in
        (Running(r'), oes)
        
        else failwith "page not valid"

        end
    end


