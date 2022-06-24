module Top_level

open BrowserIO
open Assumed
open Browser

(** {3 Top-level browser functionality} *)

  (** The type of a browser waiting for input. *)
  type waiting = browser

  (** The type of a browser in the middle of processing the current input. *)
  type running = {
    running_state: browser;
    running_task_queue: list task;
  }

 
  type input_event = BrowserIO.input_event
  type output_event = BrowserIO.output_event

  (** The type of a browser state, in general. *)
  type state =
    | Waiting of waiting
    | Running of running

  (** The initial configuration of a browser. *)
  let start: state =
    Waiting ({
        browser_windows = [];
        browser_pages = [];
        browser_nodes = [];
        browser_environments = [];
        browser_cookies = [];
        browser_connections = [];
    })

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
    else (waiting(b), [])

  // (** Handling a [User_close_win_event]. *)
  // let handle_close_win_event (wr: win_ref) (b: b)
  // : state * output_event list =
  //   assert (win_valid wr b);
  //   let oe = UI_win_closed_event(win_to_user_window wr b) in
  //   let w' = { waiting_state = win_remove wr b } in
  //   (Waiting(w'), [ oe ])

  // (** Handling an [User_input_text_event]. *)
  // let handle_input_text_event (wr: win_ref) (box_pos: int) (str: string) (b: b)
  // : state * output_event list =                                    
  //   assert (win_valid wr b);
  //   begin match textbox_handlers_in_pos wr box_pos b with
  //   | None ->           
  //       (Waiting({ waiting_state = b }), [])
  //   | Some(dr, vs) ->
  //     let task v = {
  //         task_win = wr;
  //         task_expr = Apply(X(R(v)), X(R(Node_value(dr))));
  //       } in
  //       let n' =
  //         begin match node_assoc_valid dr b with
  //         | Textbox_node(oeid, _, hs) -> Textbox_node(oeid, str, hs)
  //         | _ -> assert false
  //         end
  //       in
  //       let b' = node_update dr n' b in
  //       let r = {
  //         running_state = b';
  //         running_task_queue = List.map task vs;
  //       } in
  //       (Running(r), [])
  //   end

  // (** Handling a [User_click_button_event]. *)
  // let handle_click_button_event (wr: win_ref) (but_pos: int) (b: b)
  // : state * output_event list =
  //   assert (win_valid wr b);
  //   begin match button_handlers_in_pos wr but_pos b with
  //   | None -> (Waiting({ waiting_state = b }), [])
  //   | Some(dr, vs) ->
  //       let task v = {
  //         task_win = wr;
  //         task_expr = Apply(X(R(v)), X(R(Node_value(dr))));
  //       } in
  //       let r = {
  //         running_state = b;
  //         running_task_queue = List.map task vs;
  //       } in
  //       (Running(r), [])
  //   end

  // (** Return the known expressions at the head of the script queue for the page
  //     in the window referenced by [wr] as browser tasks, and remove them from
  //     the page's script queue. *)
  // let get_ready_exprs (wr: win_ref) (b: b)
  // : task list * b =
  //   assert (win_valid wr b);
  //   let task e = {
  //     task_win = wr;
  //     task_expr = e;
  //   } in
  //   let w = win_assoc_valid wr b in
  //   let p = page_assoc_valid w.win_page b in
  //   let (pes_ready, pes') = split_queued_exprs p.page_script_queue in
  //   let p' = {
  //     p with
  //     page_script_queue = pes';
  //   } in
  //   let b' = page_update w.win_page p' b in
  //   (List.map task pes_ready, b')

  // (** Handling a [Network_response_event]. *)
  // let handle_network_response_event
  //   (Net_connection(d, n): net_connection) (resp: resp) (b: b)
  // : state * output_event list =
  //   begin match net_connection_domain_nth d n b with
  //   | None ->
  //       (Waiting({ waiting_state = b }), [])
  //   | Some((uri, dst)) ->
  //       let b' = net_connection_domain_remove_nth d n b in
  //       let b'1 = upd_cookies d uri resp b' in
  //       begin match (dst, resp.resp_body) with

  //       | (Xhr_dst(pr, v), Script_file(e)) when page_win pr b'1 <> None ->
  //           let wr =
  //             begin match page_win pr b'1 with
  //             | Some(wr) -> wr
  //             | None -> assert false
  //             end
  //           in
  //           let t = {
  //             task_win = wr;
  //             task_expr = Apply(X(R(v)), X(R(Code_value(e))));
  //           } in
  //           let r = {
  //             running_state = b'1;
  //             running_task_queue = [ t ];
  //           } in
  //           (Running(r), [])

  //       | (Doc_dst(wr), Html_file(docs)) when win_valid wr b'1 ->
  //           let uw = win_to_user_window wr b'1 in
  //           let w = win_assoc_valid wr b'1 in
  //           let b'2 = page_remove w.win_page b'1 in
  //           let (dr, node_ext) = build_node_tree (Div(None, docs)) in
  //           let b'3 = {
  //             b'2 with
  //             browser_nodes = b'2.browser_nodes @ node_ext
  //           } in
  //           let (w', b'4) =
  //             build_win
  //               w.win_name (Http_url(d, uri)) w.win_opener (Some(dr)) b'3
  //           in
  //           let b'5 = win_update wr w' b'4 in
  //           let rd_opt = render_page w'.win_page b'4 in
  //           let oe = UI_page_loaded_event(uw, Http_url(d, uri), rd_opt) in
  //           let (b'6, oes1, ts) = process_node_scripts w'.win_page dr b'5 in
  //           let r = {
  //             running_state = b'6;
  //             running_task_queue = ts;
  //           } in
  //           (Running(r), [ oe ] @ oes1)

  //       | (Doc_dst(wr), _) when win_valid wr b'1 ->
  //           let uw = win_to_user_window wr b'1 in
  //           let w = win_assoc_valid wr b'1 in
  //           let b'3 = page_remove w.win_page b'1 in
  //           let (w', b'4) =
  //             build_win w.win_name (Http_url(d, uri)) w.win_opener None b'3
  //           in
  //           let b'5 = win_update wr w' b'4 in
  //           let rd_opt = render_page w'.win_page b'5 in
  //           let oe = UI_page_loaded_event(uw, Http_url(d, uri), rd_opt) in
  //           (Waiting({ waiting_state = b'5 }), [ oe ])

  //       | (Script_dst(pr, dr), Script_file(e)) when page_win pr b'1 <> None ->
  //           let wr =
  //             begin match page_win pr b'1 with
  //             | Some(wr) -> wr
  //             | None -> assert false
  //             end
  //           in
  //           let p = page_assoc_valid pr b'1 in
  //           let q = p.page_script_queue in
  //           let q' =
  //             let upd p =
  //               begin match p with
  //               | Unknown_expr(dr1) when dr1 = dr -> Known_expr(to_inner_expr e)
  //               | _ -> p
  //               end
  //             in
  //             List.map upd q
  //           in
  //           let p' = { p with page_script_queue = q' } in
  //           let b'2 = page_update pr p' b'1 in
  //           let (ts, b'3) = get_ready_exprs wr b'2 in
  //           let r = {
  //             running_state = b'3;
  //             running_task_queue = ts;
  //           } in
  //           (Running(r), [])

  //       | _ ->
  //         (Waiting({ waiting_state = b'1 }), [])

  //       end
  //   end

  // (** Respond to an incoming event. *)
  // let receive (ie: input_event) (w: waiting)
  // : state * list output_event=
  //   begin match ie with

  //   | User_load_in_new_win_event(u) ->
  //       handle_load_in_new_win_event No_opener No_name u w.waiting_state

  //   | User_load_in_win_event(uw, u) ->
  //       begin match win_from_user_window uw w.waiting_state with
  //       | None -> (Waiting(w), [])
  //       | Some(wr) -> handle_load_in_win_event wr u w.waiting_state
  //       end

  //   | User_link_to_new_win_event(uw, u) ->
  //       let wo =
  //         begin match win_from_user_window uw w.waiting_state with
  //         | None -> No_opener
  //         | Some(wr) -> Win_opener(wr)
  //         end
  //       in
  //       handle_load_in_new_win_event wo No_name u w.waiting_state

  //   | User_link_to_named_win_event(uw, str, u) ->
  //       let wo =
  //         begin match win_from_user_window uw w.waiting_state with
  //         | None -> No_opener
  //         | Some(wr) -> Win_opener(wr)
  //         end
  //       in
  //       begin match win_from_win_name str w.waiting_state with
  //       | None ->
  //           handle_load_in_new_win_event wo (Str_name(str)) u w.waiting_state
  //       | Some(wr) ->
  //           handle_load_in_win_event wr u w.waiting_state
  //       end

  //   | User_close_win_event(uw) ->
  //       begin match win_from_user_window uw w.waiting_state with
  //       | None -> (Waiting(w), [])
  //       | Some(wr) -> handle_close_win_event wr w.waiting_state
  //       end

  //   | User_input_text_event(User_textbox(uw, box_pos), str, _) ->
  //       begin match win_from_user_window uw w.waiting_state with
  //       | None -> (Waiting(w), [])
  //       | Some(wr) -> handle_input_text_event wr box_pos str w.waiting_state
  //       end

  //   | User_click_button_event(User_button(uw, but_pos)) ->
  //       begin match win_from_user_window uw w.waiting_state with
  //       | None -> (Waiting(w), [])
  //       | Some(wr) -> handle_click_button_event wr but_pos w.waiting_state
  //       end

  //   | Network_response_event(nr, resp) ->
  //       handle_network_response_event nr resp w.waiting_state

  //   end

  // (** Advance the browser state one step. *)
  // let continue (r: running)
  // : state * list output_event =
  //   let current t = win_valid t.task_win r.running_state in
  //   begin match r.running_task_queue with
  //   | [] ->
  //       (Waiting({ waiting_state = r.running_state}), [])
  //   | t :: ts when not (current t) ->
  //       let r' = { r with running_task_queue = ts } in
  //       (Running(r'), [])
  //   | { task_expr = X(R(Error(msg))) } :: ts ->
  //       let r' = { r with running_task_queue = ts } in
  //       (Running(r'), [ UI_error(msg) ])
  //   | { task_expr = X(R(_)) } :: ts ->
  //       let r' = { r with running_task_queue = ts } in
  //       (Running(r'), [])
  //   | t :: ts -> (* the task is current and not finished *)
  //       let b = r.running_state in
  //       let w = win_assoc_valid t.task_win b in
  //       let p = page_assoc_valid w.win_page b in
  //       let ctx = {
  //         context_win = t.task_win;
  //         context_act = p.page_environment;
  //       } in
  //       let (b', e', oes, ts') = step_expr ctx b t.task_expr in
  //       let r' = {
  //         running_state = b';
  //         running_task_queue = { t with task_expr = e' } :: ts @ ts';
  //       } in
  //       (Running(r'), oes)
  //   end


