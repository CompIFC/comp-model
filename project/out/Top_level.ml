open Prims
type waiting = Browser.browser
type running =
  {
  running_state: Browser.browser ;
  running_task_queue: Browser.task Prims.list }
let (__proj__Mkrunning__item__running_state : running -> Browser.browser) =
  fun projectee ->
    match projectee with
    | { running_state; running_task_queue;_} -> running_state
let (__proj__Mkrunning__item__running_task_queue :
  running -> Browser.task Prims.list) =
  fun projectee ->
    match projectee with
    | { running_state; running_task_queue;_} -> running_task_queue
type input_event = BrowserIO.input_event
type output_event = BrowserIO.output_event
type state =
  | Waiting of waiting 
  | Running of running 
let (uu___is_Waiting : state -> Prims.bool) =
  fun projectee -> match projectee with | Waiting _0 -> true | uu___ -> false
let (__proj__Waiting__item___0 : state -> waiting) =
  fun projectee -> match projectee with | Waiting _0 -> _0
let (uu___is_Running : state -> Prims.bool) =
  fun projectee -> match projectee with | Running _0 -> true | uu___ -> false
let (__proj__Running__item___0 : state -> running) =
  fun projectee -> match projectee with | Running _0 -> _0
let (start : state) =
  Waiting
    {
      Browser.browser_windows = [];
      Browser.browser_pages = [];
      Browser.browser_nodes = [];
      Browser.browser_environments = [];
      Browser.browser_cookies = [];
      Browser.browser_connections = []
    }
let (handle_load_in_new_win_event :
  Browser.win_opener ->
    Browser.win_name ->
      BrowserIO.url ->
        Browser.browser -> (state * BrowserIO.output_event Prims.list))
  =
  fun wo ->
    fun wn ->
      fun u ->
        fun b ->
          let uu___ = Browser.open_win wn u wo b in
          match uu___ with
          | (uu___1, b', oes) ->
              let r = { running_state = b'; running_task_queue = [] } in
              ((Running r), oes)
let (handle_load_in_win_event :
  Browser.win_ref ->
    BrowserIO.url ->
      Browser.browser -> (state * BrowserIO.output_event Prims.list))
  =
  fun wr ->
    fun u ->
      fun b ->
        if Browser.win_valid wr b
        then
          let uu___ = Browser.direct_win wr u b in
          match uu___ with
          | (b', oes) ->
              let r = { running_state = b'; running_task_queue = [] } in
              ((Running r), oes)
        else ((Waiting b), [BrowserIO.UI_error "invalid win_ref"])
let (handle_close_win_event :
  Browser.win_ref ->
    Browser.browser -> (state * BrowserIO.output_event Prims.list))
  =
  fun wr ->
    fun b ->
      if
        (Browser.win_valid wr b) &&
          (Browser.page_valid (Browser.win_assoc_valid wr b).Browser.win_page
             b)
      then
        let oe =
          BrowserIO.UI_win_closed_event (Browser.win_to_user_window wr b) in
        let w' = Browser.win_remove wr b in ((Waiting w'), [oe])
      else ((Waiting b), [BrowserIO.UI_error "invalid win_ref"])
let (handle_input_text_event :
  Browser.win_ref ->
    Prims.nat ->
      Prims.string ->
        Browser.browser -> (state * BrowserIO.output_event Prims.list))
  =
  fun wr ->
    fun box_pos ->
      fun str ->
        fun b ->
          if Browser.win_valid wr b
          then
            match Browser.textbox_handlers_in_pos wr box_pos b with
            | FStar_Pervasives_Native.None -> ((Waiting b), [])
            | FStar_Pervasives_Native.Some (dr, vs) ->
                (if Browser.node_valid dr b
                 then
                   let task v =
                     {
                       Browser.task_win = wr;
                       Browser.task_expr =
                         (BrowserIO.Apply
                            ((BrowserIO.X (Browser.R v)),
                              (BrowserIO.X
                                 (Browser.R (Browser.Node_value dr)))))
                     } in
                   let n' = Browser.node_assoc_valid dr b in
                   let b' = Browser.node_update dr n' b in
                   let r =
                     {
                       running_state = b';
                       running_task_queue = (FStar_List_Tot_Base.map task vs)
                     } in
                   ((Running r), [])
                 else
                   ((Waiting b), [BrowserIO.UI_error " invalid text event"]))
          else ((Waiting b), [BrowserIO.UI_error "invalid win_ref"])
let (handle_click_button_event :
  Browser.win_ref ->
    Prims.nat ->
      Browser.browser -> (state * BrowserIO.output_event Prims.list))
  =
  fun wr ->
    fun but_pos ->
      fun b ->
        if Browser.win_valid wr b
        then
          match Browser.button_handlers_in_pos wr but_pos b with
          | FStar_Pervasives_Native.None -> ((Waiting b), [])
          | FStar_Pervasives_Native.Some (dr, vs) ->
              let task v =
                {
                  Browser.task_win = wr;
                  Browser.task_expr =
                    (BrowserIO.Apply
                       ((BrowserIO.X (Browser.R v)),
                         (BrowserIO.X (Browser.R (Browser.Node_value dr)))))
                } in
              let r =
                {
                  running_state = b;
                  running_task_queue = (FStar_List_Tot_Base.map task vs)
                } in
              ((Running r), [])
        else ((Waiting b), [BrowserIO.UI_error "invalid win_ref"])
let (get_ready_exprs :
  Browser.win_ref ->
    Browser.browser -> (Browser.task Prims.list * Browser.browser))
  =
  fun wr ->
    fun b ->
      if
        (Browser.win_valid wr b) &&
          (Browser.page_valid (Browser.win_assoc_valid wr b).Browser.win_page
             b)
      then
        let task e = { Browser.task_win = wr; Browser.task_expr = e } in
        let w = Browser.win_assoc_valid wr b in
        let p = Browser.page_assoc_valid w.Browser.win_page b in
        let uu___ = Browser.split_queued_exprs p.Browser.page_script_queue in
        match uu___ with
        | (pes_ready, pes') ->
            let p' =
              {
                Browser.page_location = (p.Browser.page_location);
                Browser.page_document = (p.Browser.page_document);
                Browser.page_environment = (p.Browser.page_environment);
                Browser.page_script_queue = pes'
              } in
            let b' = Browser.page_update w.Browser.win_page p' b in
            ((FStar_List_Tot_Base.map task pes_ready), b')
      else ([], b)
let (handle_network_response_event :
  BrowserIO.net_connection ->
    BrowserIO.resp ->
      Browser.browser -> (state * BrowserIO.output_event Prims.list))
  =
  fun nc ->
    fun resp ->
      fun b ->
        let uu___ = nc in
        match uu___ with
        | (d, n) ->
            (match Browser.net_connection_domain_nth d n b with
             | FStar_Pervasives_Native.None -> ((Waiting b), [])
             | FStar_Pervasives_Native.Some (uri, dst) ->
                 let b' = Browser.net_connection_domain_remove_nth d n b in
                 let b'1 = Browser.upd_cookies d uri resp b' in
                 (match (dst, (resp.BrowserIO.resp_body)) with
                  | (Browser.Xhr_dst (pr, v), BrowserIO.Script_file e) ->
                      (match Browser.page_win pr b'1 with
                       | FStar_Pervasives_Native.Some w ->
                           let wr = w in
                           let t =
                             {
                               Browser.task_win = wr;
                               Browser.task_expr =
                                 (BrowserIO.Apply
                                    ((BrowserIO.X (Browser.R v)),
                                      (BrowserIO.X
                                         (Browser.R (Browser.Code_value e)))))
                             } in
                           let r =
                             { running_state = b'1; running_task_queue = [t]
                             } in
                           ((Running r), [])
                       | FStar_Pervasives_Native.None -> ((Waiting b), []))
                  | (Browser.Doc_dst wr, BrowserIO.Html_file docs) ->
                      if
                        (Browser.win_valid wr b'1) &&
                          (Browser.page_valid
                             (Browser.win_assoc_valid wr b'1).Browser.win_page
                             b'1)
                      then
                        let uw = Browser.win_to_user_window wr b'1 in
                        let w = Browser.win_assoc_valid wr b'1 in
                        let b'2 = Browser.page_remove w.Browser.win_page b'1 in
                        let nd_ref =
                          if
                            (FStar_List_Tot_Base.length
                               b'2.Browser.browser_nodes)
                              >= Prims.int_one
                          then
                            FStar_Pervasives_Native.fst
                              (FStar_List_Tot_Base.last
                                 b'2.Browser.browser_nodes)
                          else Prims.int_zero in
                        let uu___1 =
                          Browser.build_node_tree
                            (BrowserIO.Divi
                               (FStar_Pervasives_Native.None, docs)) nd_ref in
                        (match uu___1 with
                         | (dr, node_ext) ->
                             let new_nodes =
                               FStar_List_Tot_Base.op_At
                                 b'2.Browser.browser_nodes node_ext in
                             let b'3 =
                               if Browser.b_node_pred new_nodes
                               then
                                 {
                                   Browser.browser_windows =
                                     (b'2.Browser.browser_windows);
                                   Browser.browser_pages =
                                     (b'2.Browser.browser_pages);
                                   Browser.browser_nodes = new_nodes;
                                   Browser.browser_environments =
                                     (b'2.Browser.browser_environments);
                                   Browser.browser_cookies =
                                     (b'2.Browser.browser_cookies);
                                   Browser.browser_connections =
                                     (b'2.Browser.browser_connections)
                                 }
                               else failwith "b_node_pred is not satisfied" in
                             let uu___2 =
                               Browser.build_win w.Browser.win_name
                                 (BrowserIO.Http_url (d, uri))
                                 w.Browser.win_opener
                                 (FStar_Pervasives_Native.Some dr) b'3 in
                             (match uu___2 with
                              | (w', b'4) ->
                                  let b'5 = Browser.win_update wr w' b'4 in
                                  let rd_opt =
                                    Browser.render_page w'.Browser.win_page
                                      b'4 in
                                  let oe =
                                    BrowserIO.UI_page_loaded_event
                                      (uw, (BrowserIO.Http_url (d, uri)),
                                        rd_opt) in
                                  let uu___3 =
                                    Browser.process_node_scripts
                                      w'.Browser.win_page dr b'5 in
                                  (match uu___3 with
                                   | (b'6, oes1, ts) ->
                                       let r =
                                         {
                                           running_state = b'6;
                                           running_task_queue = ts
                                         } in
                                       ((Running r),
                                         (FStar_List_Tot_Base.op_At [oe] oes1)))))
                      else ((Waiting b'1), [])
                  | (Browser.Doc_dst wr, uu___1) ->
                      if
                        (Browser.win_valid wr b'1) &&
                          (Browser.page_valid
                             (Browser.win_assoc_valid wr b'1).Browser.win_page
                             b'1)
                      then
                        let uw = Browser.win_to_user_window wr b'1 in
                        let w = Browser.win_assoc_valid wr b'1 in
                        let b'3 = Browser.page_remove w.Browser.win_page b'1 in
                        let uu___2 =
                          Browser.build_win w.Browser.win_name
                            (BrowserIO.Http_url (d, uri))
                            w.Browser.win_opener FStar_Pervasives_Native.None
                            b'3 in
                        (match uu___2 with
                         | (w', b'4) ->
                             let b'5 = Browser.win_update wr w' b'4 in
                             let rd_opt =
                               Browser.render_page w'.Browser.win_page b'5 in
                             let oe =
                               BrowserIO.UI_page_loaded_event
                                 (uw, (BrowserIO.Http_url (d, uri)), rd_opt) in
                             ((Waiting b'5), [oe]))
                      else ((Waiting b'1), [])
                  | (Browser.Script_dst (pr, dr), BrowserIO.Script_file e) ->
                      if Browser.page_valid pr b'1
                      then
                        (match Browser.page_win pr b'1 with
                         | FStar_Pervasives_Native.Some w ->
                             let wr = w in
                             let p = Browser.page_assoc_valid pr b'1 in
                             let q = p.Browser.page_script_queue in
                             let q' =
                               let upd p1 =
                                 match p1 with
                                 | Browser.Unknown_expr dr1 ->
                                     if dr1 = dr
                                     then
                                       Browser.Known_expr
                                         (Browser.to_inner_expr e)
                                     else p1
                                 | uu___1 -> p1 in
                               FStar_List_Tot_Base.map upd q in
                             let p' =
                               {
                                 Browser.page_location =
                                   (p.Browser.page_location);
                                 Browser.page_document =
                                   (p.Browser.page_document);
                                 Browser.page_environment =
                                   (p.Browser.page_environment);
                                 Browser.page_script_queue = q'
                               } in
                             let b'2 = Browser.page_update pr p' b'1 in
                             let uu___1 = get_ready_exprs wr b'2 in
                             (match uu___1 with
                              | (ts, b'3) ->
                                  let r =
                                    {
                                      running_state = b'3;
                                      running_task_queue = ts
                                    } in
                                  ((Running r), []))
                         | FStar_Pervasives_Native.None ->
                             ((Waiting b'1), []))
                      else failwith "page not in browser"
                  | uu___1 -> ((Waiting b'1), [])))
let (receive :
  BrowserIO.input_event ->
    waiting -> (state * BrowserIO.output_event Prims.list))
  =
  fun ie ->
    fun w ->
      match ie with
      | BrowserIO.User_load_in_new_win_event u ->
          handle_load_in_new_win_event Browser.No_opener Browser.No_name u w
      | BrowserIO.User_load_in_win_event (uw, u) ->
          (match Browser.win_from_user_window uw w with
           | FStar_Pervasives_Native.None -> ((Waiting w), [])
           | FStar_Pervasives_Native.Some wr ->
               handle_load_in_win_event wr u w)
      | BrowserIO.User_link_to_new_win_event (uw, u) ->
          let wo =
            match Browser.win_from_user_window uw w with
            | FStar_Pervasives_Native.None -> Browser.No_opener
            | FStar_Pervasives_Native.Some wr -> Browser.Win_opener wr in
          handle_load_in_new_win_event wo Browser.No_name u w
      | BrowserIO.User_link_to_named_win_event (uw, str, u) ->
          let wo =
            match Browser.win_from_user_window uw w with
            | FStar_Pervasives_Native.None -> Browser.No_opener
            | FStar_Pervasives_Native.Some wr -> Browser.Win_opener wr in
          (match Browser.win_from_win_name str w with
           | FStar_Pervasives_Native.None ->
               handle_load_in_new_win_event wo (Browser.Str_name str) u w
           | FStar_Pervasives_Native.Some wr ->
               handle_load_in_win_event wr u w)
      | BrowserIO.User_close_win_event uw ->
          (match Browser.win_from_user_window uw w with
           | FStar_Pervasives_Native.None -> ((Waiting w), [])
           | FStar_Pervasives_Native.Some wr -> handle_close_win_event wr w)
      | BrowserIO.User_input_text_event ((uw, box_pos), str) ->
          (match Browser.win_from_user_window uw w with
           | FStar_Pervasives_Native.None -> ((Waiting w), [])
           | FStar_Pervasives_Native.Some wr ->
               handle_input_text_event wr box_pos str w)
      | BrowserIO.User_click_button_event (uw, but_pos) ->
          (match Browser.win_from_user_window uw w with
           | FStar_Pervasives_Native.None -> ((Waiting w), [])
           | FStar_Pervasives_Native.Some wr ->
               handle_click_button_event wr but_pos w)
      | BrowserIO.Network_response_event (nr, resp) ->
          handle_network_response_event nr resp w
let (continue : running -> (state * BrowserIO.output_event Prims.list)) =
  fun r ->
    let current t = Browser.win_valid t.Browser.task_win r.running_state in
    match r.running_task_queue with
    | [] -> ((Waiting (r.running_state)), [])
    | t::ts ->
        if Prims.op_Negation (current t)
        then
          let r' =
            { running_state = (r.running_state); running_task_queue = ts } in
          ((Running r'), [])
        else
          (match t.Browser.task_expr with
           | BrowserIO.X (Browser.R (Browser.Error msg)) ->
               let r' =
                 { running_state = (r.running_state); running_task_queue = ts
                 } in
               ((Running r'), [BrowserIO.UI_error msg])
           | BrowserIO.X (Browser.R uu___1) ->
               let r' =
                 { running_state = (r.running_state); running_task_queue = ts
                 } in
               ((Running r'), [])
           | uu___1 ->
               let b = r.running_state in
               let w = Browser.win_assoc_valid t.Browser.task_win b in
               if Browser.page_valid w.Browser.win_page b
               then
                 let p = Browser.page_assoc_valid w.Browser.win_page b in
                 let ctx =
                   {
                     Browser.context_win = (t.Browser.task_win);
                     Browser.context_act = (p.Browser.page_environment)
                   } in
                 let uu___2 = Browser.step_expr ctx b t.Browser.task_expr in
                 (match uu___2 with
                  | (b', e', oes, ts') ->
                      let r' =
                        {
                          running_state = b';
                          running_task_queue =
                            (FStar_List_Tot_Base.op_At
                               ({
                                  Browser.task_win = (t.Browser.task_win);
                                  Browser.task_expr = e'
                                } :: ts) ts')
                        } in
                      ((Running r'), oes))
               else failwith "page not valid")