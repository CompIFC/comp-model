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
      BrowserIO.url -> Browser.browser -> (state * output_event Prims.list))
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
    BrowserIO.url -> Browser.browser -> (state * output_event Prims.list))
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
  Browser.win_ref -> Browser.browser -> (state * output_event Prims.list)) =
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
      Prims.string -> Browser.browser -> (state * output_event Prims.list))
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
    Prims.nat -> Browser.browser -> (state * output_event Prims.list))
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