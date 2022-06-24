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