module Top_level

open BrowserIO
open Assumed
open Browser

(** The definition of a reactive system. *)

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

  val receive: input_event -> waiting -> 
    All (state * list output_event) 
        (requires (fun h0 ->  True))
        (ensures (fun h0 r h1 -> True))

  val continue: running -> 
    All (state * list (output_event))
        (requires (fun h0 ->  True))
        (ensures (fun h0 r h1 -> True))