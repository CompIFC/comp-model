module Reactive

open BrowserIO
open Assumed
open Browser

(** The definition of a reactive system. *)

  assume new type waiting
  assume new type running
  assume new type input_event
  assume new type output_event

  noeq type state =
    | Waiting of waiting
    | Running of running

  assume new type start : state

  val receive: input_event -> waiting -> state * list (output_event)

  val continue: running -> state * list (output_event)