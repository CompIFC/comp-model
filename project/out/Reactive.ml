open Prims
type waiting = unit
type running = unit
type input_event = unit
type output_event = unit
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
  Obj.magic (fun uu___ -> failwith "Not yet implemented:start")
let (receive : input_event -> waiting -> (state * output_event Prims.list)) =
  fun uu___ -> fun uu___1 -> failwith "Not yet implemented:receive"
let (continue : running -> (state * output_event Prims.list)) =
  fun uu___ -> failwith "Not yet implemented:continue"