open Labels

type id = int;;

type value =
| Void 
| DV
| Num of int
| Bool of bool
| Id of id
| FacValue of facvalue
and facvalue = {
  fh : value;
  fl : value;
}
;;

type eventName = string;;

type var = string;;
type op = Plus | Minus | And | Or | Equals | NotEquals ;;
type exp = 
  | Val of value
  | Var of var 
  | Bop of op * exp * exp
  | NodeValue of exp
;;

type command = 
  | Skip
  | Seq of command * command
  | AssignVar of var * exp
  | AssignNode of id * exp
  | While of exp * command
  | If of exp * command * command
  | Output of label * exp
  | Declassify of var * int * exp             
  | CreateElement of var * id * exp 
  | AddHandler of string * var * eventName * var * command
  | Dispatch of var * eventName * exp
  | FacCmd of command * command
;;

(* var is the input to the event handler while command is the sequence of commands and pc is the label of the event handler *)
type handler = (var * command * label option);; 

type handlers = (handler list);; 

type eventHandlerMap = (eventName * handlers) list;;

type node = {
    nid : id;
    v : value ref;
    ehMap : eventHandlerMap ref;
    (* nlabel : label ref; *)
};;

(* Does value have a label? *)
type event = (id * eventName * value)

type labelContext = ((eventName * value) * label) list;;

type globalVariables = var list;;

(* type of DOM implementation *)
type domImpl = Low | SMS | Faceted | Tainted;;

type input = Input of event;;
type output = Out of int;;
(* type action = Output of int | Event of event;; *)

(* P - Producer, C - Consumer, LC - Local Consumer *)
type execState = C | P ;;

(* kappa in rules - make handlers command, pc list - apply var in command and then use in config *)
type config = 
| Config of ((command * label option) list) * execState * ((event * label option) list)
| SMEConfig of config * config;;
