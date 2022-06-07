

type running_browser={
      Running_task_list: list task;
      Running_browser: browser;
}

(* reactive system of browser *)

type consumerState={
    state: browser;
}

type producerState={
    state: running;
}


type input=
| User_input: user_input -> input

| Network_input: network_input -> input