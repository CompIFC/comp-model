module  BrowserIO

(** Definitions of data exchanged between browser and the internet**)


(** The type of an fully qualified internet domain name. *)

type domain = list string
(** A sequence of subdomain identifiers with the TLD written first:
      for example, [\["edu"; "upenn"; "cis"\]]. *)


(** {2 URLs} *)

(** The type of a directory path, which may end with a directory name or a
    file name. *)

type path = list string
  (** A sequence of path elements:
      for example, [\["music"; "artists"; "Bauhaus.html"\]]. *)

    

// req_uri datatype(contain path and query string of the url)

type req_uri= {
    req_uri_path: path;
    req_uri_params: string; 
    (** The part of the URL after a [?]:
      for example, ["artist=bauhaus&song=slice+of+life"]. *)
}


// the type of URL

type url=
| Blank_url
    (** The URL of a blank browser window (typically written as
        [about:blank]). *)
  | Http_url of domain * req_uri
    (** A URL that begins with [http://]. *)



//datatypes for our scripting language

type typ=
    | Null_type:typ
    | Bool_type:typ
    | Int_type:typ
    | String_type:typ
    | Url_type:typ
    | Type_type:typ
    | Function_type:typ
    | Code_type:typ
    | Window_type:typ
    | Node_type:typ

(** A type for variables in the scripting environment. *)
type var = string


// expression 
(** A type for script expressions.  These contruct will generate a runtime error
    if they are applied to value of types other than those described in the
    documentation here. *)

type  expr (a:Type)=
    | X: a -> expr a
    (** Allows this data type to be extended with additional language constructs
        to be added, which will be needed for the internal language. *)
    | Null: expr a
    | Bool: bool -> expr a
    | Int: int -> expr a
    | String: string -> expr a
    | Url: url -> expr a
    | Types: typ-> expr a
    | Seq: expr a-> expr a -> expr a
    | If: expr a -> expr a -> expr a -> expr a
    | While: expr a -> expr a-> expr a

    // primary functions

    | Prim1: string -> expr a ->expr a

    | Prim2: string -> expr a -> expr a ->expr a

    | Alert: expr a -> expr a
    (** Sends a message to the user. *)

    |Function: var -> list var -> expr a -> expr a
     (** [Function(param, locals, body)] is a single-argument function, where
        [locals] are the additional variable names that will be used in the
        body. *)

    | Apply: expr a -> expr a -> expr a
    (** Applies a function to an argument. *)

    | Code: expr False -> expr a

    | Eval: expr a -> expr a

    | Var: var -> expr a

    | Set_var: var -> expr a-> expr a

    // Browser operations

    | Get_cookie: expr a ->  expr a-> expr a
    (** [Get_cookie(url, key)] gets the cookie value associated with [url].
        Returns null if there is no such cookie. *)
    | Set_cookie : expr a ->  expr a-> expr a-> expr a
    (** [Set_cookie(url, key, str)] updates (or creates) the cookie value
        associated with [url].  If [str] is [null], then the cookie is
        deleted. *)

   | Xhr: expr a ->  expr a-> expr a-> expr a
    (** [Xhr(url, msg, handler)] sends an HTTP request to [url] with the
        request body [msg].  If the response is a script and [handler] is a
        function, then the reponse will be supplied as a [Code] value to the
        handler when the response is received.  It is an error if [handler] is
        not a function.  If the response is not a script, the handler will be
        invoked with a [null] argument. *)


    // window operations

    | Self_win: expr a
        (** A reference to the statically enclosing window. *)
    | Named_win: expr a ->expr a
        (** [Named_win(str)] evaluates to the window whose string name is [str].
            It evalutes to [null] if there is no such window. *)

    | Open_win: expr a->expr a
        (** [Open_win(url)] causes [url] to be loaded in a new, unnamed window.  A
            reference to the window is returned.  (The document will not be loaded
            in the window while the current script is running.)  If [url] is [null],
            a new blank window will be opened.  Returns a reference to the opened
            window. *)
    | Open_named_win: expr a->expr a->expr a
        (** [Open_named_win(url, str)] causes [url] to be loaded in the window
            with the name [str].  If a window with name [str] does not exist, then
            it is created.  A reference to the window is returned.  (The document
            will not be loaded in the window while the current script is running.)
            If [url] is null and a window with that name exists, the window will not
            be navigated.  Returns a reference to the window. *)
    | Close_win: expr a-> expr a
        (** [Close_win(win)] closes [win].  It is a no-op if [win] is not open. *)
    | Navigate_win: expr a-> expr a-> expr a
        (** [Navigate_win(win, url)] causes [url] to be loaded in [win] and
            evaluates to [null].  ([win] will not be ready while the current script
            is running.)  It is an error if [win] is not open. *)

    | Is_win_closed: expr a-> expr a
        (** [Is_win_closed(win)] evaluates to false if [win] refers to an open
        * window and true otherwise. *)
    | Get_win_opener: expr a-> expr a
        (** [Get_win_opener(win)] evaluates to the "opener" of [win] or null if
            [win] has no opener. *)
    | Get_win_location: expr a-> expr a
        (** [Get_win_location(win)] evaluates to the URL that is loaded into the
            window [win].  It is an error if [win] is not open. *)

    | Get_win_name: expr a-> expr a
        (** [Get_win_name(win)] returns the string name of the window, or null if
            the window has no name.  It is an error if [win] is not open. *)
    | Set_win_name: expr a-> expr a-> expr a
        (** [Set_win_name(win, str)] set the name of [win] to [str], or removes the
            name if [str] is [null].  It evaluates to [null].  It is an error if
            [win] is not open. *)

    | Get_win_root_node: expr a-> expr a
        (** [Get_win_root_node(win)] gets the document root node of a window.  Returns
            null if the window has no document.  It is an error if [win] is not
            open. *)
    | Set_win_root_node: expr a-> expr a->expr a
        (** [Set_win_root_node(win, node)] updates the document root node of a
            window.  It evaluates to [null].  It is an error if [win] is not
            open. *)

    | Get_win_var:expr a-> var -> expr a
        (** [Get_win_var(win, var)] looks up a variable in a window's environment.  It
            is an error if [win] is not open. *)
    | Set_win_var: expr a -> var -> expr a-> expr a
        (** [Set_win_var(win, var, expr)] updates (or creates) a variable in a
            window's environment and evaluates to the new value.  It is an error if
            [win] is not open and ready. *)



    // node operations



    | New_node: expr a -> expr a
    (** [New_node(desc)] returns a new node of the type matching [desc], which
        should be one of the following: "para", "link", "textbox", "button",
        "inl_script", "rem_script", or "div".  It is an error if [desc] does not
        match one of these strings. *)
  | Get_node_type: expr a -> expr a
    (** [Get_node_type(node)] returns one of the strings "para", "link",
        "textbox", "button", "inl_script", "rem_script", or "div". *)

  | Get_node_contents: expr a-> expr a
    (** [Get_node_contents(node)] returns the contents of nodes that may have
        non-empty contents.  For "para", "link", and "button" nodes, this is a
        [String] value.  For "inl_script" nodes, this is a [Code] value.  It is
        an error if [node] is not one of these types. *)
  | Set_node_contents: expr a-> expr a->expr a
    (** [Set_node_contents(node, value)] updates the contents of nodes that may
        have non-empty contents.  For "para", "link", and "button" nodes,
        [value] should be a [String] value.  For "inl_script" nodes, [value]
        should be a [Code] value.  It is an error if [node] is not one of these
        types. *)
  | Get_node_attr: expr a-> expr a-> expr a
    (** [Get_node_attr(node, attr)] returns the attributes described by the
        string [attr] of the node [node].  All node types have an attribute
        "id", which can be a [Null] or [String] value.  "link" nodes have an
        "href" attribute, which is a [Url] value.  "textbox" nodes have a
        "value" attribute, which is a [String] value.  "rem_script" nodes have a
        "src" attribute, which is a [Url] value. *)
  | Set_node_attr: expr a-> expr a-> expr a-> expr a
    (** [Set_node_attr(node, attr, value)] updates the attributes described by
        the string [attr] of the node [node] to have the value [value].  See
        [Get_node_attr] for attribute descriptions. *)
  | Remove_handlers: expr a-> expr a
    (** [Remove_handlers(node)] removes all handlers from the "textbox" or
        "button" node [node].  It is an error if [node] is not a "textbox" or
        "button" node. *)
  | Add_handler: expr a-> expr a-> expr a
    (** [Add_handler(node, h)] adds the [Function] value [h] as a handler to the
        "textbox" or "button" node [node].  [h] will be applied to [node] itself
        at the time the handler is triggered. *)

  | Get_parent: expr a-> expr a
    (** [Get_parent(node)] returns the parent node of [node] if it has one or
        returns [null] if it does not (or if it is the root node of a page). *)
  | Get_child: expr a-> expr a -> expr a
    (** [Get_child(node, pos)] returns the child node of [node] in
        position [pos] (counting from 0), or [null] if [node] has less than
        [pos + 1] children.  It is an error if [node] is not a "div" node. *)
  | Insert_node: expr a-> expr a-> expr a-> expr a
    (** [Insert_node(parent, node, pos)] inserts [node] as the child at
        position [pos] of the "div" node [parent].  It is an error if [parent]
        is not a "div" node; it is an error if [node] is an ancestor of
        [parent]; it is an error if [parent] has less than [pos] nodes. *)
  | Remove_node: expr a-> expr a
    (** [Remove_node(node)] removes [node] (and therefore all of its
        descendents) from its parent node or removes directly from its enclosing
        window if it is a document root node. *)



(** {2 HTML documents} *)

(** A type to use for the [id] attribute of an HTML tag. *)

type elt_id= string

// HTML document syntax

type doc=
| Para of option elt_id * string
    (** [Para(id, text)] represents a block of text. *)
  | Link of option elt_id * url * string
    (** [Link(id, link_text, href)] represents to an HTML [a] tag. *)
  | Textbox of option elt_id * string
    (** [Textbox(id, contents)] represents a text input box. *)
  | Button of option elt_id * string
    (** [Button(id, button_text)] represents to an HTML button. *)
  | Inl_script of option elt_id * expr unit
    (** Represents a [script] tag with an inline script. *)
  | Rem_script of option elt_id * url
    (** Represents a [script] tag with a [src] attribute. *)
  | Divi of option elt_id * list doc
    (** Contains a sequence of subdocuments.  For simplicity, this is the only
        document structure with children. *)


(** {2 Network (HTTP) interface} *)

(** The type of an HTTP request with its headers.  We do not distinguish between
    GET and POST requests. *)
type req = {
  req_uri: req_uri;
  req_cookies: list (string * string);
  req_body: string;
}

(** The type of an HTTP response payload. *)
type file_data =
  | Empty_file
  | Html_file of list doc 
  | Script_file of expr unit

(** The type of an HTTP response with its headers.  Setting a cookie takes
    priority over deleting the same cookie. *)
type resp = {
  resp_del_cookies: list string;
  resp_set_cookies: list (string * string);
  resp_body: file_data;
}

// Network IO syntax

type cookies={
    cookie_list: list (string * string)
}

type cookie_updates={
    del_cookies: list string;
    set_cookies: list (string * string)
}

type request=
| Request: req_uri -> cookies -> string -> request

type network_ouput=
| Send: domain -> request -> network_ouput

type response=
| Doc_response: cookie_updates -> doc -> response

| Script_response: cookie_updates -> expr False -> response

type net_connection=
| Connection: domain -> nat -> net_connection

type network_input=
| Receive: net_connection  -> response -> network_input

// user IO syntax
(* start with number 1 to indicate 1st window *)
type user_window = url * nat

(** A type for identifying a text input box.  The integer refers to the position
    of the text box among all of the text boxes on a given page. *)
type user_textbox = user_window * int

(** A type for identifying a button.  The integer refers to the position of the
    button among all of the buttons on a given page. *)
type user_button = user_window * int

(** The type of input events that can trigger action in a browser. *)
type input_event =
  | User_load_in_new_win_event of url
    (** Represents the action of a user opening a new window at a the given
        URL. *)
  | User_load_in_win_event of user_window * url
    (** Represents the action of a user loading a new URL in a window.  The may
        be the result of typing in the address bar or of clicking on a link in
        the window's current page. *)
  | User_link_to_new_win_event of user_window * url
    (** Represents the action of a user loading a URL from one page in a new
        window.  This may be the result of a clicking on a link with a [target]
        attribute set to "_blank" or of clicking on a link in a special way
        (e.g., with the control key pressed). *)
  | User_link_to_named_win_event of user_window * string * url
    (** Represents the action of a user loading a URL from one page in a window
        with a particular name.  This may be the result of clicking on a link
        with a [target] attribute set to some particular name. *)
  | User_close_win_event of user_window
    (** Represents the action of a user closing a window. *)
  | User_input_text_event of user_textbox * string  
    (** Represents the action of a user updating the text in a text input
        box. *)
  | User_click_button_event of user_button
    (** Represents the action of a user clicking on a button in a page. *)
  | Network_response_event of net_connection * resp
    (** Represents the receipt of an HTTP response. *)


(** A type for the visible representation of a document node tree. *)

type rendered_doc =
 | Para_rendered of string
  | Link_rendered of url * string
  | Textbox_rendered of string
  | Button_rendered of string
  | Div_rendered of list rendered_doc 

(** The type of output events that a browser can generate. *)
type output_event =
  | UI_win_opened_event (* windows  open to the URL "about:blank" *)
    (** Represents the opening of a new window.  Windows always open with the
        initial location [about:blank], even if an HTTP request has been sent
        for a document that will eventually be loaded in the page. *)
  | UI_win_closed_event of user_window
    (** Represents the closing of a window. *)
  | UI_page_loaded_event of user_window * url * option rendered_doc
    (** Represents a new document being loaded in a window.  If the rendered
        document is [None], then the updated page has no visible contents. *)
  | UI_page_updated_event of user_window * option rendered_doc 
    (** Represents an update to the structure of a page.  The entire page is
        included in this event representation, even if the update only affected
        some portiion of the page. *)
  | UI_alert of string
    (** Represents a pop-up box with a message. *)
  | UI_error of string
    (** Represents an error event. *)
  | Network_send_event of domain * req
    (** Represents an HTTP request. *)



