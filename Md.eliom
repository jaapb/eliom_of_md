[%%shared
  open Eliom_lib
  open Eliom_content
  open Html
  open Html.D
  open Html_types
  open Md_lexer
  open Md_parser
]

[%%client
  open Js_of_ocaml
  open Js_of_ocaml_lwt
]

[%%shared
  type t = [
    | `A of Html_types.flow5_without_interactive
    | `B
    | `Blockquote
    | `Br
    | `Code
    | `Div
    | `Em
    | `Embed
    | `H1
    | `H2
    | `H3
    | `H4
    | `H5
    | `H6
    | `Header
    | `Hgroup
    | `Hr
    | `Img
    | `Label
    | `Ol
    | `P
    | `PCDATA
    | `Pre
    | `Ruby
    | `Span
    | `Strong
    | `Ul
  ]

  let print =
    Printf.printf "%s%!\n"

  let print_tag tag v =
    Printf.printf "%s:%s%!\n" tag v

  let print_ctag tag v =
    Printf.printf "%s:%c%!\n" tag v

  let print_itag tag v =
    Printf.printf "%s:%d%!\n" tag v

  let print_utag tag =
    Printf.printf "%s%!\n" tag

  let eof = ref false

  let reset () =
    eof := false;
    Md_parser.reset ()

  let make_a (v,href) =
    Raw.a ~a:[a_href (Xml.uri_of_string href)] [
      txt v
    ]

  let make_pcdata = function
    | `pcdata s ->
        txt s

  let make_em cnt =
    em (List.map make_pcdata cnt)

  let make_strong cnt =
    strong (List.map make_pcdata cnt)

  let make_code cnt =
    let cls = fst cnt in
    let p = snd cnt in
    code ~a:[a_class [cls]] [txt p]

  let make_pre_content = function
    | `code cnt ->
        make_code cnt

  let rec make_p_content = function
    | `a cnt ->
        make_a cnt
    | `em cnt ->
        make_em cnt
    | `strong cnt ->
        make_strong cnt
    | `code cnt ->
        make_code cnt
    | `pcdata cnt ->
        txt cnt

  and make_pre cnt =
    pre (List.map make_pre_content cnt)

  let make_p cnt =
    p (List.map make_p_content cnt)

  let make_img (alt,src) =
    Raw.img ~src:(Xml.uri_of_string src) ~alt ()

  let make_h (lvl,(v : Md_ast.h_content list)) =
    let p = List.map make_p_content v in
    let h = match lvl with
      | 1 -> h1
      | 2 -> h2
      | 3 -> h3
      | 4 -> h4
      | 5 -> h5
      | 6 -> h6
      | _ -> h1
    in
    h p

  let make_bq ~to_eliom_node cnt =
    blockquote (List.map to_eliom_node cnt)

  let rec make_li_content ~to_eliom_node = function
    | `a cnt ->
        make_a cnt
    | `p cnt ->
        make_p cnt
    | `img cnt ->
        make_img cnt
    | `hr ->
        hr ()
    | `h cnt ->
        make_h cnt
    | `pcdata s ->
        txt s
    | `bq cnt ->
        make_bq ~to_eliom_node cnt
    | `ul cnt ->
        make_ul ~to_eliom_node cnt
    | `ol cnt ->
        make_ol ~to_eliom_node cnt
    | `em cnt ->
        make_em cnt
    | `pre cnt ->
        make_pre cnt
    | `code cnt ->
        make_code cnt
    | `strong cnt ->
        make_strong cnt

  and make_li ~to_eliom_node = function
    | `li cnt ->
        li (
          List.map
            (make_li_content ~to_eliom_node)
            (cnt)
        )

  and make_ul ~to_eliom_node cnt =
    ul (List.map (make_li ~to_eliom_node) cnt)

  and make_ol ~to_eliom_node cnt =
    ol (List.map (make_li ~to_eliom_node) cnt)

  let rec to_eliom_node node : [> t] elt = match node with
    | `hr ->
        hr ()
    | `h cnt ->
        make_h cnt
    | `a cnt ->
        make_a cnt
    | `img cnt ->
        make_img cnt
    | `p cnt ->
        make_p cnt
    | `bq cnt ->
        make_bq ~to_eliom_node cnt
    | `ul cnt ->
        make_ul ~to_eliom_node cnt
    | `ol cnt ->
        make_ol ~to_eliom_node cnt
    | `em cnt ->
        make_em cnt
    | `strong cnt ->
        make_strong cnt
    | `pre cnt ->
        make_pre cnt
    | `code cnt ->
        make_code cnt
    | `pcdata s ->
        txt s

  let to_eliom ast : [> t] elt list =
    List.map to_eliom_node ast

  let rec print_token = function
    | EOF -> eof := true; Printf.printf "EOF\n"
    | A (a,b) -> print_tag "A" (a^b)
    | Ol -> print_utag "Ol"
    | Ul -> print_utag "Ul"
    | Hr -> print_utag "Hr"
    | Em v -> print_tag "Em" v
    | Strong v -> print_tag "Strong" v
    | Img (a,b) -> print_tag "Img" (a^b)
    | Space n -> print_itag "Space" n
    | P v -> print_tag "P" v
    | Bq -> print_utag "Bq";
    | Sep -> print_utag "Sep"
    | HPrefix _ -> print_utag "HPrefix"
    | Code v -> print_tag "Code" v
    | PreCode (_,v) -> print_tag "Underl" v
    | Char v -> print_ctag "Char" v
    | Underl v -> print_tag "Underl" v

  let parse lexbuf =
    ignore (Parsing.set_trace true);
    let rec aux rl =
      if ((Md_parser.is_eof ())) then begin
        print_endline "eof";
        rl
      end else try
        let acc = (Md_parser.markdown Md_lexer.next_token lexbuf) in
        aux (rl @ acc)
      with
        | _ as exc ->
            print_endline (Printexc.to_string exc);
            rl
        | Parsing.Parse_error ->
            Printf.printf "parsing error\n";
            rl
    in aux []

  let lex_and_parse lexbuf =
    parse lexbuf

  let lex_and_parse_file file =
    reset ();
    let in_ = open_in file in
    let ret = lex_and_parse (Lexing.from_channel in_) in
    close_in in_;
    ret

  let lex_and_parse_string text =
    reset ();
    lex_and_parse (Lexing.from_string text)

  let from_string text =
    to_eliom (lex_and_parse_string text)

  let from_file file =
    to_eliom (lex_and_parse_file file)

  module Debug = struct
    let pretty_print lexbuf =
      while not !eof do
        print_token (Md_lexer.next_token lexbuf);
      done

    let pp = pretty_print

    let from_string text =
      pp (Lexing.from_string text)
  end

]

[%%client
  module C = struct
    let contains elt' cls =
      elt'##.classList##contains(Js.string cls) = Js._true

    let add elt' cls =
      elt'##.classList##add(Js.string cls)

    let remove elt' cls =
      elt'##.classList##remove(Js.string cls)
  end

  let get_from_dom ~from q =
    Js.Opt.case (from##querySelector(Js.string q))
      (fun () -> failwith "elt not found")
      (fun elt' -> elt')

  let get_preview_tab ~from () =
    get_from_dom ~from ".preview"

  let get_edit_tab ~from () =
    get_from_dom ~from ".edit"

  let get_preview_area ~from () =
    get_from_dom ~from ".preview-area"

  let get_edit_area ~from () =
    get_from_dom ~from ".edit-area"

  let get_edit_value ~from () =
    Js.to_string (Js.Unsafe.coerce (get_from_dom ~from ".edit-area textarea"))##value

  let is_preview elt' =
    C.contains elt' "preview"

  let is_edit elt' =
    C.contains elt' ".edit"

  let hide elt' =
    elt'##.style##.display := Js.string "none"

  let show elt' =
    elt'##.style##.display := Js.string "block"

  let removeChildren elt' =
    List.iter
      (Dom.removeChild elt')
      (Dom.list_of_nodeList (elt'##.childNodes))

  let appendChildren elt' =
    List.iter (Dom.appendChild elt')

  let replaceChildren elt' nodes =
    removeChildren elt';
    appendChildren elt' nodes
]

let default_textarea () =
  Raw.textarea ~a:[a_placeholder "Enter some markdown here."] (txt "")

let create_editor_tab () =
  let create_tab ?(selected = false) ~cl text =
    li [
      Raw.a ~a:[
        a_tabindex (-1);
        a_href (uri_of_string (fun () -> "#"));
        a_class ["tab"; cl; (if selected then "selected" else "")];
      ] [
        txt text;
      ]
    ]
  in
  ul ~a:[a_class ["tab-bar"]] [
    create_tab ~selected:true ~cl:"edit" "Edit";
    create_tab ~cl:"preview" "Preview";
  ]

let create_editor ?(tarea = default_textarea) ?(attr = []) () =
  let editor =
    div ~a:(a_class ["md-editor"]::attr) [
      create_editor_tab ();
      div ~a:[a_class ["edit-area"]] [
        tarea ();
      ];
      div ~a:[a_class ["preview-area"]] [
      ];
    ]
  in
  ignore [%client
    (let from = To_dom.of_element ~%editor in
    let listen elt' =
      Lwt.async (fun () ->
        Lwt_js_events.clicks elt' (fun e _ ->
          let edit' = get_edit_area ~from () in
          let preview' = get_preview_area ~from () in
          if not (C.contains elt' "selected") then begin
            if is_preview elt' then begin
              C.remove (get_edit_tab ~from ()) "selected";
              C.add elt' "selected";
              hide edit';
              replaceChildren
                preview'
                (List.map To_dom.of_element (from_string (get_edit_value ~from ())));
              show preview';
            end else begin
              C.remove (get_preview_tab ~from ()) "selected";
              C.add elt' "selected";
              show edit';
              hide preview';
            end
          end;
          Dom.preventDefault e;
          Dom_html.stopPropagation e;
          Lwt.return ()
        );
      )
    in
    List.iter
      (listen)
      (Dom.list_of_nodeList (from##querySelectorAll(Js.string ".tab"))) : unit)
  ];
  editor
