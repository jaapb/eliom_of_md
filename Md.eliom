{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D
  open Html5_types
  open Md_lexer
  open Md_parser

  type t = [
    | `A of Html5_types.flow5_without_interactive
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
      pcdata v
    ]

  let make_pcdata = function
    | `pcdata s ->
        pcdata s

  let make_em cnt =
    em (List.map make_pcdata cnt)

  let make_strong cnt =
    strong (List.map make_pcdata cnt)

  let make_code cnt =
    let cls = fst cnt in
    let p = snd cnt in
    code ~a:[a_class [cls]] [pcdata p]

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
        pcdata cnt

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
        pcdata s
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
        pcdata s

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
    Parsing.set_trace true;
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

}}
