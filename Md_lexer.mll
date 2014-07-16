(** This lexer attempts to tokenize sections of an eliom file *)
{
  open Printf
  open Buffer
  open Md_parser

  exception Eof
  exception Empty

  let remove_bs s =
    let len = String.length s in
    let buf = Buffer.create len in
    let escaped = ref false in
    String.iter
      (fun c ->
        if !escaped then begin
          Buffer.add_char buf c;
          escaped := false;
        end else begin
          if c = '\\' then
             escaped := true
          else Buffer.add_char buf c;
        end)
      (s);
    Buffer.contents buf
}

let sep = '\n'

let escaped_char =
  ('\\' ('`' | '[' | ']' | '(' | ')' | '!' | '>' | '-' | '*' | '=' | '_' | '#'))

let basic_char =
  ['\\' '<' '?' 'a'-'z' 'A'-'Z' '/' ':' '&' '"' ''' ',' ';' '{' '}']

let default_char =
  (basic_char)

let code_char =
  (basic_char | '\\' '`' | ['\n' ' ' '\t' '>' '-' '+' '#' '=' '0'-'9'])

let ident_char =
  (default_char | '\\' default_char | escaped_char)

let ident_suffix_char =
  ['-' '_' '.' ' ' '\t' '>' '#' '=' '0'-'9']

let ident =
  ident_char (ident_suffix_char | '\\' ident_suffix_char | ident_char)*

let ident_num =
  (ident_char | ['0'-'9' '.']) (ident_suffix_char | '\\' ident_suffix_char | ident_char)*

let code =
  code_char*

let blank =
  [' ' '\t']

let blank_and_ident =
  blank* ident

let hr_ =
  ('-'|'+'|'*') (blank+)

let hr =
  hr_ hr_ (('-'|'+'|'*') (blank*))* sep

let u_s =
  ('_'|'*')

let hr_and_blank =
  hr blank

rule next_token = parse
  | ((' ' | '\t') as sp)
    {
      if sp = '\t' then
        Space 4
      else Space 1
    }
  | sep
    {
      Sep
    }
  | ((('-'|'=')('-'|'=')+) as v)
    {
      Underl v
    }
  | ('#'+ as prefix)
    {
      HPrefix (String.length prefix)
    }
  | (('!'|'('|')'|'['|']'|'`'|'.'|'+'|'-'|'='|'*'|'_'|'#') as c)
    {
      Char c
    }
  | '>'
    {
      Bq
    }
  | u_s (ident as idt) u_s
    {
      Em idt
    }
  | u_s u_s (ident as idt) u_s u_s
    {
      Strong idt
    }
  | ('`'+ as prefix) (['a'-'z' 'A'-'Z']* as kind) (sep? as sep)
       (code as code)
    ('`'+ as suffix)
    {
      let plen = String.length prefix in
      let slen = String.length suffix in
      let min = min plen slen in
      let prefix = String.sub prefix (min-1) (plen - min) in
      let suffix = String.sub suffix (min-1) (slen - min) in
      if String.length kind > 0 then begin
        if sep = "\n" then
          PreCode (kind,code)
        else
          Code (kind^code)
      end else Code (kind)
    }
  | hr
    {
      Hr
    }
  | ('!'? as bang) '[' (ident as idt) ']' '(' (('#'? ident) as href) ')'
    {
      if bang = "!" then
        Img (idt, href)
      else A (idt, href)
    }
  | ('-'|'+'|'*') (blank+)
    {
      Ul
    }
  | ['0'-'9']+ '.' (blank+)
    {
      Ol
    }
  | (['0'-'9']+ as nb)
    {
      P nb
    }
  | (ident as idt)
    {
      P (remove_bs idt)
    }
  | eof	{ EOF }
