%{
  let eof = ref false

  let reset () =
    eof := false

  let is_eof () =
    !eof

  type tree = {
      id : int;
      spaces : int;
      parent : tree option;
      mutable children : tree list;
      li : Md_ast.li_content list;
  }

  let make_tree make_node l : Md_ast.t =
    Printf.printf "=%d\n" (List.length l);
    let id = ref 0 in
    let of_elt ?parent (spaces,li) = {
       id = !id;
       li;
       spaces;
       parent;
       children = [];
    } in
    Printf.printf "#%d\n" !id;
    let root = of_elt (-1,[]) in
    let rec put_in_tree root node =
      let node' = of_elt ~parent:root node in
      Printf.printf "root:%d:%d\n" root.spaces node'.spaces;
      let is_same_space = root.spaces = node'.spaces in
      if is_same_space || List.length root.children = 0 then begin
        let root, node' =
          if is_same_space then begin match root.parent with
           | Some p ->
                   Printf.printf "root:parent:(#%d:%d)\n" p.id p.spaces;
                   Printf.printf "root:parent:put:(#%d,%d):(#%d:%d)\n" p.id p.spaces node'.id node'.spaces;
                   p, of_elt ~parent:p node
           | None ->
                   Printf.printf "root:put:(#%d:%d):(#%d:%d)\n" root.id root.spaces node'.id node'.spaces;
                   root, node'
          end else begin
            Printf.printf "root:put:(#%d:%d):(#%d:%d)\n" root.id root.spaces node'.id node'.spaces;
            root, node'
          end
        in
        root.children <- [node'] @ root.children;
        true
      end else begin
        List.exists (fun root -> put_in_tree root node) root.children
      end
    in
    List.iter (fun node ->  incr id; Printf.printf "#%d\n" !id; ignore (put_in_tree root node)) (l);
    let rec of_tree root =
      `li (root.li @
        (if List.length root.children > 0 then
          List.map
            (fun root ->
              (make_node [of_tree root]))
            (List.rev root.children)
        else []))
    in make_node (List.map of_tree (List.rev root.children))

  let make_ul_tree = make_tree (fun l -> `ul l)
  let make_ol_tree = make_tree (fun l -> `ol l)

%}
%token <string> P
%token <string> Strong
%token <string> Em
%token <int> HPrefix
%token <string> Code
%token <string * string> PreCode
%token <char> Char
%token EOF
%token <int> Space
%token <string> Underl
%token Hr
%token Ol
%token Ul
%token <string * string> A
%token <string * string> Img
%token Sep
%token Bq

%start markdown
%type <Md_ast.t list> markdown

%%

markdown:
  | EOF                   { eof := true; [] }
  | Sep                   { [] }
  | Space         	      { [] }
  | expr markdown         { $1 @ $2 }
  |                       { [] }
;

expr:
  | p                     { $1 }
  | hd                    { $1 }
  | ul                    { $1 }
  | ol                    { $1 }
  | hr                    { $1 }
  | img                   { $1 }
  | bq                    { $1 }
  | pre                   { $1 }
;

hr:
  | Hr                    { [`hr] }
;

img:
  | Img                   { ([`img $1] : Md_ast.t list) }
;

em:
  | Em                    { [`em [`pcdata $1]] }
;

strong:
  | Strong                { [`strong [`pcdata $1]] }
;

anchor:
  | A                     { ([`a $1] : Md_ast.p_content list) }
;

pre:
  | PreCode			      { [`pre [`code $1]] }
;

code:
  | Code			      { [`code ("",$1)] }
;

hd:
  | p_content Sep Underl  { [`h (1, $1)] }
  | HPrefix p_content     { [`h ($1,$2)]}
;

p:
  | p_next                { [`p $1] }
;
p_next:
  |           Sep         { [] }
  | p_content Sep p_next  { $1 @ [`pcdata " "] @ $3 }
  | p_content             { $1 }
;
p_content:
  |                       { [] }
  | Underl p_content      { (`pcdata $1::$2 : Md_ast.p_content list) }
  | em p_content          { ($1 @ $2 : Md_ast.p_content list) }
  | code p_content        { ($1 @ $2 : Md_ast.p_content list) }
  | anchor p_content      { ($1 @ $2 : Md_ast.p_content list) }
  | strong p_content      { ($1 @ $2 : Md_ast.p_content list) }
  | pcdata p_content      { ($1 @ $2 : Md_ast.p_content list) }
;

pcdata:
  | pcdata_content          { [`pcdata $1] }
;
pcdata_content:
  |                         { "" }
  | Char pcdata_content     { (String.make 1 $1)^$2 }
  | Space pcdata_content    { (String.make $1 ' ')^$2 }
  | P pcdata_content        { $1^$2 }
;

ol:
  | ol_next                 { [make_ol_tree $1] }
;
ol_next:
  |                              { [] }
  |        ol_li                 { [(0,$1)] }
  |        ol_li Sep             { [(0,$1)] }
  |        ol_li Sep ol_next     { [(0,$1)] @ $3 }
  |        ol_li Sep             { [(0,$1)] }
  | spaces ol_li Sep             { [($1,$2)] }
  | spaces ol_li Sep ol_next     { [($1,$2)] @ $4 }
  | spaces                       { [] }
;
ol_li:
  | Ol li_content                { $2 }
;

ul:
  | ul_next                      { [make_ul_tree $1] }
;
ul_next:
  |                              { [] }
  |        ul_li                 { [(0,$1)] }
  |        ul_li Sep             { [(0,$1)] }
  |        ul_li Sep ul_next     { [(0,$1)] @ $3 }
  |        ul_li Sep             { [(0,$1)] }
  | spaces ul_li Sep             { [($1,$2)] }
  | spaces ul_li Sep ul_next     { [($1,$2)] @ $4 }
  | spaces                       { [] }
;
ul_li:
  | Ul li_content                { $2 }
;

spaces:
  | Space spaces			{ 1 + $2 }
  | Space       			{ 1 }
  |             			{ 0 }
;
li_content:
  | ol                      { $1 }
  | ul                      { $1 }
  | bq                      { $1 }
  | p_content               { ($1 :> Md_ast.li_content list) }
;
li:
  | ol                      { `li $1 }
  | ul                      { `li $1 }
  | bq                      { `li $1 }
  | p_content               { `li ($1 :> Md_ast.li_content list) }
;

bq:
  | Bq expr                 { [`bq $2] }
;
