[%%shared
  open Printf

  type pcdata = [
    | `pcdata of string
  ]
  and p_content = [
    | `a of (string * string)
    | `pcdata of string
    | `em of pcdata list
    | `strong of pcdata list
    | `code of (string * string)
  ]
  and h_content = p_content
  and bq_content = t
  and li_content = t
  and ol_content = [
    `li of li_content list
  ]
  and ul_content = [
    `li of li_content list
  ]
  and pre_content = [
    `code of (string * string)
  ]
  and t = [
    | `hr
    | `h of (int * h_content list)
    | `em of pcdata list
    | `strong of pcdata list
    | `a of (string * string)
    | `img of (string * string)
    | `p of p_content list
    | `bq of bq_content list
    | `ul of ul_content list
    | `ol of ol_content list
    | `code of (string * string)
    | `pre of pre_content list
    | `pcdata of string
  ]
]
