{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D
}}

module App =
  Eliom_registration.App (
    struct
      let application_name = "md_editor"
    end)

let main_service =
  Eliom_service.App.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()

let () =
  App.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"A simple Markdown editor using Eliom."
           ~css:[
             ["css"; "md.css"];
             ["css"; "md_editor.css"];
           ]
           Html5.F.(body [
             Md.create_editor ();
           ])))
