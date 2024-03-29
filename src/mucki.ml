let app =
  Vdom.simple_app
    ~init:(Model.init Model.default_parameters)
    ~view:View.view ~update:Model.update ()

let run () =
  Js_browser.Document.set_title Js_browser.document "Mačja piramida";
  Vdom_blit.run app |> Vdom_blit.dom
  |> Js_browser.Element.append_child
       (match
          Js_browser.Document.get_element_by_id Js_browser.document "container"
        with
       | Some element -> element
       | None -> Js_browser.Document.document_element Js_browser.document)

let () = Js_browser.Window.set_onload Js_browser.window run
