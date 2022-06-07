open Vdom

let view_int n = text (Printf.sprintf "%d" n)
let string_of_percentage (Model.Percent p) = Printf.sprintf "%d %%" p
let view_percentage p = text (string_of_percentage p)
let view_label lbl view = div [ text (Printf.sprintf "%s: " lbl); view ]

let dropdown ?default options view (msg : 'a -> Model.msg) =
  let view_option v =
    if Some v = default then
      elt "option" ~a:[ attr "selected" "true" ] [ text (view v) ]
    else elt "option" [ text (view v) ]
  in
  div
    ~a:[ class_ "select"; onchange_index (fun i -> msg (List.nth options i)) ]
    [ elt "select" (List.map view_option options) ]

let int_dropdown ?default from_int to_int msg =
  let options = List.init (to_int - from_int + 1) (fun i -> from_int + i) in
  dropdown ?default options string_of_int msg

let percentage_dropdown ?default msg =
  let options = List.init 11 (fun i -> Model.Percent (10 * i)) in
  dropdown ?default options string_of_percentage msg

let view_population (model : Model.model) =
  let newborns = List.hd model.population in
  let females_surviving = newborns.females in
  let kittens_surviving = females_surviving + newborns.males in
  elt "tr"
    [
      elt "td" [ view_int (Model.active_females model) ];
      elt "td" [ view_int (Model.kittens_born model) ];
      elt "td" [ view_int kittens_surviving ];
      elt "td" [ view_int females_surviving ];
      elt "td" [ view_int (Model.population_size model) ];
    ]

let meseci =
  [
    "januar";
    "februar";
    "marec";
    "april";
    "maj";
    "junij";
    "julij";
    "avgust";
    "september";
    "oktober";
    "november";
    "december";
  ]

let mnozina ednina dvojina trojina mnozina n =
  match n mod 100 with
  | 1 -> ednina
  | 2 -> dvojina
  | 3 | 4 -> trojina
  | _ -> mnozina

let mesecev = mnozina "mesec" "meseca" "mesece" "mesecev"

let view_cat (cat : Model.cat) =
  let color = match cat.gender with Male -> "blue" | Female -> "red" in
  elt "span" ~a:[ style "color" color ] [ text cat.name ]

let rec view_list view_element = function
  | [] -> []
  | [ x ] -> [ view_element x ]
  | [ x; y ] -> [ view_element x; text " in "; view_element y ]
  | x :: xs -> view_element x :: text ", " :: view_list view_element xs

let view (model : Model.model) =
  let perioda =
    model.parameters.months_before_mature + model.parameters.months_of_gestation
  in
  let samec, samica, model = Model.parents model in
  let sinovi, hcere, model = Model.children model in
  let preziveli = Model.survivors model.parameters sinovi
  and prezivele = Model.survivors model.parameters hcere in
  let vnuki, vnukinje, model =
    Model.grandchildren (samica :: Model.sort_cats prezivele) model
  in
  let prezivele_vnukinje = Model.survivors model.parameters vnukinje in
  let pravnuki, pravnukinje, model =
    Model.grandchildren
      ((samica :: Model.sort_cats prezivele)
      @ Model.sort_cats prezivele_vnukinje)
      model
  in
  let prezivele_pravnukinje = Model.survivors model.parameters pravnukinje in
  let prapravnuki, _, _ =
    Model.grandchildren
      ((samica :: Model.sort_cats prezivele)
      @ Model.sort_cats prezivele_vnukinje
      @ Model.sort_cats prezivele_pravnukinje)
      model
  in
  div
    ~a:[ class_ "content" ]
    [
      elt "h2" [ text "Na začetku…" ];
      elt "p"
        [
          text "sta bila 2 potepuha: mačka ";
          view_cat samica;
          text " in njen izbranec ";
          view_cat samec;
          text ". Mačke so breje okoli ";
          int_dropdown ~default:model.parameters.months_of_gestation 1 12
            (fun months_of_gestation ->
              Model.SetParameters { model.parameters with months_of_gestation });
          text (mesecev model.parameters.months_of_gestation);
          text
            ". (Če želite, lahko to in druge številke v zgodbi nastavite na \
             svoje vrednosti.)";
        ];
      elt "h2"
        [
          text
            (Printf.sprintf "Čez %d %s…" model.parameters.months_of_gestation
               (mesecev model.parameters.months_of_gestation));
        ];
      elt "p"
        ([
           text "na svet primijavka ";
           int_dropdown ~default:model.parameters.kittens_per_litter 1 8
             (fun kittens_per_litter ->
               Model.SetParameters { model.parameters with kittens_per_litter });
           text "muckov, v povprečju ";
           percentage_dropdown
             ~default:model.parameters.percentage_of_female_kittens
             (fun percentage_of_female_kittens ->
               Model.SetParameters
                 { model.parameters with percentage_of_female_kittens });
           text
             (Printf.sprintf " samičk in %s samčkov: "
                (string_of_percentage
                   (Model.inverse_percentage
                      model.parameters.percentage_of_female_kittens)));
         ]
        @ view_list view_cat (Model.sort_cats (sinovi @ hcere))
        @ [
            text
              (Printf.sprintf ". Zdaj jih je že %d."
                 (2 + model.parameters.kittens_per_litter));
          ]);
      elt "h2"
        [
          (let months =
             model.parameters.months_of_gestation
             + model.parameters.months_before_mature
           in
           text (Printf.sprintf "Čez %d %s…" months (mesecev months)));
        ];
      text
        (mnozina "Naslednji " "Naslednja " "Naslednji " "Naslednjih "
           model.parameters.months_before_mature);
      int_dropdown ~default:model.parameters.months_before_mature 1 12
        (fun months_before_mature ->
          Model.SetParameters { model.parameters with months_before_mature });
      text
        (mnozina "mesec" "meseca" "meseci" "mesecev"
           model.parameters.months_before_mature);
      text " odraščanja ";
      text
        (mnozina "ni lahek" "nista lahka" "niso lahki" "ni lahkih"
           model.parameters.months_before_mature);
      text ", saj odraslost doživi ";
      percentage_dropdown
        ~default:
          model.parameters.percentage_of_kittens_who_survive_to_sexual_maturity
        (fun percentage_of_kittens_who_survive_to_sexual_maturity ->
          Model.SetParameters
            {
              model.parameters with
              percentage_of_kittens_who_survive_to_sexual_maturity;
            });
      text " muckov: ";
      elt "span" (view_list view_cat (Model.sort_cats (preziveli @ prezivele)));
      text ", ki pa ne počivajo…";
      elt "h2" [ text "Čez …" ];
      text "Tudi ";
      view_cat samica;
      text " ne počiva, zato so čez ";
      view_int model.parameters.months_of_gestation;
      text " ";
      text (mesecev model.parameters.months_of_gestation);
      text " tu spet novi mucki:";
      elt "ul"
        (List.map
           (fun (mother, grandchildren) ->
             elt "li"
               (view_cat mother :: text ": " :: view_list view_cat grandchildren))
           vnuki);
      text
        (Printf.sprintf " in čez %d mesec%s spet" perioda
           (mnozina "" "a" "e" "ev" perioda));
      elt "ul"
        (List.map
           (fun (mother, grandchildren) ->
             elt "li"
               (view_cat mother :: text ": " :: view_list view_cat grandchildren))
           pravnuki);
      text
        (Printf.sprintf " in čez %d mesec%s spet" perioda
           (mnozina "" "a" "e" "ev" perioda));
      elt "ul"
        (List.map
           (fun (mother, grandchildren) ->
             elt "li"
               (view_cat mother :: text ": " :: view_list view_cat grandchildren))
           prapravnuki);
      text
        "Da se ne bomo izgubili v imenih, si zapomnimo le številke. Zaenkrat \
         je situacija sledeča:";
      elt "table"
        ([
           elt "tr"
             [
               elt "th" [ text "Aktivne samice" ];
               elt "th" [ text "Rojeni mladički" ];
               elt "th" [ text "Preživeli mladički" ];
               elt "th" [ text "Preživele samičke" ];
               elt "th" [ text "Skupaj" ];
             ];
         ]
        @ List.map view_population (Model.short_history model));
      text "Če nadaljujemo z izračuni do";
      int_dropdown ~default:model.parameters.average_lifespan_of_a_feral_cat 1
        20 (fun average_lifespan_of_a_feral_cat ->
          Model.SetParameters
            { model.parameters with average_lifespan_of_a_feral_cat });
      text " let, ko naša ";
      view_cat samica;
      text " ostari, dobimo sledeče številke:";
      elt "table"
        ([
           elt "tr"
             [
               elt "th" [ text "Aktivne samice" ];
               elt "th" [ text "Rojeni mladički" ];
               elt "th" [ text "Preživeli mladički" ];
               elt "th" [ text "Preživele samičke" ];
               elt "th" [ text "Skupaj" ];
             ];
         ]
        @ List.map view_population (Model.rest_of_the_history model));
      text
        "Rast je od tam naprej malenkost počasnejša, saj se starejše mačke \
         upokojijo, vendar se to komaj opazi, saj je vmes že toliko drugih \
         mlajših.";
    ]
