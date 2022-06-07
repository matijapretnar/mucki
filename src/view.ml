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

let view_parameters (parameters : Model.parameters) =
  div
    [
      view_label "Percentage of female kittens"
        (percentage_dropdown ~default:parameters.percentage_of_female_kittens
           (fun percentage_of_female_kittens ->
             Model.SetParameters
               { parameters with percentage_of_female_kittens }));
      view_label "Percentage of kittens who survive to sexual maturity"
        (percentage_dropdown
           ~default:
             parameters.percentage_of_kittens_who_survive_to_sexual_maturity
           (fun percentage_of_kittens_who_survive_to_sexual_maturity ->
             Model.SetParameters
               {
                 parameters with
                 percentage_of_kittens_who_survive_to_sexual_maturity;
               }));
      view_label "Percentage of fertile sexually mature females"
        (percentage_dropdown
           ~default:parameters.percentage_of_fertile_sexually_mature_females
           (fun percentage_of_fertile_sexually_mature_females ->
             Model.SetParameters
               { parameters with percentage_of_fertile_sexually_mature_females }));
      view_label "Average lifespan of a feral cat"
        (int_dropdown ~default:parameters.average_lifespan_of_a_feral_cat 1 20
           (fun average_lifespan_of_a_feral_cat ->
             Model.SetParameters
               { parameters with average_lifespan_of_a_feral_cat }));
    ]

let view_population (model : Model.model) =
  let newborns = List.hd model.population in
  let fertile_females = newborns.fertile_females in
  let females_surviving = fertile_females + newborns.infertile_females in
  let kittens_surviving = females_surviving + newborns.males in
  elt "tr"
    [
      elt "td" [ view_int (Model.active_females model) ];
      elt "td" [ view_int (Model.kittens_born model) ];
      elt "td" [ view_int kittens_surviving ];
      elt "td" [ view_int females_surviving ];
      elt "td" [ view_int fertile_females ];
      elt "td" [ view_int (Model.deaths model) ];
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
  let samec, samica, model = Model.parents model in
  let sinovi, hcere, model = Model.children model in
  let vnuki, model =
    Model.grandchildren (samica :: Model.sort_cats hcere) model
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
      text "Še ";
      int_dropdown ~default:model.parameters.months_before_mature 1 12
        (fun months_before_mature ->
          Model.SetParameters { model.parameters with months_before_mature });
      text (mesecev model.parameters.months_before_mature);
      text " kasneje so tudi mladički pripravljeni na akcijo. ";
      text "No, ne vsi, aktivnih je le ";
      percentage_dropdown
        ~default:model.parameters.percentage_of_fertile_sexually_mature_females
        (fun percentage_of_fertile_sexually_mature_females ->
          Model.SetParameters
            {
              model.parameters with
              percentage_of_fertile_sexually_mature_females;
            });
      text "samičk. Poleg AAA še ... poelg). ";
      elt "ul"
        (List.map
           (fun (mother, grandchildren) ->
             elt "li"
               (view_cat mother :: text ": " :: view_list view_cat grandchildren))
           vnuki);
      text
        "Da se ne bomo izgubili v imenih, si zapomnimo le številke. Zaenkrat \
         je situacija sledeča:";
      elt "table"
        ([
           elt "tr" [];
           elt "th" [ text "Aktivne samice" ];
           elt "th" [ text "Rojeni mladički" ];
           elt "th" [ text "Preživeli mladički" ];
           elt "th" [ text "Preživele samičke" ];
           elt "th" [ text "Plodne samičke" ];
           elt "th" [ text "Smrti" ];
           elt "th" [ text "Velikost populacije" ];
         ]
        @ List.map view_population (Model.history model));
      text
        "Kot vidimo, se na vsakih XXX + YYY mesecev zgodba ponovi. Če \
         nadaljujemo z izračuni do XXX, ko naša AAA ostari, dobimo sledeče \
         številke:";
      elt "table"
        ([
           elt "tr" [];
           elt "th" [ text "Aktivne samice" ];
           elt "th" [ text "Rojeni mladički" ];
           elt "th" [ text "Preživeli mladički" ];
           elt "th" [ text "Preživele samičke" ];
           elt "th" [ text "Plodne samičke" ];
           elt "th" [ text "Smrti" ];
           elt "th" [ text "Velikost populacije" ];
         ]
        @ List.map view_population (Model.history model));
      text
        "Rast je od tam naprej malenkost počasnejša, saj se starejše mačke \
         upokojijo, vendar se to komaj opazi, saj je vmes že toliko drugih \
         mlajših.";
    ]
