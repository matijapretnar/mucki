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
  let options = List.init 101 (fun i -> Model.Percent i) in
  dropdown ?default options string_of_percentage msg

let view_parameters (model : Model.model) =
  div
    [
      view_label "Litters per year"
        (int_dropdown ~default:model.litters_per_year 1 4
           (fun litters_per_year ->
             Model.SetModel { model with litters_per_year }));
      view_label "Kittens per litter"
        (int_dropdown ~default:model.kittens_per_litter 1 8
           (fun kittens_per_litter ->
             Model.SetModel { model with kittens_per_litter }));
      view_label "Percentage of female kittens"
        (percentage_dropdown ~default:model.percentage_of_female_kittens
           (fun percentage_of_female_kittens ->
             Model.SetModel { model with percentage_of_female_kittens }));
      view_label "Percentage of kittens who survive to sexual maturity"
        (percentage_dropdown
           ~default:model.percentage_of_kittens_who_survive_to_sexual_maturity
           (fun percentage_of_kittens_who_survive_to_sexual_maturity ->
             Model.SetModel
               {
                 model with
                 percentage_of_kittens_who_survive_to_sexual_maturity;
               }));
      view_label "Percentage of fertile sexually mature females"
        (percentage_dropdown
           ~default:model.percentage_of_fertile_sexually_mature_females
           (fun percentage_of_fertile_sexually_mature_females ->
             Model.SetModel
               { model with percentage_of_fertile_sexually_mature_females }));
      view_label "Average lifespan of a feral cat"
        (int_dropdown ~default:model.average_lifespan_of_a_feral_cat 1 20
           (fun average_lifespan_of_a_feral_cat ->
             Model.SetModel { model with average_lifespan_of_a_feral_cat }));
      view_label "Years to project"
        (int_dropdown ~default:model.years_to_project 1 20
           (fun years_to_project ->
             Model.SetModel { model with years_to_project }));
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

let view (model : Model.model) =
  div
    [
      input []
        ~a:
          [ onclick (fun _ -> Model.GrowPopulation); type_button; value "Grow" ];
      view_parameters model;
      elt "hr" [];
      elt "table"
        ([
           elt "tr" [];
           elt "th" [ text "Active females" ];
           elt "th" [ text "Kittens born" ];
           elt "th" [ text "Kittens surviving" ];
           elt "th" [ text "Females surviving" ];
           elt "th" [ text "Fertile females" ];
           elt "th" [ text "Deaths" ];
           elt "th" [ text "Total population" ];
         ]
        @ List.map view_population (Model.history model));
    ]
