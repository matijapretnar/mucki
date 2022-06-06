open Vdom

let view_int n = text (Printf.sprintf "%d" n)
let view_float x = text (Printf.sprintf "%f" x)
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

let float_dropdown ?default from_float to_float step msg =
  let length = int_of_float ((to_float -. from_float) /. step) in
  let options =
    List.init length (fun i -> from_float +. (float_of_int i *. step))
  in
  dropdown ?default options string_of_float msg

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

let view_generation (generation : Model.generation) =
  div
    [
      view_label "Age" (view_int generation.age);
      view_label "Fertile females" (view_int generation.fertile_females);
      view_label "Infertile females" (view_int generation.infertile_females);
      view_label "Males" (view_int generation.males);
    ]

let view_population (model : Model.model) =
  div
    [
      view_label "Active females" (view_int (Model.active_females model));
      view_label "Kittens born" (view_int (Model.kittens_born model));
      view_label "Total population" (view_int (Model.population_size model));
      elt "ul"
        (List.map
           (fun gen -> elt "li" [ view_generation gen ])
           model.population);
    ]

let view (model : Model.model) =
  div
    [
      input []
        ~a:
          [ onclick (fun _ -> Model.GrowPopulation); type_button; value "Grow" ];
      view_parameters model;
      elt "hr" [];
      view_population model;
    ]
