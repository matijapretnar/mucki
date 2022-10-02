open Vdom

let view_int n = text (Printf.sprintf "%d" n)
let view_percentage p = text (Model.Percentage.to_string p)
let view_text fmt = Printf.ksprintf text fmt
let view_year (Model.Year y) = view_int (2021 + y)

let dropdown ?default options view (msg : 'a -> Model.msg) =
  let view_option v =
    if Some v = default then
      elt "option" ~a:[ attr "selected" "true" ] [ text (view v) ]
    else elt "option" [ text (view v) ]
  in
  div
    ~a:
      [
        class_ "select is-small";
        onchange_index (fun i -> msg (List.nth options i));
      ]
    [ elt "select" (List.map view_option options) ]

let int_dropdown ?default from_int to_int msg =
  let options = List.init (to_int - from_int + 1) (fun i -> from_int + i) in
  dropdown ?default options string_of_int msg

let percentage_dropdown ?default msg =
  let options = List.init 11 (fun i -> Model.Percentage.of_int (10 * i)) in
  dropdown ?default options Model.Percentage.to_string msg

let view_month month = text (Model.month_string month)
let view_month_year month = view_int (Model.month_year month)

let koncnica ednina dvojina trojina mnozina n =
  match n mod 100 with
  | 1 -> ednina
  | 2 -> dvojina
  | 3 | 4 -> trojina
  | _ -> mnozina

let mesecev n = "mesec" ^ koncnica "" "a" "e" "ev" n

let rec view_list view_element = function
  | [] -> []
  | [ x ] -> [ view_element x ]
  | [ x; y ] -> [ view_element x; text " in "; view_element y ]
  | x :: xs -> view_element x :: text ", " :: view_list view_element xs

let view_rich_string ~color ~strikethrough str =
  let a = [ style "color" color ] in
  let a =
    if strikethrough then style "text-decoration" "line-through" :: a else a
  in
  elt "span" ~a [ text str ]

let male_image = "😼"
let female_image = "😻"

let view_cat_name ?(show_still_alive = false) (cat : Model.cat) =
  let alive = cat.alive || show_still_alive in
  let color, emoji =
    match cat.gender with
    | Model.Male -> ((if alive then "blue" else "gray"), male_image)
    | Model.Female _ -> ((if alive then "red" else "gray"), female_image)
  in
  view_rich_string ~color ~strikethrough:(not alive) (emoji ^ cat.name)

let rec view_cats ?show_still_alive cats =
  cats
  |> List.sort (fun cat1 cat2 -> compare cat1.Model.name cat2.name)
  |> List.map (fun cat -> elt "li" [ view_cat ?show_still_alive cat ])

and view_cat ?show_still_alive (cat : Model.cat) =
  let view_born =
    elt "small" [ text " "; text (Model.month_string cat.month_born) ]
  in
  match cat.gender with
  | Model.Male -> elt "span" [ view_cat_name ?show_still_alive cat; view_born ]
  | Model.Female children ->
      elt "span"
        [
          view_cat_name ?show_still_alive cat;
          view_born;
          elt "ul" (view_cats children);
        ]

let round n =
  let n = float_of_int n in
  let k = 10. ** max 0. (ceil (log10 n) -. 2.0) in
  int_of_float k * int_of_float (Float.round (n /. k))

let view_pyramid view_month population months =
  let view_month month =
    let count =
      population
      |> List.filter (fun (generation : Model.generation) ->
             generation.month_born <= month)
      |> Model.count_size
    in
    let total = round (count.surviving_females + count.surviving_males) in

    let shelter_capacity = 200 in
    let cats =
      if total < shelter_capacity then
        List.init count.surviving_females (fun _ -> female_image)
        @ List.init count.surviving_males (fun _ -> male_image)
        |> Names.premesaj |> String.concat ""
      else
        let stevilo =
          (total / shelter_capacity)
          + if total mod shelter_capacity = 0 then 0 else 1
        in
        (List.init stevilo (fun _ -> "🏨") |> String.concat "")
        ^ Printf.sprintf " - %d %s Gmajnice" stevilo
            (koncnica "zavetišče" "zavetišči" "zavetišča" "zavetišč" stevilo)
    in

    elt "tr"
      [
        elt "td" [ view_month month ];
        elt "td" [ view_int (round total) ];
        elt "td" [ text cats ];
      ]
  in
  elt "table"
    (elt "tr"
       [ elt "th" [ text "mesec" ]; elt "th" [ text "skupaj" ]; elt "td" [] ]
    :: List.map view_month months)

let forward_button_label = function
  | Model.Introduction _ -> "Na družinsko drevo"
  | Model.FirstYearLitter { mating_months_left = _ :: _; _ } ->
      "Na naslednje leglo"
  | Model.FirstYearLitter { mating_months_left = []; _ } -> "Če povzamemo"
  | Model.EndOfFirstYear _ -> "Kaj pa po nekaj letih?"
  | Model.Over _ -> "Po nekaj letih"

let litters_per_year_dropdown parameters =
  dropdown ~default:parameters.Model.litters_per_year
    [ Model.One; Model.Two; Model.Three ]
    (function
      | Model.One -> "enkrat"
      | Model.Two -> "dvakrat"
      | Model.Three -> "trikrat")
    (fun litters_per_year ->
      Model.SetParameters { parameters with litters_per_year })

let kittens_per_litter_dropdown parameters =
  int_dropdown ~default:parameters.Model.kittens_per_litter 1 8
    (fun kittens_per_litter ->
      Model.SetParameters { parameters with kittens_per_litter })

let percentage_of_kittens_who_survive_to_sexual_maturity_dropdown parameters =
  percentage_dropdown
    ~default:
      parameters.Model.percentage_of_kittens_who_survive_to_sexual_maturity
    (fun percentage_of_kittens_who_survive_to_sexual_maturity ->
      Model.SetParameters
        { parameters with percentage_of_kittens_who_survive_to_sexual_maturity })

let view_stage parameters = function
  | Model.Introduction { female; male; children; _ } ->
      div
        [
          elt "p"
            [
              text
                "Ali je 1 + 1 vedno enako 2? V naravi ne! Nekoč sta bila 2 \
                 potepuha: mačka ";
              view_cat_name female;
              text " in njen izbranec ";
              view_cat_name male;
              text ". Mačke imajo mladiče ";
              litters_per_year_dropdown parameters;
              text
                " na leto. (Če želite, lahko to in druge številke v zgodbi \
                 nastavite na svoje vrednosti.)";
            ];
          elt "p"
            ([
               view_text "Po ";
               text (string_of_int parameters.months_of_gestation);
               text
                 (koncnica " mesecu" " mesecih" " mesecih" " mesecih"
                    parameters.months_of_gestation);
               view_text " na svet primijavka%s "
                 (koncnica "" "ta" "jo" "" parameters.kittens_per_litter);
               kittens_per_litter_dropdown parameters;
               view_text "muc%s:"
                 (koncnica "ek" "ka" "ki" "kov" parameters.kittens_per_litter);
             ]
            @ view_list (view_cat_name ~show_still_alive:true) children
            @ [
                text ". ";
                view_text "Naslednj%s %d mesec%s odraščanja %s, %s "
                  (koncnica "i" "a" "i" "ih" parameters.months_before_mature)
                  parameters.months_before_mature
                  (koncnica "" "a" "i" "ev" parameters.months_before_mature)
                  (koncnica "ni lahek" "nista lahka" "niso lahki" "ni lahkih"
                     parameters.months_before_mature)
                  (if
                   parameters
                     .percentage_of_kittens_who_survive_to_sexual_maturity
                   = Model.Percentage.of_int 100
                  then "a odraslost doživi"
                  else "zato odraslost doživi le");
                percentage_of_kittens_who_survive_to_sexual_maturity_dropdown
                  parameters;
                text "mladičkov:";
              ]
            @ view_list (view_cat_name ~show_still_alive:false) children);
          elt "p"
            [
              text
                "Da si bomo lažje predstavljali, si vse skupaj poglejmo še v \
                 obliki družinskega drevesa.";
            ];
        ]
  | Model.FirstYearLitter { cats; mating_months_left; _ } ->
      elt "p"
        [
          text "Pri drevesu je dovolj, da se osredotočimo le na samičke.";
          elt "ul" (view_cats cats);
          text
            (match mating_months_left with
            | [] ->
                "Ker drevo postaja že malo nepregledno, raje poglejmo, koliko \
                 je bilo mačk v vsakem obdobju."
            | _ :: _ -> "");
        ]
  | Model.EndOfFirstYear population ->
      div
        [
          view_pyramid view_month population
            (Model.Month 0 :: Model.litter_months parameters (Model.Year 0));
        ]
  | Model.Over { year = Model.Year y; population } ->
      let months =
        List.init y (fun y ->
            Model.add_year (Model.Year (y + 1)) (Model.Month 0))
        (* |> List.concat_map (Model.litter_months parameters) *)
      in
      div
        [
          text "Ker vidimo, da brez omejitev število mačk raste, po ";
          int_dropdown ~default:parameters.spaying_started 0
            parameters.years_of_lifespan (fun spaying_started ->
              Model.SetParameters { parameters with spaying_started });
          text " letih začnemo s sterilizacijo, ki doseže ";
          percentage_dropdown ~default:parameters.percentage_spayed
            (fun percentage_spayed ->
              Model.SetParameters { parameters with percentage_spayed });
          view_text " mačk.";
          (* view_text
             "Prej kot bi začeli, boljše bi bilo, saj je v vsem tem času \
              poginilo okoli %d mladičkov."
             (round (Model.dead_kittens parameters population)); *)
          view_pyramid
            (fun month -> view_month_year month)
            population (Model.Month 0 :: months);
          view_text
            "Če želite preizkušati različne scenarije, lahko spreminjate tudi \
             ostale parametre simulacije:";
          elt "table"
            [
              elt "tr"
                [
                  elt "th" [ text "število legel na leto" ];
                  elt "td" [ litters_per_year_dropdown parameters ];
                ];
              elt "tr"
                [
                  elt "th" [ text "število mladičev v leglu" ];
                  elt "td" [ kittens_per_litter_dropdown parameters ];
                ];
              elt "tr"
                [
                  elt "th" [ text "delež mladičev, ki doživi odraslost" ];
                  elt "td"
                    [
                      percentage_of_kittens_who_survive_to_sexual_maturity_dropdown
                        parameters;
                    ];
                ];
            ];
        ]

let show_backward (model : Model.model) =
  match model.history with [] -> false | _ :: _ -> true

let show_forward (model : Model.model) =
  match model.stage with Model.Over _ -> false | _ -> true

let view (model : Model.model) =
  let backward_button =
    input []
      ~a:
        [
          onclick (fun _ -> Model.Backward);
          type_button;
          value "Nazaj";
          class_ "button";
        ]
  and forward_button =
    input []
      ~a:
        [
          onclick (fun _ -> Model.Forward);
          type_button;
          value (forward_button_label model.stage);
          class_ "button";
        ]
  in

  div
    ((if show_backward model then [ backward_button ] else [])
    @ [
        div ~a:[ class_ "content" ] [ view_stage model.parameters model.stage ];
      ]
    @ if show_forward model then [ forward_button ] else [])
