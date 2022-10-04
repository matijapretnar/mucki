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

let view_month ?case month = text (Model.month_string ?case month)
let view_month_year month = view_int (Model.month_year month)

let koncnica ednina dvojina trojina mnozina n =
  match n mod 100 with
  | 1 -> ednina
  | 2 -> dvojina
  | 3 | 4 -> trojina
  | _ -> mnozina

let koncnica_niza ednina dvojina trojina mnozina n =
  let str = "00000" ^ n in
  match String.sub str (String.length str - 3) 3 with
  | "001" -> ednina
  | "002" -> dvojina
  | "003" | "004" -> trojina
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
    | Model.Male -> ((if alive then "rgb(33, 208, 208)" else "gray"), male_image)
    | Model.Female _ ->
        ((if alive then "rgb(255, 159, 28)" else "gray"), female_image)
  in
  view_rich_string ~color ~strikethrough:(not alive) (emoji ^ cat.name)

let sort_cats = List.sort (fun cat1 cat2 -> compare cat1.Model.name cat2.name)

let rec view_cats ?show_still_alive cats =
  cats |> sort_cats
  |> List.map (fun cat -> elt "li" [ view_cat ?show_still_alive cat ])

and view_cat ?show_still_alive (cat : Model.cat) =
  let view_born =
    elt "small"
      [ text " roj. "; text (Model.month_string ~case:1 cat.month_born) ]
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
  let thousands = floor (log10 n /. 3.) -. 1. in
  let m = int_of_float (n /. (1000. ** thousands))
  and e = int_of_float thousands in
  if e >= 1 then
    if m >= 1000 then
      let o = m mod 1000 in
      let o =
        if o < 10 then Format.sprintf "00%d" o
        else if o < 100 then Format.sprintf "0%d" o
        else Format.sprintf "%d" o
      in
      Format.sprintf "%d,%s,%s" (m / 1000) o
        (String.concat "," (List.init e (fun _ -> "000")))
    else
      Format.sprintf "%d,%s" m
        (String.concat "," (List.init e (fun _ -> "000")))
  else string_of_int (int_of_float n)

let view_pyramid ?(by_month = false) parameters population months =
  let view_month month =
    let count =
      population
      |> List.filter (fun (generation : Model.generation) ->
             generation.month_born <= month
             && month
                <= Model.increase_month generation.month_born
                     parameters.Model.months_of_lifespan)
      |> Model.count_size
    in
    let females = int_of_float count.surviving_females
    and males = int_of_float count.surviving_males in
    let total = count.surviving_males +. count.surviving_females in

    let shelter_capacity = 200.
    and country_capacity = 400000.
    and continent_capacity = 7000000.
    and planet_capacity = 300000000. in
    let shelters = int_of_float ((total +. shelter_capacity) /. shelter_capacity)
    and countries =
      int_of_float ((total +. shelter_capacity) /. country_capacity)
    and continents =
      int_of_float ((total +. shelter_capacity) /. continent_capacity)
    and planets =
      int_of_float ((total +. shelter_capacity) /. planet_capacity)
    in
    let cats =
      if total < shelter_capacity then
        List.init females (fun _ -> female_image)
        @ List.init males (fun _ -> male_image)
        |> Names.premesaj |> String.concat ""
      else if total < country_capacity then
        List.init shelters (fun _ -> "🏨") |> String.concat ""
      else if total < continent_capacity then
        List.init countries (fun _ -> "🇸🇮") |> String.concat ""
      else if total < planet_capacity then
        List.init continents (fun _ -> "🇪🇺") |> String.concat ""
      else List.init planets (fun _ -> "🌍") |> String.concat ""
    in
    let display_total =
      Printf.sprintf "%s %s" (round total)
        (koncnica_niza "mačka" "mački" "mačke" "mačk" (round total))
      ^
      if total > planet_capacity then
        Printf.sprintf " oz. %d-krat toliko, kolikor je mačk na svetu" planets
      else if total > continent_capacity then
        Printf.sprintf " oz. %d-krat toliko, kolikor je mačk v Evropi"
          continents
      else if total > country_capacity then
        Printf.sprintf " oz. %d-krat toliko, kolikor je mačk v Sloveniji"
          countries
      else if total > shelter_capacity then
        Printf.sprintf " oz. %d %s Ljubljana" shelters
          (koncnica "zavetišče" "zavetišči" "zavetišča" "zavetišč" shelters)
      else ""
    in

    [
      elt "tr"
        [
          elt "th"
            [ (if by_month then view_month month else view_month_year month) ];
          elt "td" [ text display_total ];
        ];
      elt "tr" [ elt "td" ~a:[ attr "colspan" "2" ] [ text cats ] ];
    ]
  in

  elt "table" (List.concat_map view_month months)

let forward_button_label = function
  | Model.Introduction _ -> "Na družinsko drevo"
  | Model.FirstYearLitter { mating_months_left = _ :: _; _ } ->
      "Na naslednje leglo"
  | Model.FirstYearLitter { mating_months_left = []; _ } -> "Na povzetek"
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
              text "Ali vedno velja ";
              elt "strong" [ text "1 + 1 = 2" ];
              text "? Videli bomo, da v naravi ne! Nekoč sta bila: 1 mačka ";
              view_cat_name female;
              text " in 1 maček ";
              view_cat_name male;
              text ".";
            ];
          elt "p"
            ([
               text "Po ";
               text (string_of_int parameters.Model.months_of_gestation);
               text
                 (koncnica " mesecu" " mesecih" " mesecih" " mesecih"
                    parameters.months_of_gestation);
               view_text " na svet %s "
                 (koncnica "primijavka" "primijavkata" "primijavkajo"
                    "primijavka" parameters.kittens_per_litter);
               kittens_per_litter_dropdown parameters;
               view_text " %s: "
                 (koncnica "mucek" "mucka" "mucki" "muckov"
                    parameters.kittens_per_litter);
             ]
            @ view_list
                (view_cat_name ~show_still_alive:true)
                (sort_cats children)
            @ [ text ". " ]);
          elt "p"
            ([
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
            @ view_list
                (view_cat_name ~show_still_alive:false)
                (sort_cats children));
          elt "p"
            [
              text
                "Kot bomo videli, bo potomcev kmalu precej veliko, zato si \
                 narišimo kar njihovo drevo.";
            ];
        ]
  | Model.FirstYearLitter { cats; mating_months_left; first; _ } ->
      elt "p"
        [
          (if first then
           text "Pri drevesu bomo potomce risali pod njihovo mamo:"
          else text "");
          elt "ul" (view_cats cats);
          (match mating_months_left with
          | [] when first ->
              div
                [
                  view_text "Ker imajo mačke mladiče ";
                  litters_per_year_dropdown parameters;
                  text " na leto, je to zadnje leglo v tem letu.";
                ]
          | _ :: _ when first ->
              div
                [
                  view_text "Ker imajo mačke mladiče ";
                  litters_per_year_dropdown parameters;
                  text " na leto, to ni zadnje leglo v tem letu…";
                ]
          | [] ->
              text
                "Ker drevo postaja že malo nepregledno, raje poglejmo, koliko \
                 je bilo mačk v vsakem obdobju."
          | _ :: _ -> text "Tudi to še ni zadnje leglo v tem letu…");
        ]
  | Model.EndOfFirstYear population ->
      div
        [
          elt "p"
            [ text "Na začetku in ob vsakem leglu so bile številke sledeče:" ];
          view_pyramid ~by_month:true parameters population
            (Model.Month 0 :: Model.litter_months parameters (Model.Year 0));
        ]
  | Model.Over { year = Model.Year y; population } ->
      let months =
        List.init (y + 1) (fun y ->
            Model.add_year (Model.Year (y + 1)) (Model.Month 0))
      in
      let total = population |> Model.count_size |> Model.total in
      div
        [
          text
            "Če zgodbo nadaljujemo še nekaj let, pa dobimo še večje številke. ";
          text "Tako lahko zaključimo, da je ";
          elt "strong" [ view_text "1 + 1 = %s!" (round total) ];
          view_pyramid parameters population (Model.Month 0 :: months);
          view_text "Poleg vsega pa je v tem času poginilo tudi %s mladičkov. "
            (round (Model.dead_kittens parameters population));
          text
            "Najboljši način, da tako umirimo rast kot preprečimo veliko \
             nepotrebnih smrti, je sterilizacija. Preizkusite spodnje možnosti \
             in se sami prepričajte o tem, kakšen vpliv ima.";
          elt "table"
            [
              elt "tr"
                [
                  elt "th" [ text "po koliko letih začnemo s sterilizacijo" ];
                  elt "td"
                    [
                      int_dropdown ~default:parameters.spaying_started 0
                        parameters.years_of_simulation (fun spaying_started ->
                          Model.SetParameters
                            { parameters with spaying_started });
                    ];
                ];
              elt "tr"
                [
                  elt "th" [ text "delež steriliziranih mačk" ];
                  elt "td"
                    [
                      percentage_dropdown ~default:parameters.percentage_spayed
                        (fun percentage_spayed ->
                          Model.SetParameters
                            { parameters with percentage_spayed });
                    ];
                ];
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
              elt "tr"
                [
                  elt "th" [ text "število let opazovanja" ];
                  elt "td"
                    [
                      int_dropdown ~default:parameters.years_of_simulation 0 10
                        (fun years_of_simulation ->
                          Model.SetParameters
                            { parameters with years_of_simulation });
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
          class_ "button is-pulled-left is-secondary";
        ]
  and forward_button =
    input []
      ~a:
        [
          onclick (fun _ -> Model.Forward);
          type_button;
          value (forward_button_label model.stage);
          class_ "button is-pulled-right is-primary";
        ]
  in

  div
    ([ div ~a:[ class_ "content" ] [ view_stage model.parameters model.stage ] ]
    @ (if show_backward model then [ backward_button ] else [])
    @ if show_forward model then [ forward_button ] else [])
