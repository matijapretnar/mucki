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
    | Model.Male when alive -> ("blue", male_image)
    | Model.Female _ when alive -> ("red", female_image)
    | _ -> ("gray", "☠️")
  in
  view_rich_string ~color ~strikethrough:(not alive) (emoji ^ cat.name)

let rec view_cats ?show_still_alive cats =
  cats
  |> List.sort (fun cat1 cat2 -> compare cat1.Model.name cat2.name)
  |> List.map (fun cat -> elt "li" [ view_cat ?show_still_alive cat ])

and view_cat ?show_still_alive (cat : Model.cat) =
  match cat.gender with
  | Model.Male -> view_cat_name ?show_still_alive cat
  | Model.Female children ->
      elt "span"
        [ view_cat_name ?show_still_alive cat; elt "ul" (view_cats children) ]

let view_pyramid population months =
  let view_month month =
    let count =
      population
      |> List.filter (fun (generation : Model.generation) ->
             generation.month_born <= month)
      |> Model.count_size
    in
    let total = count.surviving_females + count.surviving_males in

    let cats =
      if total < 100 then
        List.init count.surviving_females (fun _ -> female_image)
        @ List.init count.surviving_males (fun _ -> male_image)
        |> Imena.premesaj |> String.concat ""
      else "OGROMNO"
    in

    elt "tr"
      [
        elt "td" [ view_month month ];
        elt "td" [ view_int total ];
        elt "td" [ text cats ];
      ]
  in
  elt "table"
    (elt "tr"
       [ elt "th" [ text "mesec" ]; elt "th" [ text "skupaj" ]; elt "td" [] ]
    :: List.map view_month months)

let view_stage_title (parameters : Model.parameters) = function
  | Model.Introduction _ -> text "Na začetku"
  | Model.FirstLitter { mating_month; _ } ->
      view_month
        (Model.increase_month mating_month parameters.months_of_gestation)
  | Model.FirstYearLitter { mating_month; _ } ->
      view_month
        (Model.increase_month mating_month parameters.months_of_gestation)
  | Model.EndOfFirstYear _ -> text "Ob koncu prvega leta"
  | Model.EndOfOtherYears { year; _ } ->
      elt "span" [ text "Ob koncu leta "; view_year year ]

let view_stage parameters = function
  | Model.Introduction { female; male } ->
      div
        [
          elt "p"
            [
              text "Nekoč sta bila 2 potepuha: mačka ";
              view_cat_name female;
              text " in njen izbranec ";
              view_cat_name male;
              text ". Mačke imajo mladiče ";
              dropdown ~default:parameters.Model.litters_per_year
                [ Model.One; Model.Two; Model.Three ]
                (function
                  | Model.One -> "enkrat"
                  | Model.Two -> "dvakrat"
                  | Model.Three -> "trikrat")
                (fun litters_per_year ->
                  Model.SetParameters { parameters with litters_per_year });
              text
                " na leto. (Če želite, lahko to in druge številke v zgodbi \
                 nastavite na svoje vrednosti.)";
            ];
        ]
  | Model.FirstLitter { children; _ } ->
      elt "p"
        ([
           view_text "Po ";
           text (string_of_int parameters.months_of_gestation);
           text
             (koncnica " mesecu" " mesecih" " mesecih" " mesecih"
                parameters.months_of_gestation);
           view_text " na svet primijavka%s "
             (koncnica "" "ta" "jo" "" parameters.kittens_per_litter);
           int_dropdown ~default:parameters.kittens_per_litter 1 8
             (fun kittens_per_litter ->
               Model.SetParameters { parameters with kittens_per_litter });
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
               parameters.percentage_of_kittens_who_survive_to_sexual_maturity
               = Model.Percentage.of_int 100
              then "a odraslost doživi"
              else "zato odraslost doživi le");
            percentage_dropdown
              ~default:
                parameters.percentage_of_kittens_who_survive_to_sexual_maturity
              (fun percentage_of_kittens_who_survive_to_sexual_maturity ->
                Model.SetParameters
                  {
                    parameters with
                    percentage_of_kittens_who_survive_to_sexual_maturity;
                  });
            text
              "mladičkov. Poglejmo si, kakšno je videti družinsko drevo po \
               vsakem leglu.";
          ])
  | Model.FirstYearLitter { cats; _ } -> elt "ul" (view_cats cats)
  | Model.EndOfFirstYear population ->
      div
        [
          elt "p"
            [
              text
                "Če zgornje podatke povzamemo v tabeli, so številke v vsakem \
                 mesecu sledeče:";
            ];
          view_pyramid population
            (Model.Month 0 :: Model.litter_months parameters (Model.Year 0));
        ]
  | Model.EndOfOtherYears { year = Model.Year y; population } ->
      let months =
        List.init y (fun y -> Model.Year y)
        |> List.concat_map (Model.litter_months parameters)
      in
      view_pyramid population (Model.Month 0 :: months)

let view (model : Model.model) =
  div
    ~a:[ class_ "content" ]
    [
      input []
        ~a:
          [
            onclick (fun _ -> Model.Backward);
            type_button;
            value "Nazaj";
            class_ "button";
          ];
      input []
        ~a:
          [
            onclick (fun _ -> Model.Forward);
            type_button;
            value "Naprej";
            class_ "button";
          ];
      elt "h2" [ view_stage_title model.parameters model.stage ];
      view_stage model.parameters model.stage;
    ]

(* let view_cat (cat : Model.cat) =


   let view_summary history =
     let view_population (model : Model.model) =
       let newborns = List.hd model.population in
       let females_surviving = newborns.females in
       let kittens_surviving = females_surviving + newborns.males in
       elt "tr"
         [
           elt "td" [ view_month model.month ];
           elt "td" [ view_int (Model.active_females model) ];
           elt "td" [ view_int (Model.kittens_born model) ];
           elt "td" [ view_int kittens_surviving ];
           elt "td" [ view_int females_surviving ];
           elt "td" [ view_int (Model.population_size model) ];
         ]
     in
     elt "table"
       ([
          elt "tr"
            [
              elt "th" [ text "Mesec" ];
              elt "th" [ text "Aktivne samice" ];
              elt "th" [ text "Rojeni mladički" ];
              elt "th" [ text "Preživeli mladički" ];
              elt "th" [ text "Preživele samičke" ];
              elt "th" [ text "Skupaj" ];
            ];
        ]
       @ List.map view_population history)

   let view_descendants descendants =
     elt "ol"
       (List.map
          (fun (mother, grandchildren) ->
            elt "li"
              (view_cat mother :: text ": " :: view_list view_cat grandchildren))
          descendants)

   let view (model : Model.model) =
     let perioda =
       parameters.months_before_mature + parameters.months_of_gestation
     in
     let samec, samica, model = Model.parents model in
     let sinovi, hcere, model = Model.children model in
     let preziveli = Model.survivors parameters sinovi
     and prezivele = Model.survivors parameters hcere in
     let vnuki, vnukinje, model =
       Model.grandchildren (samica :: Model.sort_cats prezivele) model
     in
     let prezivele_vnukinje = Model.survivors parameters vnukinje in
     let pravnuki, pravnukinje, model =
       Model.grandchildren
         ((samica :: Model.sort_cats prezivele)
         @ Model.sort_cats prezivele_vnukinje)
         model
     in
     let prezivele_pravnukinje = Model.survivors parameters pravnukinje in
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
         elt "h2" [ view_month (perioda + parameters.months_of_gestation) ];
         text "Tudi ";
         view_cat samica;
         text " ni počivala, zato so tu spet novi mucki.";
         view_descendants vnuki;
         view_text " in čez %d mesec%s spet!" perioda
           (koncnica "" "a" "e" "ev" perioda);
         elt "h2" [ view_month (2 * perioda) ];
         view_descendants pravnuki;
         elt "h2" [ view_month (3 * perioda) ];
         view_descendants prapravnuki;
         view_text
           "Da se ne bomo izgubili v imenih, si poglejmo le številke na vsak%s %d \
            %s. "
           (koncnica "" "a" "e" "ih" perioda)
           perioda (mesecev perioda);
         text "Zaenkrat je situacija sledeča:";
         view_summary (Model.short_history model);
         text "Če nadaljujemo z izračuni do";
         int_dropdown ~default:parameters.average_lifespan_of_a_feral_cat 1
           20 (fun average_lifespan_of_a_feral_cat ->
             Model.SetParameters
               { parameters with average_lifespan_of_a_feral_cat });
         text " let, ko naša ";
         view_cat samica;
         text " ostari, dobimo sledeče številke:";
         view_summary (Model.rest_of_the_history model);
         text
           "Rast je od tam naprej malenkost počasnejša, saj se starejše mačke \
            upokojijo, vendar se to komaj opazi, saj je vmes že toliko drugih \
            mlajših.";
       ] *)
