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

let view_generation (generation : Model.generation) =
  elt "li"
    [
      view_text "%s: %d M, %d Ž"
        (Model.month_string generation.month_born)
        generation.surviving_males generation.surviving_females;
    ]

let view_population (population : Model.population) =
  elt "ul" (List.map view_generation population)

let view_cat_name ?(show_still_alive = false) (cat : Model.cat) =
  let color =
    match cat.gender with Model.Male -> "blue" | Model.Female _ -> "red"
  in
  let a = [ style "color" color ] in
  let a =
    if cat.alive || show_still_alive then a
    else style "text-decoration" "line-through" :: a
  in
  elt "span" ~a [ text cat.name ]

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

let view_stage parameters = function
  | Model.Introduction { female; male } ->
      div
        [
          elt "h2" [ text "Na začetku" ];
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
  | Model.FirstYearLitter { first; mating_month; cats; _ } when first ->
      div
        [
          elt "h2" [ view_month mating_month ];
          elt "p"
            ([
               view_text "Kmalu na svet primijavka%s "
                 (koncnica "" "ta" "jo" "" parameters.kittens_per_litter);
               int_dropdown ~default:parameters.kittens_per_litter 1 8
                 (fun kittens_per_litter ->
                   Model.SetParameters { parameters with kittens_per_litter });
               view_text "muc%s."
                 (koncnica "ek" "ka" "ki" "kov" parameters.kittens_per_litter);
             ]
            @ view_list (view_cat ~show_still_alive:true) cats);
          text
            (koncnica "Naslednji " "Naslednja " "Naslednji " "Naslednjih "
               parameters.months_before_mature);
          int_dropdown ~default:parameters.months_before_mature 1 12
            (fun months_before_mature ->
              Model.SetParameters { parameters with months_before_mature });
          text
            (koncnica "mesec" "meseca" "meseci" "mesecev"
               parameters.months_before_mature);
          text " odraščanja ";
          text
            (koncnica "ni lahek" "nista lahka" "niso lahki" "ni lahkih"
               parameters.months_before_mature);
          text ", saj odraslost doživi ";
          percentage_dropdown
            ~default:
              parameters.percentage_of_kittens_who_survive_to_sexual_maturity
            (fun percentage_of_kittens_who_survive_to_sexual_maturity ->
              Model.SetParameters
                {
                  parameters with
                  percentage_of_kittens_who_survive_to_sexual_maturity;
                });
          text " muckov: ";
          elt "span" (view_list view_cat cats);
          text ", ki pa ne počivajo…";
        ]
  | Model.FirstYearLitter { cats; mating_month; _ } ->
      div
        [
          elt "h2" [ text "Ostala legla: "; view_month mating_month ];
          elt "ul" (view_cats cats);
        ]
  | Model.EndOfFirstYear cats ->
      div
        [ elt "h2" [ text "Na koncu prvega leta" ]; elt "ul" (view_cats cats) ]
  | Model.StartOfOtherYears { year; population } ->
      div
        [
          elt "h2" [ text "Na začetku leta "; view_year year ];
          view_population population;
        ]

let view (model : Model.model) =
  div
    ~a:[ class_ "content" ]
    (Model.history model.parameters model.stage 10
    |> List.map (view_stage model.parameters))

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
