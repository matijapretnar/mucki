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
        generation.males generation.females;
    ]

let view_population (population : Model.population) =
  elt "ul" (List.map view_generation population)

let view_cat_name =
  let view_name color name _month_born =
    elt "span" ~a:[ style "color" color ] [ text name ]
  in
  function
  | Model.Male { name; month_born } -> view_name "blue" name month_born
  | Model.Female { name; month_born; _ } -> view_name "red" name month_born

let rec view_cats cats = List.map (fun cat -> elt "li" [ view_cat cat ]) cats

and view_cat = function
  | Model.Male _ as cat -> view_cat_name cat
  | Model.Female { children; _ } as cat ->
      elt "span" [ view_cat_name cat; elt "ul" (view_cats children) ]

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
              dropdown ~default:Model.One
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
  | Model.FirstYear { mating_months_left; cats } ->
      let title =
        match mating_months_left with
        | [] -> text "Ob koncu leta"
        | mating_month :: _ -> view_month mating_month
      in
      div [ elt "h2" [ title ]; elt "ul" (view_cats cats) ]
  | Model.EndOfYear cats ->
      div [ elt "h2" [ text "In potem" ]; elt "ul" (view_cats cats) ]
  | Model.FurtherYears { year; population } ->
      div [ elt "h2" [ view_year year ]; view_population population ]

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

         elt "h2" [ view_month model.parameters.months_of_gestation ];
         elt "p"
           ([
              view_text "Kmalu na svet primijavka%s "
                (koncnica "" "ta" "jo" "" model.parameters.kittens_per_litter);
              int_dropdown ~default:model.parameters.kittens_per_litter 1 8
                (fun kittens_per_litter ->
                  Model.SetParameters { model.parameters with kittens_per_litter });
              view_text "muc%s, v povprečju "
                (koncnica "ek" "ka" "ki" "kov" model.parameters.kittens_per_litter);
              percentage_dropdown
                ~default:model.parameters.percentage_of_female_kittens
                (fun percentage_of_female_kittens ->
                  Model.SetParameters
                    { model.parameters with percentage_of_female_kittens });
              view_text " samičk in %s samčkov: "
                (string_of_percentage
                   (Model.inverse_percentage
                      model.parameters.percentage_of_female_kittens));
            ]
           @ view_list view_cat (Model.sort_cats (sinovi @ hcere)));
         elt "h2" [ view_month perioda ];
         text
           (koncnica "Naslednji " "Naslednja " "Naslednji " "Naslednjih "
              model.parameters.months_before_mature);
         int_dropdown ~default:model.parameters.months_before_mature 1 12
           (fun months_before_mature ->
             Model.SetParameters { model.parameters with months_before_mature });
         text
           (koncnica "mesec" "meseca" "meseci" "mesecev"
              model.parameters.months_before_mature);
         text " odraščanja ";
         text
           (koncnica "ni lahek" "nista lahka" "niso lahki" "ni lahkih"
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
         elt "h2" [ view_month (perioda + model.parameters.months_of_gestation) ];
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
         int_dropdown ~default:model.parameters.average_lifespan_of_a_feral_cat 1
           20 (fun average_lifespan_of_a_feral_cat ->
             Model.SetParameters
               { model.parameters with average_lifespan_of_a_feral_cat });
         text " let, ko naša ";
         view_cat samica;
         text " ostari, dobimo sledeče številke:";
         view_summary (Model.rest_of_the_history model);
         text
           "Rast je od tam naprej malenkost počasnejša, saj se starejše mačke \
            upokojijo, vendar se to komaj opazi, saj je vmes že toliko drugih \
            mlajših.";
       ] *)
