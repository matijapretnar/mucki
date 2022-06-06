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
      view_label "Months of gestation"
        (int_dropdown ~default:model.months_of_gestation 1 12
           (fun months_of_gestation ->
             Model.SetModel { model with months_of_gestation }));
      view_label "Months before mature"
        (int_dropdown ~default:model.months_before_mature 1 12
           (fun months_before_mature ->
             Model.SetModel { model with months_before_mature }));
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

let mesecev n =
  match n mod 100 with
  | 1 -> "mesec"
  | 2 -> "meseca"
  | 3 | 4 -> "mesece"
  | _ -> "mesecev"

let view (model : Model.model) =
  let samica = List.hd Imena.zenska and samec = List.hd Imena.moska in
  div
    ~a:[ class_ "content" ]
    [
      elt "h2" [ text "Na začetku…" ];
      elt "p"
        [
          text
            (Printf.sprintf
               "sta bila 2 potepuha: mačka %s in njen izbranec %s. " samica
               samec);
          text
            "Mačke so breje med XXX in YYY mesecev, pa recimo, da je AAA breja ";
          int_dropdown ~default:model.months_of_gestation 1 12
            (fun months_of_gestation ->
              Model.SetModel { model with months_of_gestation });
          text (mesecev model.months_of_gestation);
          text
            "(Če želite, lahko to in druge številke v zgodbi nastavite na \
             svoje vrednosti.)";
        ];
      elt "h2"
        [
          text
            (Printf.sprintf "Čez %d %s…" model.months_of_gestation
               (mesecev model.months_of_gestation));
        ];
      elt "p"
        [
          text "na svet primijavka ";
          int_dropdown ~default:model.kittens_per_litter 1 8
            (fun kittens_per_litter ->
              Model.SetModel { model with kittens_per_litter });
          text "muckov: AAA, BBB, CCC, DDD, EEE, FFF.";
          text
            (Printf.sprintf "Zdaj jih je že %d." (2 + model.kittens_per_litter));
        ];
      elt "h2"
        [
          (let months =
             model.months_of_gestation + model.months_before_mature
           in
           text (Printf.sprintf "Čez %d %s…" months (mesecev months)));
        ];
      text "Še ";
      int_dropdown ~default:model.months_before_mature 1 12
        (fun months_before_mature ->
          Model.SetModel { model with months_before_mature });
      text (mesecev model.months_before_mature);
      text
        " kasneje so tudi mladički pripravljeni na akcijo. no, ne vsi, \
         aktivnih je le %%% samičk. Poleg AAA še ... poelg). Tako se mesecev \
         AAA izležejo XYZ, BBB, ... Da se ne bomo izgubili v imenih, si \
         zapomnimo le številke. Zaenkrat je situacija sledeča:";
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
