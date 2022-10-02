module Percentage : sig
  type t

  val of_int : int -> t
  val take_percentage : int -> t -> int
  val ratio : t -> t -> t
  val inverse : t -> t
  val to_string : t -> string
end = struct
  type t = Percent of int

  let of_int p = Percent p

  let round_div m n =
    let o = m mod n in
    if 2 * o <= n then m / n else (m / n) + 1

  let take_percentage n (Percent p) =
    (* let rec sample_bernoulli acc = function
         | 0 -> acc
         | n ->
             if Random.int 101 < p then sample_bernoulli (succ acc) (pred n)
             else sample_bernoulli acc (pred n)
       in *)
    round_div (n * p) 100

  let inverse (Percent p) = Percent (100 - p)
  let ratio (Percent p) (Percent q) = Percent (100 * p / q)
  let to_string (Percent p) = Printf.sprintf "%d %%" p
end

type year = Year of int

let next_year (Year y) = Year (succ y)

type month = Month of int

let increase_month (Month m) d = Month (m + d)
let add_year (Year y) (Month m) = Month (m + (12 * y))
let month_year (Month m) = 2020 + ((m + 12) / 12)

let month_string (Month m) =
  let month_names =
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
  in
  Printf.sprintf "%s %d"
    (List.nth month_names ((m + 12) mod 12))
    (month_year (Month m))

type litters_per_year = One | Two | Three

type parameters = {
  litters_per_year : litters_per_year;
  months_of_gestation : int;
  months_before_mature : int;
  years_of_lifespan : int;
  kittens_per_litter : int;
  percentage_of_female_kittens : Percentage.t;
  percentage_of_kittens_who_survive_to_sexual_maturity : Percentage.t;
  percentage_spayed : Percentage.t;
  spaying_started : int;
}

let default_parameters =
  {
    litters_per_year = Three;
    months_of_gestation = 2;
    months_before_mature = 4;
    years_of_lifespan = 4;
    kittens_per_litter = 6;
    percentage_of_female_kittens = Percentage.of_int 50;
    percentage_of_kittens_who_survive_to_sexual_maturity = Percentage.of_int 50;
    percentage_spayed = Percentage.of_int 30;
    spaying_started = 4;
  }

let mating_months = function
  | One -> [ Month 1 ]
  | Two -> [ Month 1; Month 8 ]
  | Three -> [ Month 1; Month 5; Month 8 ]

let litter_months parameters year =
  mating_months parameters.litters_per_year
  |> List.map (fun month ->
         add_year year (increase_month month parameters.months_of_gestation))

let is_sexually_active parameters mating_month month_born =
  increase_month month_born parameters.months_before_mature <= mating_month
  && mating_month
     <= increase_month month_born (12 * parameters.years_of_lifespan)

type count = {
  surviving_females : int;
  nonsurviving_females : int;
  surviving_males : int;
  nonsurviving_males : int;
}

type generation = { month_born : month; count : count }
type population = generation list

let active_females parameters mating_month (population : population) =
  population
  |> List.filter (fun generation ->
         is_sexually_active parameters mating_month generation.month_born)
  |> List.map (fun generation -> generation.count.surviving_females)
  |> List.map (fun females ->
         if mating_month < add_year (Year parameters.spaying_started) (Month 1)
         then females
         else
           Percentage.take_percentage females
             (Percentage.inverse parameters.percentage_spayed))
  |> List.fold_left ( + ) 0

let kittens_born parameters mating_month population =
  active_females parameters mating_month population
  * parameters.kittens_per_litter

let newborn_generation parameters mating_month population =
  let kittens = kittens_born parameters mating_month population in
  let females =
    Percentage.take_percentage kittens parameters.percentage_of_female_kittens
  in
  let males = kittens - females in
  let survivors =
    Percentage.take_percentage kittens
      parameters.percentage_of_kittens_who_survive_to_sexual_maturity
  in
  let surviving_females, surviving_males =
    if Random.bool () then
      let surviving_males =
        Percentage.take_percentage males
          parameters.percentage_of_kittens_who_survive_to_sexual_maturity
      in
      let surviving_females = survivors - surviving_males in
      (surviving_females, surviving_males)
    else
      let surviving_females =
        Percentage.take_percentage females
          parameters.percentage_of_kittens_who_survive_to_sexual_maturity
      in
      let surviving_males = survivors - surviving_females in
      (surviving_females, surviving_males)
  in
  let nonsurviving_females = females - surviving_females
  and nonsurviving_males = males - surviving_males in
  let month_born = increase_month mating_month parameters.months_of_gestation in
  {
    month_born;
    count =
      {
        surviving_females;
        nonsurviving_females;
        surviving_males;
        nonsurviving_males;
      };
  }

let empty_count =
  {
    surviving_females = 0;
    nonsurviving_females = 0;
    surviving_males = 0;
    nonsurviving_males = 0;
  }

let add_count count1 count2 =
  {
    surviving_females = count1.surviving_females + count2.surviving_females;
    nonsurviving_females =
      count1.nonsurviving_females + count2.nonsurviving_females;
    surviving_males = count1.surviving_males + count2.surviving_males;
    nonsurviving_males = count1.nonsurviving_males + count2.nonsurviving_males;
  }

let empty_generation month_born = { month_born; count = empty_count }

let population_after_mating parameters mating_month population =
  let newborns = newborn_generation parameters mating_month population in
  newborns :: population

let population_after_year parameters year population =
  mating_months parameters.litters_per_year
  |> List.map (add_year year)
  |> List.fold_left
       (fun population mating_month ->
         population_after_mating parameters mating_month population)
       population

type cat = { name : string; month_born : month; alive : bool; gender : gender }
and gender = Male | Female of cats
and cats = cat list

let rec filter_cat p cat =
  {
    cat with
    gender =
      (match cat.gender with
      | Male -> Male
      | Female children -> Female (filter_cats p children));
  }

and filter_cats p cats = cats |> List.filter p |> List.map (filter_cat p)

let pick_name unused all =
  match unused with
  | name :: unused -> (unused, name)
  | [] ->
      let m = List.length all in
      let i = Random.int m and j = Random.int m in
      ([], Printf.sprintf "%s-%s" (List.nth all i) (List.nth all j))

let rec new_cats unused all new_cat = function
  | 0 -> (unused, [])
  | n ->
      let unused, name = pick_name unused all in
      let cat = new_cat name in
      let unused, cats = new_cats unused all new_cat (n - 1) in
      (unused, cat :: cats)

let cat_population cats =
  let counter = Hashtbl.create 64 in
  let update month_born f =
    let current =
      Hashtbl.find_opt counter month_born |> Option.value ~default:empty_count
    in
    Hashtbl.replace counter month_born (f current)
  in
  let rec traverse_cats cats = List.iter traverse_cat cats
  and traverse_cat cat =
    match cat.gender with
    | Male when cat.alive ->
        update cat.month_born (fun count ->
            { count with surviving_males = succ count.surviving_males })
    | Male ->
        update cat.month_born (fun count ->
            { count with nonsurviving_males = succ count.nonsurviving_males })
    | Female children when cat.alive ->
        update cat.month_born (fun count ->
            { count with surviving_females = succ count.surviving_females });
        traverse_cats children
    | Female children ->
        update cat.month_born (fun count ->
            {
              count with
              nonsurviving_females = succ count.nonsurviving_females;
            });
        assert (children = [])
  in
  traverse_cats cats;
  counter |> Hashtbl.to_seq |> List.of_seq
  |> List.map (fun (month_born, count) -> { month_born; count })

let count_size (population : population) =
  population
  |> List.fold_left
       (fun count generation -> add_count count generation.count)
       empty_count

let total { surviving_males; surviving_females; _ } =
  surviving_males + surviving_females

let dead_kittens parameters population =
  let alive_cats = total (count_size population) in
  Percentage.take_percentage alive_cats
    (Percentage.ratio
       (Percentage.inverse
          parameters.percentage_of_kittens_who_survive_to_sexual_maturity)
       parameters.percentage_of_kittens_who_survive_to_sexual_maturity)

let rec mate_cats parameters (names : Names.t) mating_month population =
  population
  |> filter_cats (fun cat -> cat.alive)
  |> List.fold_left_map
       (fun names cat -> mate_cat parameters names mating_month cat)
       names

and mate_cat parameters names mating_month cat =
  match cat.gender with
  | Female children when cat.alive ->
      let names, new_children =
        if is_sexually_active parameters mating_month cat.month_born then
          let new_kittens =
            newborn_generation parameters mating_month
              [
                ({
                   month_born = cat.month_born;
                   count =
                     {
                       surviving_females = 1;
                       nonsurviving_females = 0;
                       surviving_males = 0;
                       nonsurviving_males = 0;
                     };
                 }
                  : generation);
              ]
          in
          let month_born = new_kittens.month_born
          and count = new_kittens.count in
          let female_names', surviving_females =
            new_cats names.female Names.zenska
              (fun name ->
                { name; month_born; alive = true; gender = Female [] })
              count.surviving_females
          and male_names', surviving_males =
            new_cats names.male Names.moska
              (fun name -> { name; month_born; alive = true; gender = Male })
              count.surviving_males
          in
          let female_names'', nonsurviving_females =
            new_cats female_names' Names.zenska
              (fun name ->
                { name; month_born; alive = false; gender = Female [] })
              count.nonsurviving_females
          and male_names'', nonsurviving_males =
            new_cats male_names' Names.moska
              (fun name -> { name; month_born; alive = false; gender = Male })
              count.nonsurviving_males
          in
          ( Names.{ female = female_names''; male = male_names'' },
            surviving_females @ surviving_males @ nonsurviving_females
            @ nonsurviving_males )
        else (names, [])
      in
      let names', children = mate_cats parameters names mating_month children in
      (names', { cat with gender = Female (new_children @ children) })
  | _ -> (names, cat)

type stage =
  | Introduction of { female : cat; male : cat }
  | FirstLitter of {
      female : cat;
      male : cat;
      children : cats;
      mating_month : month;
      mating_months_left : month list;
    }
  | FirstYearLitter of {
      mating_month : month;
      mating_months_left : month list;
      cats : cats;
    }
  | EndOfFirstYear of population
  | Over of { year : year; population : population }

let next_stage parameters names = function
  | Introduction { female; male } -> (
      match mating_months parameters.litters_per_year with
      | [] -> assert false
      | mating_month :: mating_months_left -> (
          let names, female = mate_cat parameters names mating_month female in
          match female.gender with
          | Female children ->
              ( names,
                FirstLitter
                  { female; male; children; mating_month; mating_months_left }
              )
          | _ -> assert false))
  | FirstLitter { female; male; mating_month; mating_months_left; _ } ->
      let cats = [ female; male ] in
      (names, FirstYearLitter { mating_month; mating_months_left; cats })
  | FirstYearLitter { mating_months_left = []; cats; _ } ->
      ( names,
        EndOfFirstYear
          (cat_population (filter_cats (fun cat -> cat.alive) cats)) )
  | FirstYearLitter
      { mating_months_left = mating_month :: mating_months_left; cats; _ } ->
      let names, cats = mate_cats parameters names mating_month cats in
      (names, FirstYearLitter { mating_month; mating_months_left; cats })
  | EndOfFirstYear population ->
      let new_year, new_population =
        List.init 5 (fun y -> Year (y + 1))
        |> List.fold_left
             (fun (_, population) year ->
               (year, population_after_year parameters year population))
             (Year 0, population)
      in
      (names, Over { year = new_year; population = new_population })
  | Over _ as stage -> (names, stage)

type model = {
  parameters : parameters;
  stage : stage;
  names : Names.t;
  history : stage list;
}

let init =
  let female_name, male_name, names = Names.initial in
  {
    parameters = default_parameters;
    stage =
      Introduction
        {
          female =
            {
              name = female_name;
              month_born = Month (-11);
              alive = true;
              gender = Female [];
            };
          male =
            {
              name = male_name;
              month_born = Month (-11);
              alive = true;
              gender = Male;
            };
        };
    history = [];
    names;
  }

let next_model model =
  let names, stage = next_stage model.parameters model.names model.stage in
  { model with stage; names; history = model.stage :: model.history }

let recompute_model model =
  List.fold_left
    (fun model () -> next_model model)
    { init with parameters = model.parameters }
    (List.init (List.length model.history) (fun _ -> ()))

type msg = SetParameters of parameters | Forward | Backward

let update (model : model) : msg -> model = function
  | SetParameters parameters -> recompute_model { model with parameters }
  | Forward -> next_model model
  | Backward -> (
      match model.history with
      | stage :: history -> { model with stage; history }
      | _ -> model)
