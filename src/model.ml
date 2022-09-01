module Percentage : sig
  type t

  val of_int : int -> t
  val take_percentage : int -> t -> int
  val inverse : t -> t
  val to_string : t -> string
end = struct
  type t = Percent of int

  let of_int p = Percent p

  let round_div m n =
    let o = m mod n in
    if 2 * o <= n then m / n else (m / n) + 1

  let take_percentage n (Percent p) = round_div (n * p) 100
  let inverse (Percent p) = Percent (100 - p)
  let to_string (Percent p) = Printf.sprintf "%d %%" p
end

type year = Year of int

let next_year (Year y) = Year (succ y)

type month = Month of int

let increase_month (Month m) d = Month (m + d)
let add_year (Year y) (Month m) = Month (m + (12 * y))

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
    (2020 + ((m + 12) / 12))

type litters_per_year = One | Two | Three

type parameters = {
  litters_per_year : litters_per_year;
  months_of_gestation : int;
  months_before_mature : int;
  months_of_lifespan : int;
  kittens_per_litter : int;
  percentage_of_female_kittens : Percentage.t;
  percentage_of_kittens_who_survive_to_sexual_maturity : Percentage.t;
}

let default_parameters =
  {
    litters_per_year = Three;
    months_of_gestation = 2;
    months_before_mature = 4;
    months_of_lifespan = 48;
    kittens_per_litter = 6;
    percentage_of_female_kittens = Percentage.of_int 50;
    percentage_of_kittens_who_survive_to_sexual_maturity = Percentage.of_int 50;
  }

let mating_months = function
  | One -> [ Month 1 ]
  | Two -> [ Month 1; Month 8 ]
  | Three -> [ Month 1; Month 5; Month 8 ]

let is_sexually_active parameters mating_month month_born =
  increase_month month_born parameters.months_before_mature <= mating_month
  && mating_month <= increase_month month_born parameters.months_of_lifespan

type generation = {
  month_born : month;
  surviving_females : int;
  nonsurviving_females : int;
  surviving_males : int;
  nonsurviving_males : int;
}

type population = generation list

let active_females parameters mating_month (population : population) =
  population
  |> List.filter (fun generation ->
         is_sexually_active parameters mating_month generation.month_born)
  |> List.map (fun generation -> generation.surviving_females)
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
  let surviving_females =
    Percentage.take_percentage females
      parameters.percentage_of_kittens_who_survive_to_sexual_maturity
  and surviving_males =
    Percentage.take_percentage males
      parameters.percentage_of_kittens_who_survive_to_sexual_maturity
  in
  let nonsurviving_females = females - surviving_females
  and nonsurviving_males = males - surviving_males in
  let month_born = increase_month mating_month parameters.months_of_gestation in
  {
    month_born;
    surviving_females;
    nonsurviving_females;
    surviving_males;
    nonsurviving_males;
  }

let empty_generation month_born =
  {
    month_born;
    surviving_females = 0;
    nonsurviving_females = 0;
    surviving_males = 0;
    nonsurviving_males = 0;
  }

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

let female_names = ref Imena.zenska
let male_names = ref Imena.moska

let pick_name unused all =
  match !unused with
  | name :: names ->
      unused := names;
      name
  | [] ->
      let m = List.length all in
      let i = Random.int m and j = Random.int m in
      Printf.sprintf "%s-%s" (List.nth all i) (List.nth all j)

let new_male ~alive month_born =
  let name = pick_name male_names Imena.moska in
  { name; month_born; alive; gender = Male }

let new_female ~alive month_born =
  let name = pick_name female_names Imena.zenska in
  { name; month_born; alive; gender = Female [] }

let cat_population cats =
  let counter = Hashtbl.create 64 in
  let update month_born f =
    let current =
      Hashtbl.find_opt counter month_born
      |> Option.value ~default:(empty_generation month_born)
    in
    Hashtbl.replace counter month_born (f current)
  in
  let rec traverse_cats cats = List.iter traverse_cat cats
  and traverse_cat cat =
    match cat.gender with
    | Male when cat.alive ->
        update cat.month_born (fun generation ->
            {
              generation with
              surviving_males = succ generation.surviving_males;
            })
    | Male ->
        update cat.month_born (fun generation ->
            {
              generation with
              nonsurviving_males = succ generation.nonsurviving_males;
            })
    | Female children when cat.alive ->
        update cat.month_born (fun generation ->
            {
              generation with
              surviving_females = succ generation.surviving_females;
            });
        traverse_cats children
    | Female children ->
        update cat.month_born (fun generation ->
            {
              generation with
              surviving_females = succ generation.nonsurviving_females;
            });
        assert (children = [])
  in
  traverse_cats cats;
  counter |> Hashtbl.to_seq |> List.of_seq
  |> List.map (fun (_, generation) -> generation)

let rec mate_cats parameters mating_month population =
  List.map (mate_cat parameters mating_month) population

and mate_cat parameters mating_month cat =
  match cat.gender with
  | Female children when cat.alive ->
      let new_children =
        if is_sexually_active parameters mating_month cat.month_born then
          let new_kittens =
            newborn_generation parameters mating_month
              [
                ({
                   month_born = cat.month_born;
                   surviving_females = 1;
                   nonsurviving_females = 0;
                   surviving_males = 0;
                   nonsurviving_males = 0;
                 }
                  : generation);
              ]
          in
          List.init new_kittens.surviving_females (fun _ ->
              new_female ~alive:true new_kittens.month_born)
          @ List.init new_kittens.nonsurviving_females (fun _ ->
                new_female ~alive:false new_kittens.month_born)
          @ List.init new_kittens.surviving_males (fun _ ->
                new_male ~alive:true new_kittens.month_born)
          @ List.init new_kittens.surviving_females (fun _ ->
                new_male ~alive:false new_kittens.month_born)
        else []
      in
      {
        cat with
        gender =
          Female (new_children @ mate_cats parameters mating_month children);
      }
  | _ -> cat

type stage =
  | Introduction of { female : cat; male : cat }
  | FirstYearLitter of {
      first : bool;
      mating_month : month;
      mating_months_left : month list;
      cats : cats;
    }
  | EndOfFirstYear of cats
  | StartOfOtherYears of { year : year; population : population }

let next_stage parameters = function
  | Introduction { female; male } -> (
      match mating_months parameters.litters_per_year with
      | [] -> assert false
      | mating_month :: mating_months_left ->
          FirstYearLitter
            {
              first = true;
              mating_month;
              mating_months_left;
              cats = mate_cats parameters mating_month [ female; male ];
            })
  | FirstYearLitter { mating_months_left = []; cats; _ } -> EndOfFirstYear cats
  | FirstYearLitter
      { mating_months_left = mating_month :: mating_months_left; cats; _ } ->
      FirstYearLitter
        {
          first = false;
          mating_month;
          mating_months_left;
          cats = mate_cats parameters mating_month cats;
        }
  | EndOfFirstYear cats ->
      StartOfOtherYears { year = Year 1; population = cat_population cats }
  | StartOfOtherYears { year; population } ->
      StartOfOtherYears
        {
          year = next_year year;
          population = population_after_year parameters year population;
        }

let rec history parameters stage = function
  | 0 -> []
  | periods ->
      stage :: history parameters (next_stage parameters stage) (periods - 1)

type model = { parameters : parameters; stage : stage }

let init =
  let female = new_female ~alive:true (Month (-11))
  and male = new_male ~alive:true (Month (-11)) in
  { parameters = default_parameters; stage = Introduction { female; male } }

type msg = SetParameters of parameters

let update (model : model) : msg -> model = function
  | SetParameters parameters -> { model with parameters }
