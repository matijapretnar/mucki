type percentage = Percent of int

let round_div m n =
  let o = m mod n in
  if 2 * o <= n then m / n else (m / n) + 1

let take_percentage n (Percent p) = round_div (n * p) 100
let inverse_percentage (Percent p) = Percent (100 - p)

type year = Year of int

let next_year (Year y) = Year (succ y)

type month = Month of int

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
  Printf.sprintf "%s %d" (List.nth month_names (m mod 12)) (2021 + (m / 12))

type litters_per_year = One | Two | Three
type generation = { month_born : month; females : int; males : int }

let first_generation = { month_born = Month 0; females = 1; males = 1 }

type population = generation list

let initial_population = [ first_generation ]
let add_month (Month m1) (Month m2) = Month (m1 + m2)
let add_year (Year y) (Month m) = Month (m + (12 * y))

type parameters = {
  litters_per_year : litters_per_year;
  months_of_gestation : month;
  months_before_mature : month;
  months_of_lifespan : month;
  kittens_per_litter : int;
  percentage_of_female_kittens : percentage;
  percentage_of_kittens_who_survive_to_sexual_maturity : percentage;
}

let default_parameters =
  {
    litters_per_year = Three;
    months_of_gestation = Month 2;
    months_before_mature = Month 4;
    months_of_lifespan = Month 48;
    kittens_per_litter = 4;
    percentage_of_female_kittens = Percent 50;
    percentage_of_kittens_who_survive_to_sexual_maturity = Percent 50;
  }

type stage =
  | Introduction
  | FirstYear of { mating_months_left : month list; population : population }
  | EndOfYear of population
  | FurtherYears of { year : year; population : population }

let mating_months = function
  | One -> [ Month 1 ]
  | Two -> [ Month 1; Month 8 ]
  | Three -> [ Month 1; Month 5; Month 8 ]

let is_sexually_active parameters mating_month { month_born; _ } =
  add_month month_born parameters.months_before_mature <= mating_month
  && mating_month <= add_month month_born parameters.months_of_lifespan

let active_females parameters mating_month population =
  population
  |> List.filter (is_sexually_active parameters mating_month)
  |> List.map (fun generation -> generation.females)
  |> List.fold_left ( + ) 0

let kittens_born parameters mating_month population =
  active_females parameters mating_month population
  * parameters.kittens_per_litter

let newborn_generation parameters mating_month population =
  let kittens = kittens_born parameters mating_month population in
  let survivors =
    take_percentage kittens
      parameters.percentage_of_kittens_who_survive_to_sexual_maturity
  in
  let females =
    take_percentage survivors parameters.percentage_of_female_kittens
  in
  let males = survivors - females in
  let month_born = add_month mating_month parameters.months_of_gestation in
  { month_born; females; males }

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

let next_stage parameters = function
  | Introduction ->
      FirstYear
        {
          mating_months_left = mating_months parameters.litters_per_year;
          population = initial_population;
        }
  | FirstYear { mating_months_left = []; population } -> EndOfYear population
  | FirstYear
      { mating_months_left = mating_month :: mating_months_left; population } ->
      FirstYear
        {
          mating_months_left;
          population =
            population_after_mating parameters mating_month population;
        }
  | EndOfYear population -> FurtherYears { year = Year 1; population }
  | FurtherYears { year; population } ->
      FurtherYears
        {
          year = next_year year;
          population = population_after_year parameters year population;
        }

let rec history parameters stage = function
  | 0 -> []
  | periods ->
      stage :: history parameters (next_stage parameters stage) (periods - 1)

type model = { parameters : parameters; stage : stage }

let init = { parameters = default_parameters; stage = Introduction }

type msg = SetParameters of parameters

let update (model : model) : msg -> model = function
  | SetParameters parameters -> { model with parameters }

(*
   type model = {
     parameters : parameters;
     population : generation list;
     male_names : string list;
     female_names : string list;
     month : int;
   }


   let init : model =
     {
       parameters = default_parameters;
       population = [ { month_born = 0; females = 1; males = 1 } ];
       male_names = Imena.moska;
       female_names = Imena.zenska;
       month = 0;
     }

   let generation_size generation = generation.females + generation.males

   let population_size model =
     model.population |> List.map generation_size |> List.fold_left ( + ) 0

   (* let deaths model =
     if List.length model.population >= model.parameters.months_of_lifespan then
       generation_size (List.hd (List.rev model.population))
     else 0 *)

   (* let grow_generation parameters generation =
     if
       generation.age
       * (12 / (parameters.months_before_mature + parameters.months_of_gestation))
       < parameters.months_of_lifespan - 1
     then Some { generation with age = generation.age + 1 }
     else None *)

   (* let grow_population model =
     let newborns = newborn_generation model in
     let adults =
       List.filter_map (grow_generation model.parameters) model.population
     in
     {
       model with
       population = newborns :: adults;
       month =
         model.month + model.parameters.months_before_mature
         + model.parameters.months_of_gestation;
   } *)

   (* let long_history model =
     history model
       (model.parameters.months_of_lifespan
        * (12
          / (model.parameters.months_before_mature
           + model.parameters.months_of_gestation))
       + 1) *)

   let short_history model = history model 4

   let rest_of_the_history model =
     long_history model |> List.filteri (fun i _ -> i >= 4)


   type gender = Female | Male
   type cat = { gender : gender; name : string }

   let pick_name remaining all =
     match remaining with
     | name :: names -> (name, names)
     | [] ->
         let m = List.length all in
         let i = Random.int m and j = Random.int m in
         let name = Printf.sprintf "%s-%s" (List.nth all i) (List.nth all j) in
         (name, [])

   let new_male model =
     let name, male_names = pick_name model.male_names Imena.moska in
     ({ gender = Male; name }, { model with male_names })

   let new_female model =
     let name, female_names = pick_name model.female_names Imena.zenska in
     ({ gender = Female; name }, { model with female_names })

   let rec new_cats new_cat number_of_cats model =
     match number_of_cats with
     | 0 -> ([], model)
     | n ->
         let cat, model = new_cat model in
         let cats, model = new_cats new_cat (n - 1) model in
         (cat :: cats, model)

   let parents model =
     let mother, model = new_male model in
     let father, model = new_female model in
     (mother, father, model)

   let sort_cats cats =
     List.sort (fun cat1 cat2 -> compare cat1.name cat2.name) cats

   let children model =
     let total_number = model.parameters.kittens_per_litter in
     let number_of_females =
       take_percentage total_number model.parameters.percentage_of_female_kittens
     in
     let number_of_males = total_number - number_of_females in
     let males, model = new_cats new_male number_of_males model in
     let females, model = new_cats new_female number_of_females model in
     (males, females, model)

   let survivors model cats =
     let cats = Imena.premesaj cats in
     let number_of_survivors =
       take_percentage (List.length cats)
         model.percentage_of_kittens_who_survive_to_sexual_maturity
     in
     cats |> Imena.premesaj
     |> List.filteri (fun i _ -> i < number_of_survivors)
     |> sort_cats

   let grandchildren mothers model =
     List.fold_left
       (fun (grandchildren, granddaughters, model) mother ->
         let males, females, model = children model in
         ( (mother, sort_cats (males @ females)) :: grandchildren,
           females @ granddaughters,
           model ))
       ([], [], model) (List.rev mothers) *)
