type generation = {
  age : int;
  fertile_females : int;
  infertile_females : int;
  males : int;
}

type percentage = Percent of int

let round_div m n =
  let o = m mod n in
  if 2 * o <= n then m / n else (m / n) + 1

let take_percentage n (Percent p) = round_div (n * p) 100
let inverse_percentage (Percent p) = Percent (100 - p)

type parameters = {
  months_of_gestation : int;
  months_before_mature : int;
  kittens_per_litter : int;
  percentage_of_female_kittens : percentage;
  percentage_of_kittens_who_survive_to_sexual_maturity : percentage;
  percentage_of_fertile_sexually_mature_females : percentage;
  average_lifespan_of_a_feral_cat : int;
}

type model = {
  parameters : parameters;
  years_to_project : int;
  population : generation list;
  male_names : string list;
  female_names : string list;
}

let default_parameters =
  {
    months_of_gestation = 2;
    months_before_mature = 4;
    kittens_per_litter = 4;
    percentage_of_female_kittens = Percent 50;
    percentage_of_kittens_who_survive_to_sexual_maturity = Percent 50;
    percentage_of_fertile_sexually_mature_females = Percent 90;
    average_lifespan_of_a_feral_cat = 4;
  }

let init : model =
  {
    parameters = default_parameters;
    years_to_project = 7;
    population =
      [ { age = 0; fertile_females = 1; infertile_females = 0; males = 1 } ];
    male_names = Imena.moska;
    female_names = Imena.zenska;
  }

let active_females model =
  model.population
  |> List.map (fun generation -> generation.fertile_females)
  |> List.fold_left ( + ) 0

let generation_size generation =
  generation.fertile_females + generation.infertile_females + generation.males

let population_size model =
  model.population |> List.map generation_size |> List.fold_left ( + ) 0

let kittens_born model =
  active_females model * model.parameters.kittens_per_litter

let deaths model =
  if
    List.length model.population
    >= model.parameters.average_lifespan_of_a_feral_cat
  then generation_size (List.hd (List.rev model.population))
  else 0

let newborn_generation model =
  let kittens = kittens_born model in
  let survivors =
    take_percentage kittens
      model.parameters.percentage_of_kittens_who_survive_to_sexual_maturity
  in
  let females =
    take_percentage survivors model.parameters.percentage_of_female_kittens
  in
  let fertile_females =
    take_percentage females
      model.parameters.percentage_of_fertile_sexually_mature_females
  in
  let infertile_females = females - fertile_females in
  let males = survivors - females in
  { age = 0; fertile_females; infertile_females; males }

let grow_generation parameters generation =
  if
    generation.age
    * (12 / (parameters.months_before_mature + parameters.months_of_gestation))
    < parameters.average_lifespan_of_a_feral_cat - 1
  then Some { generation with age = generation.age + 1 }
  else None

let grow_population model =
  let newborns = newborn_generation model in
  let adults =
    List.filter_map (grow_generation model.parameters) model.population
  in
  { model with population = newborns :: adults }

let history model =
  let rec aux model = function
    | 0 -> []
    | years -> model :: aux (grow_population model) (years - 1)
  in
  aux model
    (model.years_to_project
     * (12
       / (model.parameters.months_before_mature
        + model.parameters.months_of_gestation))
    + 1)

type msg = GrowPopulation | SetParameters of parameters

let update (model : model) : msg -> model = function
  | GrowPopulation -> grow_population model
  | SetParameters parameters -> { model with parameters }

type gender = Female | Male
type cat = { gender : gender; name : string }

let new_male model =
  match model.male_names with
  | name :: male_names -> ({ gender = Male; name }, { model with male_names })
  | [] -> failwith "No names left"

let new_female model =
  match model.female_names with
  | name :: female_names ->
      ({ gender = Female; name }, { model with female_names })
  | [] -> failwith "No names left"

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

let grandchildren mothers model =
  List.fold_left
    (fun (grandchildren, model) mother ->
      let males, females, model = children model in
      ((mother, sort_cats (males @ females)) :: grandchildren, model))
    ([], model) (List.rev mothers)
