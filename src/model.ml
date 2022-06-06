type generation = {
  age : int;
  fertile_females : int;
  infertile_females : int;
  males : int;
}

type percentage = Percent of int

let take_percentage n (Percent p) = n * p / 100

type model = {
  litters_per_year : int;
  kittens_per_litter : int;
  percentage_of_female_kittens : percentage;
  percentage_of_kittens_who_survive_to_sexual_maturity : percentage;
  percentage_of_fertile_sexually_mature_females : percentage;
  average_lifespan_of_a_feral_cat : int;
  years_to_project : int;
  population : generation list;
}

let init : model =
  {
    litters_per_year = 2;
    kittens_per_litter = 4;
    percentage_of_female_kittens = Percent 50;
    percentage_of_kittens_who_survive_to_sexual_maturity = Percent 70;
    percentage_of_fertile_sexually_mature_females = Percent 100;
    average_lifespan_of_a_feral_cat = 4;
    years_to_project = 7;
    population =
      [ { age = 1; fertile_females = 1; infertile_females = 0; males = 1 } ];
  }

let active_females model =
  model.population
  |> List.map (fun generation -> generation.fertile_females)
  |> List.fold_left ( + ) 0

let generation_size generation =
  generation.fertile_females + generation.infertile_females + generation.males

let population_size model =
  model.population |> List.map generation_size |> List.fold_left ( + ) 0

let newborn_generation model =
  let kittens = active_females model * model.kittens_per_litter in
  let females = take_percentage kittens model.percentage_of_female_kittens in
  let males = kittens - females in
  { age = 0; fertile_females = females; infertile_females = 0; males }

let kittens_born model = generation_size (newborn_generation model)

let grow_generation model generation =
  match generation.age with
  | 0 ->
      let females =
        take_percentage
          (generation.infertile_females + generation.fertile_females)
          model.percentage_of_kittens_who_survive_to_sexual_maturity
      in
      let fertile_females =
        take_percentage females
          model.percentage_of_fertile_sexually_mature_females
      in
      let infertile_females = females - fertile_females in
      let males =
        take_percentage generation.males
          model.percentage_of_kittens_who_survive_to_sexual_maturity
      in
      Some { age = 1; fertile_females; infertile_females; males }
  | age
    when age < model.average_lifespan_of_a_feral_cat * model.litters_per_year ->
      Some { generation with age = generation.age + 1 }
  | _ -> None

let grow_population model =
  let newborns = newborn_generation model in
  let adults = List.filter_map (grow_generation model) model.population in
  { model with population = newborns :: adults }

let rec history model = function
  | 0 -> []
  | n -> model :: history (grow_population model) (n - 1)

type msg = GrowPopulation | SetModel of model

let update (model : model) : msg -> model = function
  | GrowPopulation -> grow_population model
  | SetModel model -> model
