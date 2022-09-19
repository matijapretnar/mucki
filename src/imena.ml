Random.self_init ()

let premesaj lst =
  lst
  |> List.map (fun a -> (Random.float 1., a))
  |> List.sort (fun (x, _) (y, _) -> Stdlib.compare x y)
  |> List.map (fun (_, a) -> a)

let moska =
  premesaj
    [
      "Bali";
      "Boss";
      "Capek";
      "Cmok";
      "Coolio";
      "Drogo";
      "Faraon";
      "Felix";
      "Garfield";
      "George";
      "Iny";
      "Kiki";
      "Koko";
      "Leo";
      "Lex";
      "Lisek";
      "Lord";
      "Lump";
      "Medo";
      "Muki";
      "Muri";
      "Nero";
      "Nero";
      "Nino";
      "Obi";
      "Pacek";
      "Pepi";
      "Puhko";
      "Rambo";
      "Sid";
      "Smrček";
      "Taček";
      "Tiger";
      "Tipo";
      "Tom";
    ]

let zenska =
  premesaj
    [
      "Angel";
      "Ariel";
      "Bela";
      "Biba";
      "Boni";
      "Coco";
      "Čupka";
      "Ezra";
      "Heidi";
      "Kala";
      "Kiki";
      "Koko";
      "Lexa";
      "Lori";
      "Lumpa";
      "Luna";
      "Megi";
      "Mufka";
      "Nafta";
      "Nala";
      "Piksi";
      "Samba";
      "Šapi";
      "Tačka";
      "Tana";
      "Taša";
      "Tufi";
      "Zara";
    ]
