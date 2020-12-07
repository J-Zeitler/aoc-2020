let input = 
  "1.in"
  |> System.IO.File.ReadAllLines
  |> Array.toList
  |> List.map int;;

let rec find2 target ns =
  match ns with
  | hd::tl ->
    if tl |> List.contains (target - hd)
    then Some(hd * (target - hd))
    else find2 target tl
  | [] -> None

let rec find3 ns =
  match ns with
  | hd::tl ->
    match find2 (2020 - hd) tl with
    | Some(x) -> Some(hd * x)
    | None -> find3 tl
  | [] -> None

// 1a
input
|> find2 2020
|> function Some(x) -> sprintf "found %d" x | None -> "not found"
|> printfn "%s";;

// 1b
input
|> find3
|> function Some(x) -> sprintf "found %d" x | None -> "not found"
|> printfn "%s";;
