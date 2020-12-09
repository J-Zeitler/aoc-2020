open System.Text.RegularExpressions

let input = 
  "2.in"
  |> System.IO.File.ReadAllLines
  |> Array.toList;;

type PolicyPassword = {
  PolicyLetter: char
  MinFreq: int
  MaxFreq: int
  Password: string
}

let parsePolicyPassword line =
  let m = Regex.Match(line, "(\d+)\-(\d+)\s([A-z])\:\s([A-z]+)")
  if m.Success then
    match (m.Groups.Values |> List.ofSeq |> List.map (fun v -> v.Value)) with 
    | [_; minFreq; maxFreq; policyLetter; password] ->
        Some({ PolicyLetter = policyLetter.[0];
          MinFreq = int minFreq;
          MaxFreq = int maxFreq;
          Password = password })
    | _ -> None
  else None

let isValidSledRentalDownTheStreetPassword pp =
  let numChars = pp.Password |> Seq.filter ((=) pp.PolicyLetter) |> Seq.length
  numChars >= pp.MinFreq && numChars <= pp.MaxFreq

let boolXor a b =
  (a && not b) || (b && not a)

let isValidToboganRentalPassword pp =
  pp.Password.[pp.MinFreq - 1] = pp.PolicyLetter
  |> boolXor (pp.Password.[pp.MaxFreq - 1] = pp.PolicyLetter)

let numValidPassword isValid input =
  input
  |> List.map parsePolicyPassword
  |> List.fold (fun acc opp ->
    match opp with
    | Some(pp) ->
      if isValid pp
      then acc + 1
      else acc
    | None -> acc) 0

// 2a
input
  |> numValidPassword isValidSledRentalDownTheStreetPassword
  |> printfn "valid sled rental down the street passwords: %d"

// 2b
input
  |> numValidPassword isValidToboganRentalPassword
  |> printfn "valid tobogan rental passwords: %d"
