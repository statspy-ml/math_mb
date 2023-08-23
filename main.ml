let baskara a b c =
  let delta = b ** 2.0 -. 4.0 *. a *. c in
  if delta < 0.0 then
    None
  else if delta = 0.0 then
    let x = (-. b) /. (2.0 *. a) in
    Some (x, x)
  else
    let x1 = (-. b +. sqrt delta) /. (2.0 *. a) in
    let x2 = (-. b -. sqrt delta) /. (2.0 *. a) in
    Some (x1, x2)

(* Test the function *)
let () =
  match baskara 1.0 5.0 6.0 with
  | None -> print_endline "No real solutions."
  | Some (x1, x2) -> Printf.printf "x1 = %f, x2 = %f\n" x1 x2
