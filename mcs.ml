type graph =
  { adjmat : int array array;
    colours : int array;
    n      : int;
  }

type bidomain =
  {
    left: int list;
    right: int list;
  }

(* function taken from http://ocaml.org/learn/tutorials/99problems.html *)
let range a b =
  let rec aux a b =
	if a > b then [] else a :: aux (a+1) b  in
  if a > b then List.rev (aux b a) else aux a b

let read_word c =
  let first_byte = input_byte c in
  let second_byte = input_byte c in
  first_byte + second_byte lsl 8

let read_graph fname =
  let c = open_in_bin fname in
  let n = read_word c in
  let colours = Array.map (fun _ -> read_word c) (Array.make n 0) in
  let adjmat = Array.make_matrix n n 0 in
  for i = 0 to n-1 do
    let edge_count = read_word c in
    for j = 1 to edge_count do
      let target = read_word c in
      let _ = read_word c in (* discard edge label *)
      adjmat.(i).(target) <- adjmat.(i).(target) lor 1;
      adjmat.(target).(i) <- adjmat.(target).(i) lor 2;
    done
  done;
  close_in c;
  { adjmat = adjmat;
    colours = colours;
    n      = n
  }

let min_set_size bidomain =
  min (List.length bidomain.left) (List.length bidomain.right)

let max_set_size bidomain =
  max (List.length bidomain.left) (List.length bidomain.right)

let bound current bidomains =
  List.fold_left (+) (List.length current) (List.map min_set_size bidomains)

let solve g0 g1 incumbent current bidomains =
  let incumbent' =
    if List.length incumbent > List.length current then incumbent
    else current in
  if (bound current bidomains) <= (List.length incumbent) then incumbent'
  else
    incumbent'

let () =
  let fname0 = Sys.argv.(1) in
  let fname1 = Sys.argv.(2) in
  let g0 = read_graph fname0 in
  let g1 = read_graph fname0 in
  Printf.printf "%i\n" g0.n;
  Printf.printf "%i\n" g1.n;
  Printf.printf "%i\n" g0.colours.(0);
  Printf.printf "%s\n" fname0;
  Printf.printf "%s\n" fname1;
  for i = 0 to g0.n-1 do
    for j = 0 to g0.n-1 do
      Printf.printf "%i " g0.adjmat.(i).(j);
    done;
    Printf.printf "\n";
  done;
  let left = [] in
  let right = [] in
  let solution = solve g0 g1 [] [] [{left=left; right=right}] in
  Printf.printf "Length %i\n" (List.length solution);

