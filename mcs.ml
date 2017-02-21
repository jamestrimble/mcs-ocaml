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

let range start one_past_end =
  let rec aux a b lst =
    if a==b then lst else a::(aux (a+1) b lst) in
  aux start one_past_end []

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

let sort_bidomains bidomains =
  let cmp b0 b1 = max_set_size b0 - max_set_size b1 in
  List.sort cmp bidomains

let filter_bidomain g0_adjrow g1_adjrow edge_type bidomain =
  { left = List.filter (fun u -> g0_adjrow.(u)==edge_type) bidomain.left;
    right = List.filter (fun u -> g1_adjrow.(u)==edge_type) bidomain.right
  }

let filter g0 g1 v w bidomains =
  let head = List.hd bidomains in
  let tail = List.tl bidomains in
  let bidomains' = { left = List.filter (fun u -> u!=v) head.left;
                     right = List.filter (fun u -> u!=v) head.right;
                   } :: tail in
  let g0_adjrow = g0.adjmat.(v) in
  let g1_adjrow = g1.adjmat.(w) in
  let fn = fun edge_type -> List.map (filter_bidomain g0_adjrow g1_adjrow edge_type) bidomains' in
  List.map fn [0; 1; 2; 3] |> List.concat |> List.filter (fun b -> min_set_size b > 0)

let remove_v v bidomains =
  let head = List.hd bidomains in
  let tail = List.tl bidomains in
  let head' =
    { left = List.filter (fun u -> u!=v) head.left;
      right = head.right;
    } in
  if min_set_size head' == 0 then tail
  else head' :: tail

let rec solve g0 g1 incumbent current bidomains =
  Printf.printf "%i\n" (List.length incumbent);
  let incumbent' =
    if List.length incumbent > List.length current then incumbent
    else current in
  if (bound current bidomains) <= (List.length incumbent') then incumbent'
  else
    let bidomains' = sort_bidomains bidomains in
    let head = List.hd bidomains' in
    let v = List.hd head.left in
    (* try mapping v to each w in turn *)
    let incumbent'' =
      let fn = fun incumb w -> solve g0 g1 incumb ((v,w)::current) (filter g0 g1 v w bidomains') in
      List.fold_left fn incumbent' head.right in
    (* try leaving vertex v unassigned *)
    solve g0 g1 incumbent'' current (remove_v v bidomains)

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
  let left = range 0 g0.n in
  let right = range 0 g1.n in
  Printf.printf "%i\n" (List.length left);
  Printf.printf "%i\n" (List.nth left 0);
  Printf.printf "%i\n" (List.nth left 3);
  Printf.printf "%i\n" (List.nth left 17);
  let solution = solve g0 g1 [] [] [{left=left; right=right}] in
  Printf.printf "Length %i\n" (List.length solution);

