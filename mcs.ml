type y =
  { mutable nodes : int
  }

let x = {nodes = 0}

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

let filter_bidomain g0_adjrow g1_adjrow bidomain edge_type =
  { left = List.filter (fun u -> g0_adjrow.(u)==edge_type) bidomain.left;
    right = List.filter (fun u -> g1_adjrow.(u)==edge_type) bidomain.right
  }

let filter g0 g1 v w bidomains =
  let head = List.hd bidomains in
  let tail = List.tl bidomains in
  let bidomains' = { left = List.filter (fun u -> u!=v) head.left;
                     right = List.filter (fun u -> u!=w) head.right;
                   } :: tail in
  let g0_adjrow = g0.adjmat.(v) in
  let g1_adjrow = g1.adjmat.(w) in
  let fn = fun bidomain -> List.map (filter_bidomain g0_adjrow g1_adjrow bidomain) [0;1;2;3] in
  List.map fn bidomains' |> List.concat |> List.filter (fun b -> min_set_size b > 0)

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
  x.nodes <- x.nodes + 1;
  Printf.printf "%i\n" x.nodes;
(*  Printf.printf "%i\n" (List.length incumbent);
  Printf.printf "%i\n" (List.length current);
  List.iter (fun p ->
    Printf.printf "%i->%i  " (fst p) (snd p)) current;
  Printf.printf "\n";
  List.iter (fun b -> List.iter (Printf.printf "%i ") b.left;
                      Printf.printf "   ";
                      List.iter (Printf.printf "%i ") b.right;
                      Printf.printf "\n") bidomains;
  Printf.printf "\n"; *)
  let incumbent' =
    if List.length current < List.length (List.hd incumbent) then incumbent
    else if List.length current == List.length (List.hd incumbent) then current :: incumbent
    else [current] in
  let best_mapping_sz = List.length (List.hd incumbent') in
  if List.length bidomains==0 || bound current bidomains < best_mapping_sz then incumbent'
  else
    let bidomains' = sort_bidomains bidomains in
    let head = List.hd bidomains' in
    let v = List.hd head.left in
    (* try mapping v to each w in turn *)
    let incumbent'' =
      let fn = fun incumb w -> solve g0 g1 incumb ((v,w)::current) (filter g0 g1 v w bidomains') in
      List.fold_left fn incumbent' head.right in 
    (* try leaving vertex v unassigned *)
    solve g0 g1 incumbent'' current (remove_v v bidomains')

let adjrow_deg adjrow =
  let x = Array.fold_left (fun a b -> if (b land 1) == 1 then a+1 else a) 0 adjrow in
  let y = Array.fold_left (fun a b -> if (b land 2) == 2 then a+1 else a) 0 adjrow in
  x + y

let vv_order g =
  range 0 g.n |> List.sort (fun v w -> adjrow_deg g.adjmat.(v) - adjrow_deg g.adjmat.(w)) |> List.rev 

let induced_subgraph g vv =
  let n = Array.length vv in
  let colours = Array.map (fun v -> g.colours.(v)) vv in
  let adjmat = Array.map (fun v -> Array.map (fun w -> g.adjmat.(v).(w)) vv)
                          vv in
  { adjmat = adjmat;
    colours = colours;
    n      = n
  }

let all_colours g0 g1 =
  let cc0 = Array.to_list g0.colours in
  let cc1 = Array.to_list g1.colours in
  let cc = List.sort (-) (List.append cc0 cc1) in
  Printf.printf "cc len %i\n" (List.length cc);
  let rec dedup lst =
    match lst with
    | [] -> []
    | hd :: [] -> [hd] 
    | f :: s :: tl -> if f==s then dedup (s::tl) else f::(dedup (s::tl)) in
  dedup cc

let get_label_class g0 g1 colour =
  { left = List.filter (fun v -> g0.colours.(v)==colour) (range 0 g0.n);
    right = List.filter (fun v -> g1.colours.(v)==colour) (range 0 g1.n)
  }

let get_label_classes g0 g1 =
  let colours = all_colours g0 g1 in
  Printf.printf "Number of colours %i\n" (List.length colours);
  List.map (get_label_class g0 g1) colours |> List.filter (fun b -> min_set_size b > 0)

let () =
  let fname0 = Sys.argv.(1) in
  let fname1 = Sys.argv.(2) in
  let g0 = read_graph fname0 in
  let g1 = read_graph fname1 in
  let vv0 = vv_order g0 in
  let vv1 = vv_order g1 in
  let g0' = induced_subgraph g0 (Array.of_list vv0) in
  let g1' = induced_subgraph g1 (Array.of_list vv1) in 
  let initial_label_classes = get_label_classes g0' g1' in
  Printf.printf "Number of label classes %i\n" (List.length initial_label_classes);
  let solution = solve g0' g1' [[]] [] initial_label_classes in
  Printf.printf "Best size %i\n" (List.length (List.hd solution));
  Printf.printf "Length %i\n" (List.length solution);
  List.iter (fun m ->
    List.iter (fun p -> Printf.printf "(%i -> %i) " (fst p) (snd p)) m;
    Printf.printf "\n") solution;


