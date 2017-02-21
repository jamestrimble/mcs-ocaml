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
  { Mcs.adjmat = adjmat;
    colours = colours;
    n      = n
  }

let () =
  let fname0 = Sys.argv.(1) in
  let fname1 = Sys.argv.(2) in
  let g0 = read_graph fname0 in
  let g1 = read_graph fname1 in
  let solution = Mcs.mcs g0 g1 (fun m -> true) in
  Printf.printf "Best size %i\n" (List.length (List.hd solution));
  Printf.printf "Number of solutions %i\n" (List.length solution);
  List.iter (fun m ->
    List.iter (fun p -> Printf.printf "(%i -> %i) " (fst p) (snd p)) m;
    Printf.printf "\n") solution;

