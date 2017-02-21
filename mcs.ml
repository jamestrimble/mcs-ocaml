type graph =
  { adjmat : int array array;
    labels : int array;
    n      : int;
  };;

let make_graph n =
  { adjmat = Array.make_matrix n n 0;
    labels = Array.make n 0;
    n      = n
  };;

let read_word c =
  input_byte c + input_byte c lsl 8;;

let read_graph fname =
  let c = open_in_bin fname in
  let n = read_word c in
  close_in c;
  { adjmat = Array.make_matrix n n 0;
    labels = Array.make n 0;
    n      = n
  };;
