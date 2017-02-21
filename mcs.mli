type graph =
  { adjmat : int array array;
    colours : int array;
    n : int;
  }

val mcs : graph -> graph -> (int * int) list list