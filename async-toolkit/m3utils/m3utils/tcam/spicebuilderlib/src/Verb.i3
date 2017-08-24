INTERFACE Verb;


(* Addr and Data are pseudo-verbs ! *)

TYPE
  T =   {  Nop ,  Rset ,  Look ,  Read ,  Writ ,  Addr ,  Data   };

CONST Names = ARRAY T OF TEXT                        
    { "Nop", "Rset", "Look", "Read", "Writ", "Addr", "Data"  };
  Nops = ARRAY T OF [-1..LAST(CARDINAL)]
    {    0 ,     0 ,     0 ,     1 ,     1 ,    -1 ,    -1   };

CONST Brand = "Verb";

END Verb.
