INTERFACE LambVerb;

(* Radr, Wadr, and Wdata are pseudo-verbs! *)

TYPE
  T =   {  Nop , Read ,  Writ ,  RdWr, Radr , Wadr,  Wdata   };

CONST Names = ARRAY T OF TEXT                        
    { "Nop", "Read", "Writ", "RdWr", "Radr", "Wadr", "Wdata" };
  Nops = ARRAY T OF [-1..LAST(CARDINAL)]
    {    0 ,     .. };

CONST Brand = "LambVerb";

END LambVerb.
