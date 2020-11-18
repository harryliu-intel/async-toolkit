INTERFACE StdfE;

EXCEPTION E ( TEXT );  (* error parsing a field *)

          Missing;     (* field is missing 
                          (parsing initiated with len=0) *)

CONST Brand = "StdfE";

END StdfE.
      
