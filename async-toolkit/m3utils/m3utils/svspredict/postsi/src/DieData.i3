INTERFACE DieData;

CONST
  VidSteps = 8;

TYPE
  Vids = ARRAY [ 0 .. VidSteps - 1 ] OF LONGREAL;

TYPE
  T = OBJECT
    x, y  : LONGREAL;   (* dist. #s *)

    vmin  : LONGREAL;   (* ATE Vmin *)
    vminP : LONGREAL;   (* P @ ATE Vmin *)

    ageP  : LONGREAL;   (* P @ EOL (age margin + ATE margin) *)

    vcust : LONGREAL;   (* customer V (incl. PS margin) *)
    custP : LONGREAL;   (* P @ vcust *)

    vroun : LONGREAL;   (* V rounded up to nearest mV *)
    rounP : LONGREAL;   (* P @ vroun *)
    
    vidbin: [-1 .. LAST(Vids) ] ;   (* which VID bin *)
    
    vidP  : LONGREAL;   (* P @ VID (incl. VID round-off) *)
  END;

CONST Brand = "DieData";

END DieData.
