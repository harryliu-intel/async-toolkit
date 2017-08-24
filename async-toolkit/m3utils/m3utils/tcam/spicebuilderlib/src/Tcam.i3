INTERFACE Tcam;

TYPE
  T = RECORD
    N,   (* number of entries       *)
    W,   (* width of each entry     *)
    LN, 
    CN,  (* number of config bits   *)
    SS,  (* depth of a slice        *)
    SN   (* number of slices        *)                 : CARDINAL;
  END;

END Tcam.
