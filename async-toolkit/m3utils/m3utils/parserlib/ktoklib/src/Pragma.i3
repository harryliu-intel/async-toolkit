INTERFACE Pragma;
IMPORT Rd;
CONST
  Brand = "Pragma";
TYPE
  T = OBJECT
  METHODS
    do(rd: Rd.T);
    (* on entry: reader positionned after "%pragma" or line start ("") *)
    (* on exit: reader positionned after pragma arguments *)
  END;
END Pragma.
