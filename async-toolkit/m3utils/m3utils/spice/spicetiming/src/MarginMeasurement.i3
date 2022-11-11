INTERFACE MarginMeasurement;
IMPORT MarginScenario;

TYPE
  T = RECORD
    scenario : MarginScenario.T;
    margin   : LONGREAL;
    at       : LONGREAL;
  END;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1];
  (* compare by margin *)

CONST Brand = "MarginMeasurement";

END MarginMeasurement.
