INTERFACE TvpMeasurement;


TYPE
  T = RECORD
    t, (* temp in degrees Celsius *)
    v, (* voltage in volts *)
    p  (* power in watts *)         : LONGREAL;
  END;

CONST Brand = "TvpMeasurement";

PROCEDURE Fmt(READONLY a : T) : TEXT;
  
END TvpMeasurement.
