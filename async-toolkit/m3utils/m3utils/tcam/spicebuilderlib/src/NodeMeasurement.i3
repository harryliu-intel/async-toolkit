INTERFACE NodeMeasurement;
IMPORT Word, ProbeType;

TYPE
  T = RECORD
    nm : TEXT;
    quantity : ProbeType.T;
  END;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

CONST Brand = "NodeMeasurement";

END NodeMeasurement.
