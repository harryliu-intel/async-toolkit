INTERFACE TriOsc;
IMPORT DataPointSeq;
IMPORT Oscillator;
IMPORT Triangle3List;
IMPORT P3;
IMPORT Rd;
IMPORT Json;

EXCEPTION SyntaxError;
          
PROCEDURE LoadJson(rd : Rd.T) : DataPointSeq.T RAISES { SyntaxError, Json.E };

PROCEDURE Calibrate(corner         : TEXT;
                    recs           : DataPointSeq.T;
                    READONLY temps : ARRAY OF LONGREAL) : Oscillator.T;

PROCEDURE MakeMeshes(oscs : ARRAY [0..2] OF Oscillator.T;
                     READONLY calTemps : ARRAY OF LONGREAL;
                     N    : CARDINAL) : REF ARRAY OF Triangle3List.T;

PROCEDURE Estimate(at          : P3.T;
                   tempMeshes  : REF ARRAY OF Triangle3List.T;
                   READONLY calTemps : ARRAY OF LONGREAL;
                   Samples     : CARDINAL;
                   k           : LONGREAL) : LONGREAL;
  
CONST Brand = "TriOsc";
      
END TriOsc.
