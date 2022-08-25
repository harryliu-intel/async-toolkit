INTERFACE TriOsc;
IMPORT DataPointSeq;
IMPORT Oscillator;
IMPORT Triangle3List;
IMPORT P3;
IMPORT Rd;
IMPORT Json;
IMPORT P3P3Tbl;

EXCEPTION SyntaxError;
          
PROCEDURE LoadJson(rd : Rd.T) : DataPointSeq.T RAISES { SyntaxError, Json.E };

PROCEDURE Calibrate(corner         : TEXT;
                    recs           : DataPointSeq.T;
                    READONLY temps : ARRAY OF LONGREAL) : Oscillator.T;

PROCEDURE MakeMeshes(oscs : ARRAY [0..2] OF Oscillator.T;
                     READONLY calTemps : ARRAY OF LONGREAL;
                     N    : CARDINAL) : REF ARRAY OF Mesh;

PROCEDURE Estimate(at          : P3.T;
                   tempMeshes  : REF ARRAY OF Mesh;
                   Samples     : CARDINAL;
                   k           : LONGREAL) : LONGREAL;

TYPE
  Mesh = RECORD
    temp : LONGREAL;
    triangles : Triangle3List.T;
    volttbl : P3P3Tbl.T;
  END;
  
CONST Brand = "TriOsc";
      
END TriOsc.
