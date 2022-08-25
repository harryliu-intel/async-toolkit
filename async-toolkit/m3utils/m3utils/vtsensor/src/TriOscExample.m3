MODULE TriOscExample;
FROM TriOsc IMPORT LoadJson, Calibrate, MakeMeshes, Estimate;
IMPORT P3;
IMPORT Wr;
IMPORT Debug;
IMPORT Rd;
IMPORT FileWr;
IMPORT Oscillator;
IMPORT FileRd;
IMPORT Thread;
IMPORT OSError;
IMPORT DataPointSeq;
FROM Fmt IMPORT Int, F;

<*FATAL Thread.Alerted*>

PROCEDURE DoIt() =
    <*FATAL Rd.Failure, OSError.E, Wr.Failure*>
  CONST
    calTemps = ARRAY OF LONGREAL { 0.0d0, 125.0d0 }; 
    k = 0.0d0; (* fudge factor *)
    Samples = 500;
    MeshSize = 50;
  VAR
    recs : DataPointSeq.T;
    rd := FileRd.Open("RING_OSCILLATOR.json");
  BEGIN
    
    recs := LoadJson(rd);
    Rd.Close(rd);
    
    Debug.Out(F("%s points", Int(recs.size())));
    
    VAR
      osc := Calibrate("tttt", recs, calTemps);
      
      testPoint := P3.T { 53.0008d6, 87.8511d6, 216.8556d6 };
      (* this point is 0.38V, 0.47V, 0.85V / all @ tttt 65C *)
      
      tempMeshes := MakeMeshes(ARRAY [0..2] OF Oscillator.T { osc, osc, osc },
                               calTemps,
                               MeshSize);
    BEGIN
      WITH wr = FileWr.Open("testpoint.dat") DO
        Wr.PutText(wr, P3.FormatGnu(testPoint));
        Wr.PutChar(wr, '\n');
        Wr.Close(wr)
      END;
      
      EVAL Estimate(testPoint, tempMeshes, Samples, k)
    END;
  END DoIt; 

BEGIN END TriOscExample.
