INTERFACE TempReader;

(* read temp files for convert trace program *)
(* where is write version of this? *)

IMPORT Rd, OSError;
IMPORT FileNamer;
IMPORT TempDataRep;
IMPORT PolySegment16Serial;
IMPORT Matrix;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(fnr : FileNamer.T) : T;

    readEntireFile(idx : CARDINAL; VAR data : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, OSError.E, PolySegment16Serial.Error, Rd.EndOfFile };
    (* read data for node index idx from file and store the time series
       in the data array.

       If the data is stored in compressed format, it will be uncompressed
       as part of the reading process.
    *)

    readEntireFileZ(idx     : CARDINAL;
                    VAR rep : TempDataRep.T; 
                    nsteps  : CARDINAL; (* in case we need it *)
                    targMaxDev := 0.01d0 (* if we need to compress on the fly *)
    )
      RAISES { Rd.Failure, OSError.E, Matrix.Singular };
    (*
       read data from temp file in compressed format, regardless of what
       format was used to store the temp file.

       this method may raise Matrix.Singular only in the case that the temp
       file itself is uncompressed, and a compressed format is requested, and
       ??? goes wrong.  (Matrix.Singular really shouldn't be raised under
       any circumstances---we should probably get to the bottom of this at
       the source rather than passing around the exception like this.)
    *)
  END;

CONST Brand = "TempReader";

END TempReader.
