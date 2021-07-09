INTERFACE TempReader;
IMPORT Rd, OSError;
IMPORT FileNamer;

(* read temp files for convert trace program *)
(* where is write version of this? *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(fnr : FileNamer.T) : T;

    readEntireFile(idx : CARDINAL; VAR data : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, OSError.E };
  END;

CONST Brand = "TempReader";

END TempReader.
