INTERFACE TempReader;
IMPORT Rd, OSError;
IMPORT FileNamer;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(fnr : FileNamer.T) : T;

    readEntireFile(idx : CARDINAL; VAR data : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, OSError.E };
  END;

CONST Brand = "TempReader";

END TempReader.
