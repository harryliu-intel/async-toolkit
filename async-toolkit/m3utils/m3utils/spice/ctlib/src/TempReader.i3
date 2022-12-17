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
    (* read data for node index idx from file and store the time series
       in the data array.

       If the data is stored in compressed format, it will be uncompressed
       as part of the reading process.
    *)
  END;

CONST Brand = "TempReader";

END TempReader.
