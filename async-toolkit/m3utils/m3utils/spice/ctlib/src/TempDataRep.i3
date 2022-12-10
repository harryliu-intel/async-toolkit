INTERFACE TempDataRep;

(* this is how compressed data is represented in the temporary files
   e.g. ct.work directory *)

IMPORT SpiceCompress;
IMPORT ArithConstants;

TYPE
  T = RECORD
    norm      : SpiceCompress.Norm;
    code      : ArithConstants.CodeIdx;
    finalData : TEXT;
  END;
  (* this is the format used on disk.

     the data here is arithmetically coded polynomial data 
     
     the codebook used is denoted by the code *)

PROCEDURE ReadFromTemp(tempData    : TEXT;
                       VAR into    : T);

PROCEDURE Reconstruct(READONLY t : T; VAR a : ARRAY OF LONGREAL);
    
CONST Brand = "TempDataRep";

END TempDataRep.
