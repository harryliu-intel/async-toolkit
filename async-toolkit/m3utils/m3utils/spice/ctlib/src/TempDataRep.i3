INTERFACE TempDataRep;

(* this is how compressed data is represented in the temporary files
   e.g. ct.work directory *)

IMPORT SpiceCompress;
IMPORT ArithConstants;
IMPORT Rd;

TYPE
  T = RECORD
    norm      : SpiceCompress.Norm;
    code      : ArithConstants.CodeIdx;
    finalData : TEXT;
  END;
  (* this is the format used on disk.

     the data here is arithmetically coded polynomial data 
     
     the codebook used is denoted by the code *)

PROCEDURE ReadFromTemp(tempData    : TEXT; VAR into    : T);

(*
   TempFile format is as follows
   
   norm.min   4B
   norm.max   4B
   code       1B
   data       <variable>
*)

PROCEDURE ReadFromTempNoNorm(rd : Rd.T; VAR into : T; bytes : CARDINAL);
  (* read a stream as above but without the norm 
     
     thus contains the code (1 byte) and the compressed data 

     bytes is the length of the code + the length of the compressed data

     will update into.code and into.finalData

     will not touch into.norm
  *)


PROCEDURE Reconstruct(READONLY t : T; VAR a : ARRAY OF LONGREAL);
  (* given a T, reconstruct the time series data as [0,1] limited data.
     To recover the original simulation data, apply the norm transformation. *)
  
CONST Brand = "TempDataRep";

END TempDataRep.
