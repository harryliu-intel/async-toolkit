(* $Id$ *)

INTERFACE SX;
IMPORT SXRoot;

EXCEPTION Uninitialized;

(* "spreadsheet expressions" *)

TYPE T = SXRoot.T;

TYPE LRF = PROCEDURE(a, b : LONGREAL) : LONGREAL;

CONST Brand = "SX";

VAR mu : MUTEX; 
    (* global mu: locking it causes ALL SX updating
       activity to cease.  Locking mu must be done AFTER locking
       all variables.  (There would be no point in doing it the other
       way around, as only this mu would then be needed and the program
       would become entirely synchronous---maybe not an entirely bad idea.) *)

END SX.
  
