(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: FileReWr.i3,v 1.2 2004/06/26 00:47:42 kp Exp $ *)

INTERFACE FileReWr;
IMPORT Wr, Pathname;
TYPE
  T <: Wr.T;
PROCEDURE Open(p: Pathname.T; errorMsg: TEXT := NIL): T;

  (* If the file exists and the contents match what is written,
     then don't touch the file. Otherwise write the file.

     hint: Don't forget to call "Wr.Close" when you're done

     If "errorMsg = NIL", then "Wr.Close" raises "Wr.Failure".
     Otherwise, "Wr.Close" prints "errorMsg",
     replacing occurences of "%filename%" in "errorMsg" by "p".
  

  must not use in multiple threads 


*)

END FileReWr.
