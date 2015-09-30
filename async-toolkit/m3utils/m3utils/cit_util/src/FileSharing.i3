(* $Id: FileSharing.i3,v 1.1 2007/05/31 22:24:25 mika Exp $ *)

INTERFACE FileSharing;

VAR SimultaneousReadersAndWritersAreOK : BOOLEAN; (* CONST *)
(* this "VAR" is FALSE for Windows and TRUE for Unix *)

END FileSharing.
  
