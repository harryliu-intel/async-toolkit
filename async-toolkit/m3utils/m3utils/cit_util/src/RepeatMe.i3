INTERFACE RepeatMe;
IMPORT Time;

(* call my command line recursively but add execflag as argv[1] *)
(* repeat it until it succeeds *)

PROCEDURE Do(execFlag : TEXT) : BOOLEAN;
  (* returns TRUE if exitcode is 0, FALSE otherwise *)

PROCEDURE Repeat(execFlag : TEXT; maxTimes : CARDINAL; delay : Time.T) : BOOLEAN;
  
END RepeatMe.
