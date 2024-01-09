INTERFACE RepeatMe;
IMPORT Time;

(* call my command line recursively but add execflag as argv[1] *)
(* repeat it until it succeeds *)

PROCEDURE Do(execFlag : TEXT) : BOOLEAN;
  (* returns TRUE if exitcode is 0, FALSE otherwise *)

PROCEDURE Repeat(execFlag : TEXT; maxAttempts : CARDINAL; delay : Time.T);
  (* make up to maxAttempts attempts to run the program, with the execFlag
     given each time.

     After a failure, wait for delay seconds before attempting again.
  *)

  (* example usage:

     In main program:
  
   VAR
     pp := NEW(ParseParams.T).init(Stdio.stderr);

   BEGIN

     IF NOT pp.keywordPresent("-execute") THEN
       RepeatMe.Repeat("-execute", 10, 1.0d0)
     END;

     (* parse command line normally ... *)

     (* run program ... *)

  *)
  
END RepeatMe.
