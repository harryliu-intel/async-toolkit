(* $Id$ *)

INTERFACE ProcUtils;
IMPORT Rd, Wr, Pathname;
TYPE
  T = TEXT;
  (* a sequence of newline-separated unix commands with arguments
     also understands "cd",";","<",">",">&","|","|&","'","`"
  *)


(* run the command(s) and return their output text *)

EXCEPTION ErrorExit(Error);

TYPE Error = OBJECT END; 
     (* generic errors, e.g., Rd.Failure *)

     OS = Error OBJECT error : TEXT END;
     (* OSError.E *)

     ExitCode = Error OBJECT code : INTEGER END;
     (* process set non-zero exit code *)
  

PROCEDURE ToText(source: T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL): TEXT RAISES { Rd.Failure, ErrorExit } ;

PROCEDURE RdToRd(source: Rd.T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 VAR rd: Rd.T): Completion;

TYPE
  Completion = OBJECT METHODS wait() RAISES { ErrorExit }; END;


(* for more control over i/o: *)

PROCEDURE Run(source: Rd.T;
              stdout,stderr: Writer := NIL;
              stdin: Reader := NIL;
              wd0: Pathname.T := NIL): Completion RAISES { ErrorExit };

PROCEDURE RunText(source: TEXT;
              stdout,stderr: Writer := NIL;
              stdin: Reader := NIL;
              wd0: Pathname.T := NIL): Completion RAISES { ErrorExit };

(* the following are helpers for Reader/Writer threads *)
TYPE
  Reader <: ROOT;
  Writer <: ROOT;

PROCEDURE WriteHere(wr: Wr.T): Writer;
  (* allocate a Writer that writes to wr *)

PROCEDURE GimmeRd(VAR rd: Rd.T): Writer;
  (* allocate a Writer that writes to the read stream of rd *)

PROCEDURE Stdout(): Writer;
  (* allocate a Writer that writes to Stdio.stdout *)

PROCEDURE Stderr(): Writer;
  (* allocate a Writer that writes to Stdio.stderr *)

PROCEDURE ReadHere(rd: Rd.T): Reader;
  (* allocate a Reader that reads from rd *)

PROCEDURE ReadThis(t: TEXT): Reader;
  (* allocate a Reader that reads from the TEXT t *)

PROCEDURE GimmeWr(VAR wr: Wr.T): Reader;
  (* allocate a Reader that reads from the output stream of wr *)

PROCEDURE Stdin(): Reader;
  (* allocate a Reader that reads from Stdio.stdin *)

CONST Brand = "ProcUtils";

END ProcUtils.
