INTERFACE ProcUtils;
IMPORT Rd, Wr, Pathname;
TYPE
  T = TEXT;
  (* a sequence of newline-separated unix commands with arguments
     also understands "cd",";","<",">",">&","|","|&","'","`"
  *)


(* run the command(s) and return their output text *)

PROCEDURE ToText(source: T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL): TEXT;

PROCEDURE RdToRd(source: Rd.T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 VAR rd: Rd.T): Completion;

TYPE
  Completion = OBJECT METHODS wait(); END;


(* for more control over i/o: *)

PROCEDURE Run(source: Rd.T;
              stdout,stderr: Writer := NIL;
              stdin: Reader := NIL;
              wd0: Pathname.T := NIL): Completion;

TYPE
  Reader <: ROOT;
  Writer <: ROOT;

PROCEDURE WriteHere(wr: Wr.T): Writer;
PROCEDURE GimmeRd(VAR rd: Rd.T): Writer;
PROCEDURE Stdout(): Writer;
PROCEDURE Stderr(): Writer;

PROCEDURE ReadHere(rd: Rd.T): Reader;
PROCEDURE ReadThis(t: TEXT): Reader;
PROCEDURE GimmeWr(VAR wr: Wr.T): Reader;
PROCEDURE Stdin(): Reader;

END ProcUtils.
