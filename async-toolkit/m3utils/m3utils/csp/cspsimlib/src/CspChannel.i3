INTERFACE CspChannel;
IMPORT CspPortObject;
IMPORT CspCompiledProcess AS Process;

TYPE
  T <: Public;

  Public = CspPortObject.T  OBJECT
    slack          : (*CONST*) CARDINAL;
    writer, reader : (*CONST*) Process.Frame;

    width          : (*CONST*) CARDINAL;

    writes         : CARDINAL; (* stats counter : updated by Send() *)
  METHODS
    makeSurrogate() : T;
    (* make a surrogate *)

    unmakeSurrogate() : T;
    (* remove surrogate and return the target *)
    
    writeSurrogate();
    (* update surrogate->target from write end *)
    
    readSurrogate();
    (* update target->surrogate from read end *)

    clean();
    (* mark surrogate/target linkage clean *)
  END;

CONST Brand = "CspChannel";

END CspChannel.
