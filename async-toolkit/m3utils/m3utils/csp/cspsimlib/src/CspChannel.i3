INTERFACE CspChannel;
IMPORT CspPortObject;
IMPORT CspCompiledProcess AS Process;

TYPE
  T <: Public;

  Update <: ROOT;

  ReadUpdate <: Update;
  WriteUpdate <: Update;

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
    (* update surrogate->target from write end (called by reader on surrogate) *)
    
    readSurrogate();
    (* update target->surrogate from read end (called by writer on target) *)

    clean();
    (* mark surrogate/target linkage clean *)

    (* the following are split versions of the Surrogate ops, for 
       distributed implementations : *)
    getReadUpdate() : ReadUpdate;
    applyReadUpdate(u : ReadUpdate);

    getWriteUpdate() : WriteUpdate;
    applyWriteUpdate(u : WriteUpdate);
    
  END;

CONST Brand = "CspChannel";

END CspChannel.
