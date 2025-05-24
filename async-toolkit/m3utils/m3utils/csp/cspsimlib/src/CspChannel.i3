INTERFACE CspChannel;
IMPORT CspPortObject;
IMPORT CspCompiledProcess AS Process;

TYPE
  T = CspPortObject.T BRANDED Brand OBJECT
    slack          : CARDINAL;
    wr, rd         : CARDINAL;       (* write, read pointers *)
    waiter         : Process.Closure;
    writer, reader : Process.Frame;  (* used ONLY for assertions! *)

    (* locking *)
    lockwr, lockrd : CARDINAL;
  END;

CONST Brand = "CspChannel";

END CspChannel.
