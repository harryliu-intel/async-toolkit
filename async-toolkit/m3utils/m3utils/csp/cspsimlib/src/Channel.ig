GENERIC INTERFACE Channel(Type);
IMPORT CspCompiledProcess AS Process;

TYPE
  T = RECORD
    nm             : TEXT;
    slack          : CARDINAL;
    wr, rd         : CARDINAL;       (* write, read pointers *)
    data           : REF Buff;       (* indirect is so can be part of several *)
    waiter         : Process.Closure;
    writer, reader : Process.Frame;  (* used ONLY for assertions! *)
  END;

  Buff = ARRAY OF Type.T;          (* size slack + 1 *)

CONST Brand = "Channel(" & Type.Brand & ")";

PROCEDURE SendProbe(VAR c : T; cl : Process.Closure) : BOOLEAN;

PROCEDURE Send(VAR      c : T;
               READONLY x : Type.T;
               cl         : Process.Closure) : BOOLEAN;

PROCEDURE RecvProbe(VAR c : T; cl : Process.Closure) : BOOLEAN;
    
PROCEDURE Recv(VAR      c : T;
               VAR      x : Type.T;
               cl         : Process.Closure) : BOOLEAN;

PROCEDURE ChanDebug(READONLY chan : T) : TEXT;

END Channel.
