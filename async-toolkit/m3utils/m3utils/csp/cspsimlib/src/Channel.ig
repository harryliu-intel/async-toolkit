GENERIC INTERFACE Channel(Type);
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
IMPORT DynamicInt;

TYPE
  T = RECORD
    nm             : TEXT;
    slack          : CARDINAL;
    wr, rd         : CARDINAL;       (* write, read pointers *)
    data           : REF Buff;       (* indirect is so can be part of several *)
    waiter         : Process.Closure;
    writer, reader : Process.Frame;  (* used ONLY for assertions! *)

    (* locking *)
    lockwr, lockrd : CARDINAL;
  END;

CONST
  Bitwidth = Type.Width;
  NWords   = (Type.Width - 1) DIV BITSIZE(Word.T) + 1;

TYPE
  Item     = ARRAY [ 0 .. NWords - 1 ] OF Word.T;
  Buff     = ARRAY OF Item;          (* size slack + 1 *)

CONST Brand = "Channel(" & Type.Brand & ")";

PROCEDURE SendProbe(VAR c : T; cl : Process.Closure) : BOOLEAN;

PROCEDURE Send(VAR      c : T;
               READONLY x : Item;
               cl         : Process.Closure) : BOOLEAN;

PROCEDURE SendNative(VAR c : T;
                     x     : INTEGER;
                     cl    : Process.Closure) : BOOLEAN;

PROCEDURE SendDynamic(VAR c : T;
                      x     : DynamicInt.T;
                      cl    : Process.Closure) : BOOLEAN;
  
PROCEDURE RecvProbe(VAR c : T; cl : Process.Closure) : BOOLEAN;
    
PROCEDURE Recv(VAR      c : T;
               VAR      x : Item;
               cl         : Process.Closure) : BOOLEAN;

PROCEDURE RecvNative(VAR      c : T;
                     VAR      x : INTEGER;
                     cl         : Process.Closure) : BOOLEAN;

PROCEDURE RecvDynamic(VAR      c : T;
                      x(*OUT*)   : DynamicInt.T;
                      cl         : Process.Closure) : BOOLEAN;

PROCEDURE ChanDebug(READONLY chan : T) : TEXT;

PROCEDURE New(nm : TEXT; slack : CARDINAL := 0) : REF T;

PROCEDURE Lock  (VAR c : T);
PROCEDURE Unlock(VAR c : T);
PROCEDURE Ready (VAR c : T) : BOOLEAN;
PROCEDURE Wait  (VAR c : T; cl : Process.Closure);
PROCEDURE Unwait(VAR c : T; cl : Process.Closure);
  
END Channel.
