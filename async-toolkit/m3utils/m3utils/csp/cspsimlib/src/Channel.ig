GENERIC INTERFACE Channel(Type);
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
IMPORT DynamicInt;

TYPE
  Ref = T;
  
  T = OBJECT
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

PROCEDURE SendProbe(c : T; cl : Process.Closure) : BOOLEAN;

PROCEDURE Send(         c : T;
               READONLY x : Item;
               cl         : Process.Closure) : BOOLEAN;

PROCEDURE SendNative(    c : T;
                     x     : INTEGER;
                     cl    : Process.Closure) : BOOLEAN;

PROCEDURE SendDynamic(    c : T;
                      x     : DynamicInt.T;
                      cl    : Process.Closure) : BOOLEAN;
  
PROCEDURE RecvProbe(c : T; cl : Process.Closure) : BOOLEAN;
    
PROCEDURE Recv(         c : T;
               VAR      x : Item;
               cl         : Process.Closure) : BOOLEAN;

PROCEDURE RecvNative(         c : T;
                     VAR      x : INTEGER;
                     cl         : Process.Closure) : BOOLEAN;

PROCEDURE RecvDynamic(         c : T;
                      x(*OUT*)   : DynamicInt.T;
                      cl         : Process.Closure) : BOOLEAN;

PROCEDURE ChanDebug(chan : T) : TEXT;

PROCEDURE New(nm : TEXT; slack : CARDINAL := 0) : Ref;

PROCEDURE Lock  (c : T; cl : Process.Closure);
PROCEDURE Unlock(c : T; cl : Process.Closure);
PROCEDURE Ready (c : T; cl : Process.Closure) : BOOLEAN;
PROCEDURE Wait  (c : T; cl : Process.Closure);
PROCEDURE Unwait(c : T; cl : Process.Closure);
  
END Channel.
