GENERIC INTERFACE Channel(Type);
IMPORT CspChannel;
IMPORT CspChannelOps1 AS CspChannelOps;
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
IMPORT DynamicInt;

TYPE
  Ref = T;
  
  T = CspChannel.T BRANDED Brand OBJECT
    data           : REF Buff;       (* indirect is so can be part of several *)
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

(* the following could ACTUALLY be moved out of here. *)
CONST Lock   = CspChannelOps.Lock;
CONST Unlock = CspChannelOps.Unlock;
CONST Ready  = CspChannelOps.Ready;
CONST Wait   = CspChannelOps.Wait;
CONST Unwait = CspChannelOps.Unwait;

END Channel.
