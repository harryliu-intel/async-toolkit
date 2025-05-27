GENERIC INTERFACE Channel(Type);
IMPORT CspChannel;
IMPORT CspChannelOps1 AS CspChannelOps;
IMPORT CspCompiledProcess AS Process;
IMPORT Word;
IMPORT DynamicInt;

TYPE
  Ref = T;

  T <: CspChannel.T;

  Surrogate <: CspChannel.T;

CONST
  Bitwidth = Type.Width;
  NWords   = (Type.Width - 1) DIV BITSIZE(Word.T) + 1;

TYPE
  Item     = ARRAY [ 0 .. NWords - 1 ] OF Word.T;

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

(* surrogate operations *)
(* note that normally we would call the generic .makeSurrogate method to 
   actually make the surrogate, since that's type-independent, and the caller
   probably doesn't want to keep track of the types of all the channels *)
PROCEDURE MakeSurrogate  (t : T) : Surrogate;
PROCEDURE WriteSurrogate (s : Surrogate);      (* update surrogate from writer *)
PROCEDURE ReadSurrogate  (t : T);              (* update surrogate from reader *)
PROCEDURE UnmakeSurrogate(s : Surrogate) : T;  (* destroy surrogate            *)

END Channel.
