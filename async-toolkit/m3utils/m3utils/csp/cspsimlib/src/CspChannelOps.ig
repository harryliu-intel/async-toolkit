GENERIC INTERFACE CspChannelOps();

(* 
   not sure whether this needs to be GENERIC 

   It is only intended to be instantiated once, together with its 
   implementation.

   The only thing that is parameterized is CspDebug --- the level of debugging
   desired at runtime.
*)

IMPORT CspCompiledProcess AS Process;
IMPORT CspChannel;

TYPE T = CspChannel.T;

PROCEDURE Lock  (c : T; cl : Process.Closure);
PROCEDURE Unlock(c : T; cl : Process.Closure);
PROCEDURE Ready (c : T; cl : Process.Closure) : BOOLEAN;
PROCEDURE Wait  (c : T; cl : Process.Closure);
PROCEDURE Unwait(c : T; cl : Process.Closure);

END CspChannelOps.
