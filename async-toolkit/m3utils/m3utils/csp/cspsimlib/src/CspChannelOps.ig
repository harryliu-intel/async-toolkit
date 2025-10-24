(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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

(* 
   These are operations for implementing CSP selection statements.

   See the discussion in selection.txt for more details.

   Will be used as part of a "waiting-if".
*)     
     
PROCEDURE Lock  (c : T; cl : Process.Closure);
  (* Lock doesn't actually "lock" anything.

     What it does is makes a copy (in lockwr and lockrd) of the channel
     pointers.

     This means that we can detect whether those pointers have moved in the
     following sequence:

     Lock(c, cl);

     ... sequence of non-atomic operations ...

     <test whether the pointers have moved using Ready()>

     ... sequence of non-atomic operations ...

     Unlock(c, cl)
  *)

  
PROCEDURE Unlock(c : T; cl : Process.Closure);
  (* stop monitoring the pointers *)
  
PROCEDURE Ready (c : T; cl : Process.Closure) : BOOLEAN;
  (* have the pointers moved since we called Lock()? *)
  
PROCEDURE Wait  (c : T; cl : Process.Closure);
  (* 
     mark ourselves waiting in a selection.

     This is then followed by a RETURN FALSE so that the scheduler
     knows we have not yet succeeded 
  *)

     
PROCEDURE Unwait(c : T; cl : Process.Closure);
  (* 
     unmark ourselves from waiting in a selection.

     Will typically be called as the last action in a selection statement,
     before RETURN TRUE (and allowing the scheduler to schedule the
     next block to execute)
  *)


END CspChannelOps.
