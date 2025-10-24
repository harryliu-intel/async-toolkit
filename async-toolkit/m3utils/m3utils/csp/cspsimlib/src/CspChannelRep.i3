(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspChannelRep;
IMPORT CspChannel;
IMPORT CspCompiledProcess AS Process;
IMPORT Word;

(* 
   This is the private interface to a CspChannel.T, needed for 
   implementing sends and receives 

   This interface reveals fields that are the same across all types of
   channels but not needed for non-implementers of channel operations.

   The things revealed here are:

   -- the read and write pointers

   -- the waiter and selecter fields for suspending and unsuspending.
      Note that waiter is not needed anymore here, since it is actually
      used entirely within the Send and Recv operations, which are type-
      specific, but we still document waiter here.  selecter, on the other
      hand, is used in Wait/Unwait, which are type-independent (used in
      the waiting-if implementation)

   -- the fields used for channel "locking"; we put "locking" in quotes 
      because Lock doesn't really lock anything.  What it does is make a
      record of relevant channel state at the entry to a waiting-if so
      that we can tell whether or not the channel state has changed 
      between two points in the code and then take an appropriate action.

   waiter and selecter are separate because there is a fundamental
   difference between a process's suspending on a channel action
   (send, receive, peek) versus its suspending on a select.  If a
   process (really a block/closure) has suspended on a channel action,
   it will certainly be woken up by the complementary action's being
   executed.  If, on the other hand, a process has been suspended on a
   selection, waking that process will only cause guarded commands to
   be retried, that is, no effective code will be executed until
   guards have evaluated to true.  


   Selections
   ==========

   As a consequence of the above, we can implement selections in a way
   that may have spurious wakings-up.  This is important because it
   may be difficult to ensure that selection wake-ups are delivered in
   a timely fashion: consider selecting on two channels.  If both
   channels become active, we would like the implementation to be
   allowed to proceed on the first event without having to definitely
   ensure that no further wake-ups are going to occur.  Otherwise,
   waking up could become very expensive, especially in a distributed
   implementation.

   Sends/Receives
   ==============

   As a second consequence of the above, let's discuss the distributed
   implementation of waiting on a receive or send action.  We observe
   that, if a process has suspended on a receive or send action, it
   has observed that there is no complementary action available YET.
   That process has hence suspended (i.e., it is in a stable state).
   In other words, a non-Nil waiter value means that the implied
   closure is suspended.  It is therefore safe to unsuspend it when it
   is detected that a complementary action is available.  This is true
   even in the case of a distributed implementation.  This means that in
   case of a Channel/Surrogate split, it is safe to wake the local process
   when the data from the counterpart arrives.

*)

REVEAL
  CspChannel.T <: Private;

TYPE
  Private = CspChannel.Public OBJECT
    wr, rd         : CARDINAL;       (* write, read pointers *)

    (* rules for wr, rd : 

       the write pointer wr points to the next location that is to 
       be written

       the data buffer is sized slack + 1, indices therefore increment
       from 0 to slack inclusive, and then wrap.

       wr is incremented only by sending, which may only be done by sender
       rd is incremented only by receiving, which may only be done by receiver
    *)
    
    (*waiter         : Process.Closure;*) (* move to Channel.mg *)
    (*
      Must not be NIL, use special Nil singletons instead.

      Points to either a waiting Closure, or to one of the special Nils.

      the protocol rules for the waiter are as follows:

      I. the waiter is set (assigned) only to a closure of the process 
      performing the assignment.

      So, the receiver will, if it has to wait, assign a closure from 
      among its own closures.

      Likewise, the sender will, if it has to wait, assign a closure from among
      its own closures.

      II. there is a problem with clearing the waiter!

      The counterparty clears the waiter.
    *)

    selecter       : Process.Closure;
    (* 
       Must not be NIL, use special Nil singletons instead.

       Points to either a waiting Closure, or to one of the special Nils.

       the protocol rules for selecter are more lenient than for 
       waiter.

       selecter is just a block that will be notified it might be 
       OK to proceed, not one that is blocked on a communication action.
    *)

    (* locking *)
    lockwr, lockrd : CARDINAL;
    locker         : Process.Closure; (* only used during debug *)

    dirty          : BOOLEAN;  (* have pending surrogate updates *)
    
  METHODS
    isNilClosure(cl : Process.Closure) : BOOLEAN;
    (* check whether a given closure is Nil per the particular channel type.
       only used by CspChannelOps.Unwait *)
  END;

CONST Brand = "CspChannelRep";

(* 
   The following types 
   to be used to do remote surrogates.

   For local surrogates, we just have to call a method locally, and it
   either copies the data from the surrogate to the target or vice versa.

   Using the below, we can split these operations further.  To get the
   data from the target to the surrogate, we generate a ReadUpdate at
   the target and carry it (by any means) to the surrogate, where we
   can apply it.  Likewise, to update the target from the surrogate,
   we can generate a WriteUpdate from the surrogate end and apply this
   to the target end by some means.
*)

REVEAL
  CspChannel.Update = BRANDED Brand & " Update" OBJECT
    id       : CARDINAL;
    waiter   : End;
    selecter : End;
  END;

  CspChannel.ReadUpdate = Update BRANDED Brand & " Read Update" OBJECT
    (* the "Read end" of a remote channel *)
    rd       : CARDINAL;
  END;
  
  CspChannel.WriteUpdate = Update BRANDED Brand & " Write Update" OBJECT
    (* the "Surrogate end" of a remote channel *)
    wr       : CARDINAL;
    writes   : CARDINAL;
    data     : REF ARRAY OF Word.T;
    (* new data since last update -- data up to but not including t.data[wr] *)
  END;

TYPE
  Update = CspChannel.Update;
  
  End = { None, Reader, Writer, ReaderNil, WriterNil, Unknown };
  
END CspChannelRep.
