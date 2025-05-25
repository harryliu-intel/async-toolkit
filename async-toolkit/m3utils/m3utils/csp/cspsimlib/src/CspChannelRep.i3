INTERFACE CspChannelRep;
IMPORT CspChannel;
IMPORT CspCompiledProcess AS Process;

(* 
   This is the private interface to a CspChannel.T, needed for 
   implementing sends and receives 
*)

REVEAL
  CspChannel.T <: Private;

TYPE
  Private = CspChannel.Public OBJECT
    wr, rd         : CARDINAL;       (* write, read pointers *)

    (* rules for wr, rd : 

       the data buffer is sized slack + 1, indices therefore increment
       from 0 to slack inclusive, and then wrap.

       wr is incremented only by sending, which may only be done by sender
       rd is incremented only by receiving, which may only be done by receiver
    *)
    
    
    waiter         : Process.Closure;
    (*
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
       the protocol rules for selecter are more lenient than for 
       waiter.

       selecter is just a block that will be notified it might be 
       OK to proceed, not one that is blocked on a communication action.
    *)

    (* locking *)
    lockwr, lockrd : CARDINAL;
    locker         : Process.Closure; (* only used during debug *)
  END;

CONST Brand = "CspChannelRep";

END CspChannelRep.
