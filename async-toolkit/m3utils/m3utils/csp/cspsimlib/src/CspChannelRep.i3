INTERFACE CspChannelRep;
IMPORT CspChannel;
IMPORT CspCompiledProcess AS Process;
IMPORT Word;

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
