(* $Id$ *)

INTERFACE SXRoot;
(* just defines the SX.T type *)

TYPE 
  T <: Public;
  
  Public = OBJECT 

  METHODS
    init() : T;

    wait();
    (* routine will eventually return if the value has changed.
       Note that immediate (synchronous) return can't be guaranteed
       with this method. *)

    destroy();  (* a freeing mechanism, might want WeakRef instead *)

    uninitialize(); 
    (* set so that value() RAISES Uninitialized *)
  END;

(*
   note that this code can be very tricky!

   LOCK t DO t.wait(); < use t.value() >  END(*LOCK*);

   will NOT see a use of t.value() every time t is updated.
*)
END SXRoot.
