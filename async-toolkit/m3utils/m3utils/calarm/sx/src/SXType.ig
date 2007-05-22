(* $Id$ *)

GENERIC INTERFACE SXType(Elem);

FROM SX IMPORT Uninitialized;
IMPORT SXRoot;
IMPORT Time;

(* locking order: SX.mu must be locked after self.mu in all cases *)

TYPE 
  Base = Elem.T;

  T <: Public;

  Public = SXRoot.T OBJECT METHODS
    value() : Elem.T RAISES { Uninitialized };
    waitFor(val : Elem.T);

    update(newValue : Elem.T; when : Time.T) : BOOLEAN;
    (* 
       used only by routines implementing a node;
       DOES NOT propagate up the tree.
       Returns TRUE if value was changed, FALSE otherwise. 

       self.mu may not be locked when calling this
    *)

    updateLocked(newValue : Elem.T; when : Time.T) : BOOLEAN;
    (* same as update, but self.mu must be locked *)

    numUpdates() : CARDINAL;
    (* how many times updated? *)

    initialized() : BOOLEAN;
    (* TRUE if numUpdates > 0 *)

  END;

  Var <: PublicVar;
  
  PublicVar = T OBJECT METHODS
    set(newValue : Elem.T; when := FIRST(Time.T));
    (* call from outside LOCK self.mu --- is this really right? *)

    setLocked(newValue : Elem.T; when := FIRST(Time.T));
    (* call this is you are already holding self.mu. *)

    initVal(initValue : Elem.T) : Var; 
    (* initialize with value, and updates = 1 *)
  END;

  Const <: PublicConst;

  PublicConst = T OBJECT METHODS
    init(value : Elem.T) : Const;
  END;

CONST Brand = "SXType(" & Elem.Brand & ")";

CONST BaseEqual = Elem.Equal;

PROCEDURE BaseCompare(a, b : Base) : INTEGER;
  (* same as Elem.Compare, but type-wrapped *)

PROCEDURE NewConst(value : Elem.T) : Const;

END SXType.
