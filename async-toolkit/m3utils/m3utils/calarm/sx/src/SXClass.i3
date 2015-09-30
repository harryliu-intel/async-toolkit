(* $Id: SXClass.i3,v 1.2 2007/05/22 01:37:38 mika Exp $ *)

INTERFACE SXClass;
IMPORT SXRoot;
IMPORT Time;

REVEAL SXRoot.T <: Private;

TYPE
  Private = SXRoot.Public OBJECT
    mu : MUTEX; (* only for single locking---to lock multiple, use SX.Lock *)
    updated : Time.T;
  METHODS
    depends(depender : SXRoot.T);
    (* register a dependency *)

    touch(when : Time.T; locked := FALSE);
    (* re-calc this node and all its dependents *)

    propagate(when : Time.T; locked := FALSE);
    (* re-calc all dependents *)

    recalc(when : Time.T) : BOOLEAN;
    (* 
       override this: re-calc this node.
       Returns TRUE if value changed, FALSE if it stayed the same.
    *)
  END;

END SXClass.
