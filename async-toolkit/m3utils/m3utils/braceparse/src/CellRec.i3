INTERFACE CellRec;
IMPORT Wx;
IMPORT Atom;

(* 
   see CellRecClass.Private for the complete revelation of T.

   We need to do it this way because a (completely revealed) T includes
   a reference to its subcells, and Subcell.T refers to this interface, 
   so that would cause circular imports otherwise. 
*)

TYPE
  T <: Public;

  Public = OBJECT
    nm       : Atom.T;
  END;

CONST Brand = "CellRec";

PROCEDURE DebugOut(t : T; wx : Wx.T);
  (* human-readable debugging string for t *)

END CellRec.
    
