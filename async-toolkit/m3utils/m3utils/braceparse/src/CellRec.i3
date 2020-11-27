INTERFACE CellRec;
IMPORT Wx;
IMPORT Atom;

TYPE
  T <: Public;

  Public = OBJECT
    nm       : Atom.T;
  END;

CONST Brand = "CellRec";

PROCEDURE DebugOut(t : T; wx : Wx.T);

END CellRec.
    
