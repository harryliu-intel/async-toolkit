INTERFACE MosInfo;
IMPORT Atom, Word;
IMPORT Wx;

TYPE
  T = RECORD
    type  : Atom.T;
    len   : CARDINAL; (* in micro-microns = picometers *)
  END;

CONST Brand = "MosInfo";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE DebugOut(READONLY a : T; wx : Wx.T);

END MosInfo.
