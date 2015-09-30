INTERFACE Valenv;
IMPORT X01;
IMPORT Dims;
IMPORT Bit;
IMPORT NodeRecSeq;
IMPORT AssertionList;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(dutName : TEXT; srcs : NodeRecSeq.T; lims : Lims) : T;

    setTime(tm : LONGREAL); (* must call in monotonically increasing order *)
    getTime() : LONGREAL;   (* mostly debug *)

    getSngl(nm : TEXT) : X01.T;
    getArry(nm : TEXT) : REF ARRAY OF X01.T;

    knownOutput (nm : TEXT; READONLY idx : Dims.T; dly : CARDINAL; v : Bit.T);

    getAssertions() : AssertionList.T;
  END;

  Lims = RECORD
    minLo, maxLo, minHi, maxHi : LONGREAL;
  END;

END Valenv.
