INTERFACE CspPort;
IMPORT Atom;
IMPORT CspDirection;

TYPE
  T = OBJECT
    name     : Atom.T;
    dir      : CspDirection.T;
    def      : Channel;
  END;

  Channel = ROOT BRANDED OBJECT END;

  Scalar = Channel OBJECT
    class    : Class;
    width    : CARDINAL;
    typeName : Atom.T;
  END;

  Array = Channel OBJECT
    range : Range;
    elem  : Channel;
  END;

  Range = RECORD min, max : INTEGER END;

  Class = { Node, Channel };

CONST
  ClassTypeNames = ARRAY Class OF TEXT { "Node", "UInt" };

PROCEDURE  New( name     : Atom.T;
                dir      : CspDirection.T;
                def      : Channel ) : T;

PROCEDURE NewScalar(class    : Class;
                    width    : CARDINAL;
                    typeName : Atom.T) : Scalar;

PROCEDURE NewArray(range : Range;
                   elem  : Channel) : Array;

PROCEDURE NewRange(min, max : INTEGER) : Range;

PROCEDURE BaseChanType(chan : Channel) : Scalar;

PROCEDURE M3ChanDecl(chan : Channel) : TEXT;

CONST Brand = "CspPort";

END CspPort.
