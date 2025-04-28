INTERFACE CspPort;
IMPORT Atom;
IMPORT CspDirection;

TYPE
  T = OBJECT
    name     : Atom.T;
    dir      : CspDirection.T;
    class    : Class;
    width    : CARDINAL;
    typeName : Atom.T;
  END;

  Class = { Node, Channel };

CONST
  ClassTypeNames = ARRAY Class OF TEXT { "Node", "UInt" };

PROCEDURE  New( name     : Atom.T;
                dir      : CspDirection.T;
                class    : Class;
                width    : CARDINAL;
                typeName : Atom.T ) : T;
  
CONST Brand = "CspPort";

END CspPort.
