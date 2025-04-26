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

PROCEDURE  New( name     : Atom.T;
                dir      : CspDirection.T;
                class    : Class;
                width    : CARDINAL;
                typeName : Atom.T ) : T;
  
CONST Brand = "CspPort";

END CspPort.
