MODULE CspPort;
IMPORT Atom;
IMPORT CspDirection;

PROCEDURE  New( name     : Atom.T;
                dir      : CspDirection.T;
                class    : Class;
                width    : CARDINAL;
                typeName : Atom.T ) : T =
  BEGIN
    RETURN
      NEW(T,
          name     := name     ,
          dir      := dir      ,
          class    := class    ,
          width    := width    ,
          typeName := typeName )
  END New;

BEGIN END CspPort.
