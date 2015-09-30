(* $Id: SchemeInputPortClass.i3,v 1.1 2009/04/27 01:04:19 mika Exp $ *)

INTERFACE SchemeInputPortClass;
IMPORT SchemeInputPort;
FROM Scheme IMPORT E;

REVEAL SchemeInputPort.T <: Private;

TYPE
  Private = SchemeInputPort.Public OBJECT METHODS
    fastGetCh() : INTEGER RAISES { E };
    lock();
    unlock();
  END;

END SchemeInputPortClass.
    
