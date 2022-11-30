INTERFACE SpiceCompress;
IMPORT Trace;

PROCEDURE DoDemo(targMaxDev : LONGREAL;
                 KK         : REF ARRAY OF CARDINAL;
                 trace      : Trace.T;
                 doAllDumps : BOOLEAN);
  
END SpiceCompress.
