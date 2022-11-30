INTERFACE SpiceCompress;
IMPORT Trace;
IMPORT Matrix;
IMPORT Wr;

PROCEDURE DoDemo(targMaxDev : LONGREAL;
                 KK         : REF ARRAY OF CARDINAL;
                 trace      : Trace.T;
                 doAllDumps : BOOLEAN);

PROCEDURE CompressArray(rn            : TEXT;
                        (* for debug *)
                        
                        VAR darr : ARRAY OF LONGREAL;
                        (* input data---will be normalized in place *)
                        
                        VAR rarr : ARRAY OF LONGREAL;
                        (* workspace *)
                        
                        targMaxDev : LONGREAL;
                        doAllDumps : BOOLEAN;

                        wr : Wr.T
                        (* can be NIL *)
  )
  RAISES { Matrix.Singular };

END SpiceCompress.
