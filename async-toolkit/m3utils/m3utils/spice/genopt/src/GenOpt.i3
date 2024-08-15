INTERFACE GenOpt;
IMPORT SchemeSymbol;
IMPORT OptCallback;
IMPORT LongRealSeq;
IMPORT SchemeObject;
IMPORT Pathname;

PROCEDURE DefOptVar(nm : SchemeSymbol.T; defval, defstep : LONGREAL);

PROCEDURE SetRhoBeg(to : LONGREAL);

PROCEDURE SetRhoEnd(to : LONGREAL);

PROCEDURE SetCallback(obj : OptCallback.T);

PROCEDURE GetCoords() : LongRealSeq.T;

PROCEDURE OptInit();

PROCEDURE DoIt();

PROCEDURE SetNetbatch(to : BOOLEAN);

PROCEDURE DefSchemaPath(path : Pathname.T);

PROCEDURE DefLoadScm(scmPath : Pathname.T);

PROCEDURE DefDataFilename(fnm : Pathname.T);

PROCEDURE DefEval(obj : SchemeObject.T);

CONST Brand = "GenOpt";
  
END GenOpt.
