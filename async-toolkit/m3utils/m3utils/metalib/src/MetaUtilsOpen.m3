MODULE MetaUtilsOpen EXPORTS MetaUtils;
IMPORT Pathname, Scheme, FileRd, SchemeBoolean, OSError;

PROCEDURE FileRdOpenOrFalse(pn : Pathname.T) : Scheme.Object =
  BEGIN
    TRY
      RETURN FileRd.Open(pn)
    EXCEPT 
      OSError.E =>
        RETURN SchemeBoolean.False()
    END
  END FileRdOpenOrFalse;

BEGIN END MetaUtilsOpen.
      
