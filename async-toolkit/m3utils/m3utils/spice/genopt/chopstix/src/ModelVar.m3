MODULE ModelVar;
IMPORT ResponseModel;
IMPORT QuadResponse;
IMPORT SchemeUtils;
FROM Fmt IMPORT F;

PROCEDURE Format(READONLY t : T) : TEXT =
  VAR
    str := " ";
  BEGIN
    FOR i := FIRST(QuadResponse.T) TO LAST(QuadResponse.T) DO
      str := str & "[" & QuadResponse.Names[i] & " : " & ResponseModel.TypeNames[t.models[i]] & "] "
    END;
    RETURN F(" %s : {%s}", SchemeUtils.Stringify(t.nm), str)
  END Format;

BEGIN END ModelVar.
