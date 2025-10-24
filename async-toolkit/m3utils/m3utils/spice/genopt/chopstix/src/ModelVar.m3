MODULE ModelVar;
IMPORT ResponseModel;
IMPORT StatComponent;
IMPORT SchemeUtils;
FROM Fmt IMPORT F;

PROCEDURE Format(READONLY t : T) : TEXT =
  VAR
    str := " ";
  BEGIN
    FOR i := FIRST(StatComponent.T) TO LAST(StatComponent.T) DO
      str := str & "[" & StatComponent.Names[i] & " : " & ResponseModel.OrderNames[t.orders[i]] & "] "
    END;
    RETURN F(" %s : {%s}", SchemeUtils.Stringify(t.nm), str)
  END Format;

BEGIN END ModelVar.
