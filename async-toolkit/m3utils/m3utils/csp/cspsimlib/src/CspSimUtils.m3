MODULE CspSimUtils;
IMPORT TextReader;
IMPORT Scan;
IMPORT Lex, FloatMode;
IMPORT IP;
FROM Fmt IMPORT Int, F;

PROCEDURE ScanIp(str : TEXT) : IP.Address4 =

  PROCEDURE Next() : INTEGER =
    BEGIN
      RETURN Scan.Int(reader.nextE("."))
    END Next;
    
  VAR
    reader := NEW(TextReader.T).init(str);
    res : IP.Address4;
  BEGIN
    FOR i := FIRST(res.a) TO LAST(res.a) DO
      res.a[i] := Next()
    END;
    RETURN res
  END ScanIp;

PROCEDURE FmtIp(READONLY addr : IP.Address4) : TEXT =
  BEGIN
    RETURN F("%s.%s.%s.%s",
             Int(addr.a[0]),
             Int(addr.a[1]),
             Int(addr.a[2]),
             Int(addr.a[3]))
  END FmtIp;

BEGIN END CspSimUtils.
