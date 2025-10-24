MODULE ServerPacketDbg EXPORTS ServerPacket;
IMPORT ByteSeq, Debug;
IMPORT Fmt; FROM Fmt IMPORT F, Int;
IMPORT Wx;

PROCEDURE DisplayChar(c : CHAR) : CHAR =
  BEGIN
    IF ORD(c) >= 16_20 AND ORD(c) <= 16_7e THEN
      RETURN c
    ELSE
      RETURN '.'
    END
  END DisplayChar;
  
  (**********************************************************************)
  
PROCEDURE DebugOut(seq : ByteSeq.T) =

  PROCEDURE Push() =
    BEGIN
      IF n = 0 THEN RETURN END;
      WHILE n < CharsPerBlock * BlocksPerLine DO
        Put(' ', TRUE)
      END;
      Debug.Out(F("%s | %s", Wx.ToText(l), Wx.ToText(r)));
      l := Wx.New(); r := Wx.New();
      n := 0
    END Push;

  PROCEDURE Put(c : CHAR; silent := FALSE) =
    BEGIN
      IF n # 0 AND n MOD CharsPerBlock = 0 THEN
        Wx.PutChar(r, ' ');
        Wx.PutChar(l, ' ');
      END;
      Wx.PutChar(l, DisplayChar(c));
      IF silent THEN
        Wx.PutText(r, "   ")
      ELSE
        Wx.PutText(r, " " & Fmt.Pad(Int(ORD(c),base := 16),
                                    length:=2,
                                    padChar:= '0'))
      END;
      INC(n)
    END Put;
    
  CONST
    CharsPerBlock = 4;
    BlocksPerLine = 2;
  VAR
    n := 0;
    l, r := Wx.New();
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      IF i = 0 THEN
        (* skip *)
      ELSIF i MOD (CharsPerBlock*BlocksPerLine) = 0 THEN
        Push()
      END;
      WITH c = seq.get(i) DO
        Put(VAL(c,CHAR))
      END
    END;
    Push()
  END DebugOut;

BEGIN END ServerPacketDbg.
