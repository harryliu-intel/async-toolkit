MODULE RdlNum;
IMPORT BigInt;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;

CONST SQ = '\'';

VAR doDebug := Debug.DebugThis("RdlNum");
    
REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

TYPE Digit = [0..15];

PROCEDURE Init(t : T; str : TEXT; accepted : SET OF ParsedType) : T
  RAISES { ParseError } =
  VAR
    len := Text.Length(str);
    chars := NEW(REF ARRAY OF CHAR, len);
    sigStart := 0;
  BEGIN
    t.str := str;
    IF doDebug THEN Debug.Out(F("RdlNum: parse \"%s\"", str)) END;
    Text.SetChars(chars^, str);

    t.parsedType := ParsedType.PlainDec;
    IF len > 2 AND chars[0] = '0' AND chars[1] = 'x' THEN
      t.parsedType := ParsedType.HexHex;
      sigStart := 2
    ELSE
      FOR i := 0 TO len-1 DO
        IF chars[i] = SQ THEN
          IF i > len-2 THEN
            RAISE ParseError("Verilog width tick near EOS")
          END;
          CASE chars[i+1] OF
            'b' => t.parsedType := ParsedType.VerilogBin
          |
            'h' => t.parsedType := ParsedType.VerilogHex
          |
            'd' => t.parsedType := ParsedType.VerilogDec
          |
            'o' => t.parsedType := ParsedType.VerilogOct
          ELSE
            RAISE ParseError("Unknown base specifier : " & Text.FromChar(chars[i+1]))
          END;
          t.width := 0;
          FOR j := 0 TO i-1 DO
            t.width := t.width*10 + ORD(chars[j])-ORD('0')
          END;
          sigStart := i + 2;
          EXIT
        END
      END
    END;

    IF NOT t.parsedType IN accepted THEN
      RAISE ParseError ("ParsedType not accepted in context : " & ParsedTypeNames[t.parsedType])
    END;

    WITH base  = ParsedBase[t.parsedType],
         bbase = big[base] DO
      t.x := big[0];
      FOR i := sigStart TO len-1 DO
        VAR cur : Digit;
            c := chars[i];
        BEGIN
          IF c # '_' THEN (* skip underscores in Verilog numbers *)
            CASE c OF
              '0'..'9' => cur := ORD(c) - ORD('0')
            |
              'a'..'f' => cur := ORD(c) - ORD('a') + 10
            |
              'A'..'F' => cur := ORD(c) - ORD('A') + 10
            ELSE
              RAISE ParseError("unknown digit")
            END;
            IF cur >= base THEN RAISE ParseError("digit too large") END;
            t.x := BigInt.Add(BigInt.Mul(bbase, t.x), big[cur])
          END
        END
      END
    END;
    IF doDebug THEN
      VAR widT := "*"; BEGIN
        IF t.width # UnspecifiedWidth THEN widT := Int(t.width) END;
        Debug.Out(
            F("RdlNum.Init : %s -> 10_%s 16_%s",
              t.str, BigInt.Format(t.x, 10), BigInt.Format(t.x, 16) )&
            F(" 2_%s type %s width %s",
              BigInt.Format(t.x, 2), ParsedTypeNames[t.parsedType], widT))
      END
    END;
    RETURN t
  END Init;

VAR
  big : ARRAY [0..16] OF BigInt.T;
BEGIN
  FOR i := FIRST(big) TO LAST(big) DO
    big[i] := BigInt.New(i)
  END
END RdlNum.
