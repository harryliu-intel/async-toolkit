MODULE PgCRIF;
IMPORT Debug, XMLParseStream, Pathname; FROM Debug IMPORT UnNil;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT TextSet, TextSetDef;
IMPORT Wx;
IMPORT PgField;

VAR DoDebug := Debug.GetLevel() > 10;

CONST
  TE = Text.Equal;

CONST
  LenPerByte = 8; (* this is for CRIF in general *)
  
TYPE
  Fields = { Name, AddressOffset, Size, SecurityPG, SecurityReadAccess, SecurityReadCPSecured, SecurityWriteAccess };

CONST
  CRIFhierarchy = ARRAY OF TEXT { "crif", "registerFile", "register" };
  (* the CRIF path to get to a register element *)

  RegTagName = CRIFhierarchy[LAST(CRIFhierarchy)];

  CRIFfields = ARRAY Fields OF TEXT {
    "name",
    "addressOffset",
    "size",
    "Security_PolicyGroup",
    "Security_ReadAccess_Str",
    "Security_Read_CP_Secured",
    "Security_WriteAccess_Str"
  };
  (* CRIF fields we care about *)
  
TYPE
  Simple = XMLParseStream.FileStream OBJECT
    interesting : TextSet.T := NIL;
    parsingReg : Register := NIL;
    parsingField : Wx.T := NIL;
    depth : CARDINAL := 0;
    processBuf : Processor;
  OVERRIDES
    start := Start;
    attr := Attr;
    end := End;
    charData := CharData;
  END;

  Disp = XMLParseStream.Disp;

PROCEDURE Start(self : Simple; el : TEXT) : Disp =
  BEGIN
    INC(self.depth);
    Debug.Out(F("start %s dep %s", el, Int(self.depth)));
    IF self.interesting # NIL AND NOT self.interesting.member(el) THEN
      Debug.Out("start ignoring!");
      RETURN Disp.Abort
    END;

    IF TE(el, RegTagName) THEN
      Debug.Out("New register, depth " & Int(self.depth));
      self.parsingReg := NEW(Register, depth := self.depth)
    ELSIF self.parsingReg # NIL THEN
      FOR i := FIRST(Fields) TO LAST(Fields) DO
        IF TE(el, CRIFfields[i]) THEN
          self.parsingReg.field[i] := Wx.New();
          self.parsingField := self.parsingReg.field[i]
        END
      END
    END;

    RETURN Disp.Continue
  END Start;
  
PROCEDURE Attr(<*UNUSED*>self : Simple; tag, attr : TEXT) : Disp =
  BEGIN
    Debug.Out(F("attr %s %s", tag, attr));
    RETURN Disp.Continue
  END Attr;
  
PROCEDURE End(self : Simple) =
  BEGIN
    DEC(self.depth);
    Debug.Out(F("end, dep <- %s", Int(self.depth)));
    IF self.parsingReg # NIL AND self.depth < self.parsingReg.depth THEN
      (* done parsing reg *)
      VAR
        ft := ARRAY Fields OF TEXT { NIL, .. };
      BEGIN
        FOR i := FIRST(Fields) TO LAST(Fields) DO
          WITH wx = self.parsingReg.field[i] DO
            IF wx # NIL THEN ft[i] := Wx.ToText(wx) END
          END
        END;

        Debug.Out(F("Got reg %s @ %s (+: %s) PG %s",
                    UnNil(ft[Fields.Name]),
                    UnNil(ft[Fields.AddressOffset]),
                    UnNil(ft[Fields.Size]),
                    UnNil(ft[Fields.SecurityPG])));

        CheckForNulls(ft, SET OF Fields { Fields.Name,
                                          Fields.AddressOffset,
                                          Fields.Size,
                                          Fields.SecurityPG });

        self.processBuf(ARRAY PgField.T OF TEXT { ft[Fields.Name],
                                                  ft[Fields.AddressOffset],
                                                  ft[Fields.Size],
                                                  ft[Fields.SecurityPG] },
                        LenPerByte);
        
      END;
      self.parsingReg := NIL;
    END;
    self.parsingField := NIL;
  END End;

PROCEDURE CheckForNulls(READONLY ft : ARRAY Fields OF TEXT;
                        reqd : SET OF Fields) =
  BEGIN
    FOR i := FIRST(ft) TO LAST(ft) DO
      IF i IN reqd AND ft[i] = NIL THEN
        VAR
          errStr := "CheckForNulls : null required field in register: ";
          r : TEXT;
        BEGIN
          FOR j := FIRST(ft) TO LAST(ft) DO
            IF j IN reqd THEN r := " [REQUIRED]" ELSE r := "" END;
            errStr := errStr & "\n" & CRIFfields[j] & r & " : " & UnNil(ft[j])
          END;
          Debug.Error(errStr)
        END
      END
    END
  END CheckForNulls;
  
PROCEDURE CharData(self : Simple; READONLY data : ARRAY OF CHAR) : Disp =
  BEGIN
    Debug.Out(F("chardata %s", Text.FromChars(data)));
    IF self.parsingField # NIL THEN
      Wx.PutStr(self.parsingField, data)
    END;
    RETURN Disp.Continue
  END CharData;

PROCEDURE Parse(path : Pathname.T; processBuf : Processor) =
  VAR
    stream : Simple := 
        NEW(Simple, processBuf := processBuf, interesting := NEW(TextSetDef.T).init()).init(path);
  BEGIN
    FOR i := FIRST(CRIFhierarchy) TO LAST(CRIFhierarchy) DO
      EVAL stream.interesting.insert(CRIFhierarchy[i])
    END;
    FOR i := FIRST(CRIFfields) TO LAST(CRIFfields) DO
      EVAL stream.interesting.insert(CRIFfields[i])
    END;
    
    stream.parse()
  END Parse;

TYPE
  Register = OBJECT
    field : ARRAY Fields OF Wx.T := ARRAY Fields OF Wx.T { NIL, .. };
    depth : CARDINAL;
  END;

BEGIN END PgCRIF.
