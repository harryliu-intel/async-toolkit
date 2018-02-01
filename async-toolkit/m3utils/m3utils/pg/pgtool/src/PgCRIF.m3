MODULE PgCRIF;

(* parse Security Policy Group information from a CRIF XML file.

   Author: Mika Nystrom <mika.nystroem@intel.com>

   August 2017
*)

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
  (* the tag that corresponds to a register spec *)

  CRIFfields = ARRAY Fields OF TEXT {
    "name",
    "addressOffset",
    "size",
    "Security_PolicyGroup",
    "Security_ReadAccess_Str",
    "Security_Read_CP_Secured",
    "Security_WriteAccess_Str"
  };
  (* CRIF fields we care about (we currently dont process all of them *)
  
TYPE
  (* the actual XML parsing is done in XMLParseStream.FileStream

     here we override the callback methods of that object and implement
     the business logic 
  *)
  Simple = XMLParseStream.FileStream OBJECT
    interesting : TextSet.T := NIL;
    (* interesting is the tags we are at all interested in *)
    
    parsingReg : Register := NIL;
    (* register that we are currently parsing *)
    
    parsingField : Wx.T := NIL;
    (* field that we are currently parsing (=stuffing with chardata *)
    
    depth : CARDINAL := 0;
    (* depth of XML a.t.m. *)
    
    processBuf : Processor;
    (* callback to the main program, called when we have a complete
       register spec *)
    
    gotErrors := FALSE;
    (* remember that we got errors.. *)
    
    errBuff := "";
    (* ..and remember the output *)
    
  OVERRIDES
    (* these are all the callbacks to XMLParseStream.FileStream *)
    start    := Start;
    attr     := Attr;
    end      := End;
    charData := CharData;
  END;

  Disp = XMLParseStream.Disp;

PROCEDURE Start(self : Simple; el : TEXT) : Disp =
  (* callback on XML el start *)
  BEGIN
    INC(self.depth);
    Debug.Out(F("PgCRIF start %s dep %s", el, Int(self.depth)));
    IF self.interesting # NIL AND NOT self.interesting.member(el) THEN
      Debug.Out("start ignoring!");
      RETURN Disp.Abort
    END;

    IF TE(el, RegTagName) THEN
      (* starting a new register *)
      <*ASSERT self.parsingReg = NIL*>
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
  (* callback on XML attr *)
  BEGIN
    Debug.Out(F("PgCRIF attr %s %s", tag, attr));
    RETURN Disp.Continue
  END Attr;
  
PROCEDURE End(self : Simple) =
  (* callback on XML el end *)
  BEGIN
    DEC(self.depth);
    Debug.Out(F("PgCRIF end, dep <- %s", Int(self.depth)));
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

        WITH nullErr = CheckForNulls(ft, SET OF Fields { Fields.Name,
                                                         Fields.AddressOffset,
                                                         Fields.Size,
                                                         Fields.SecurityPG })
         DO
          IF nullErr # NIL THEN
            Debug.Error("register error : \n" & nullErr & "\n", exit := FALSE);
            self.errBuff := self.errBuff & nullErr;
            self.gotErrors := TRUE
          ELSE
            self.processBuf(ARRAY PgField.T OF TEXT { ft[Fields.Name],
                                                      ft[Fields.AddressOffset],
                                                      ft[Fields.Size],
                                                      ft[Fields.SecurityPG] },
                            LenPerByte)
          END
        END
        
      END;
      self.parsingReg := NIL;
    END;
    self.parsingField := NIL;
  END End;

PROCEDURE CheckForNulls(READONLY ft : ARRAY Fields OF TEXT;
                        reqd : SET OF Fields) : TEXT =
  BEGIN
    FOR i := FIRST(ft) TO LAST(ft) DO
      IF i IN reqd AND ft[i] = NIL THEN
        VAR
          errStr := "CheckForNulls : null required field in register: \n  ";
          r : TEXT;
        BEGIN
          FOR j := FIRST(ft) TO LAST(ft) DO
            IF j IN reqd THEN r := " [REQUIRED]" ELSE r := "" END;
            errStr := errStr & "\n  " & CRIFfields[j] & r & " : " & UnNil(ft[j])
          END;
          RETURN errStr
        END
      END
    END;
    RETURN NIL
  END CheckForNulls;
  
PROCEDURE CharData(self : Simple; READONLY data : ARRAY OF CHAR) : Disp =
  (* callback on XML char data.
     Note that a single char data field can be returned in several goes.

     So we build up the chardata using a Wx.T before passing it on *)
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
    
    stream.parse();

    IF stream.gotErrors THEN
      CONST
        CautionText = "GOT REGISTER ERRORS -- REFUSING TO GENERATE OUTPUT!";
      BEGIN
        Debug.Error(CautionText);
      END
    END
  END Parse;

TYPE
  Register = OBJECT
    field : ARRAY Fields OF Wx.T := ARRAY Fields OF Wx.T { NIL, .. };
    depth : CARDINAL;
  END;

BEGIN END PgCRIF.
