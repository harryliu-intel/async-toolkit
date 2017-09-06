MODULE Main;
IMPORT XMLParseStream;
IMPORT Debug;
FROM Fmt IMPORT F;
IMPORT Text;
IMPORT TextSet, TextSetDef;
IMPORT Params;

VAR
  Path := Params.Get(1);

TYPE
  Simple = XMLParseStream.FileStream OBJECT
    interesting : TextSet.T := NIL;
  OVERRIDES
    start := Start;
    attr := Attr;
    end := End;
    charData := CharData;
  END;

  Disp = XMLParseStream.Disp;

VAR DoDebug := Debug.GetLevel() > 10;

PROCEDURE Start(self : Simple; el : TEXT) : Disp =
  BEGIN
    Debug.Out(F("start %s", el));
    IF self.interesting = NIL OR self.interesting.member(el) THEN
      RETURN Disp.Continue
    ELSE
      RETURN Disp.Abort
    END
  END Start;
  
PROCEDURE Attr(self : Simple; tag, attr : TEXT) : Disp =
  BEGIN
    Debug.Out(F("attr %s %s", tag, attr));
    RETURN Disp.Continue
  END Attr;
  
PROCEDURE End(self : Simple) =
  BEGIN
    Debug.Out("end")
  END End;
  
PROCEDURE CharData(self : Simple; READONLY data : ARRAY OF CHAR) : Disp =
  BEGIN
    Debug.Out(F("chardata %s", Text.FromChars(data)));
    RETURN Disp.Continue
  END CharData;

CONST
  InterestingEls =
    ARRAY OF TEXT { "crif",
                    "registerFile",
                    "register",
                    "name",
                    "addressOffset",
                    "size",
                    "Security_PolicyGroup",
                    "Security_ReadAccess_Str",
                    "Security_Read_CP_Secured",
                    "Security_WriteAccess_Str"
  };

VAR
  stream : Simple := NEW(Simple).init(Path);
BEGIN
  stream.parse()
END Main.
 
