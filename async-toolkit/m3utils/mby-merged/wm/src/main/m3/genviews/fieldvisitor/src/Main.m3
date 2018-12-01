MODULE Main;
IMPORT MemoryMap;
IMPORT AddrVisitor;
IMPORT mby_top_map_addr AS MbyMapAddr;
IMPORT hlp_top_map_addr AS HlpMapAddr;
IMPORT CompAddr;
IMPORT Fmt;
IMPORT Debug;
IMPORT ParseParams;
IMPORT Stdio, Text;
IMPORT Params;
IMPORT RefSeq;
IMPORT CompRange;
IMPORT FileWr, Wr, Pickle;

CONST Usage = "[-m[odel] hlp|mby]";
TYPE   Models     =                          {  Hlp,   Mby  };
CONST  ModelNames = ARRAY Models OF TEXT     { "hlp", "mby" };

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN Params.Get(0) & ": usage: " & Usage
  END DoUsage;

TYPE
  MyVisitor = AddrVisitor.T OBJECT
    fields : RefSeq.T;
  OVERRIDES
    internal := MakeInternal;
    field    := MakeField;
  END;

TYPE Neg = [ FIRST(INTEGER) .. -1 ];
     Pos = CARDINAL;

VAR nxtField    : Pos :=  0;
VAR nxtInternal : Neg := -1;

TYPE
  Internal = AddrVisitor.Internal OBJECT
    id : Neg;
    up : Internal;
  END;

  Field = OBJECT
    id : Pos;
    up : Internal;
    at : CompRange.T;
  END;

PROCEDURE MakeInternal(v        : MyVisitor;
                       name, tn : TEXT;
                       type     : AddrVisitor.Type;
                       array    : AddrVisitor.Array;
                       parent   : AddrVisitor.Internal)
  : AddrVisitor.Internal =
  BEGIN
    WITH res = NEW(Internal, id := nxtInternal, up := parent) DO
      DEC(nxtInternal);
      RETURN res
    END
  END MakeInternal;

PROCEDURE MakeField(v          : MyVisitor;
                    nm         : TEXT;
                    at         : CompRange.T;
                    lsb, width : CARDINAL;
                    parent     : AddrVisitor.Internal) =
  BEGIN
    WITH res = NEW(Field, id := nxtField, at := at, up := parent) DO
      INC(nxtField);
      v.fields.addhi(res)
    END
  END MakeField;
  
VAR
  map : MemoryMap.T;
  base := CompAddr.T { 0, 0 };
  model := Models.Mby;
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-model") OR pp.keywordPresent("-m") THEN
        VAR
          modelStr := pp.getNext();
          success := FALSE;
        BEGIN
          FOR i := FIRST(Models) TO LAST(Models) DO
            IF Text.Equal(modelStr, ModelNames[i]) THEN
              model := i;
              success := TRUE
            END
          END;
          IF NOT success THEN
            Debug.Error("Unknown model \"" & modelStr & "\"")
          END
        END
      END;
      pp.skipParsed();
      pp.finish()
    END;
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  Debug.Out("Building address map...");
  CASE model OF
    Models.Hlp => map := NEW(HlpMapAddr.H).init(base)
  |
    Models.Mby => map := NEW(MbyMapAddr.H).init(base)
  END;

  Debug.Out("Visiting address map...");
  WITH v = NEW(MyVisitor, fields := NEW(RefSeq.T).init()) DO
    map.visit(v);
  
    Debug.Out("Writing address map...");
    WITH wr = FileWr.Open("mapfields.out") DO
      Pickle.Write(wr, v.fields);
      Wr.Close(wr)
    END
  END
END Main.
