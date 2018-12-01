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
IMPORT CompRange;
IMPORT FileWr, Wr, Pickle2 AS Pickle;
(*IMPORT BigInt;*)
IMPORT Word;
IMPORT ContainerData, FieldData;
IMPORT FieldDataSeq;
FROM ContainerData IMPORT Pos, Neg;

CONST TE = Text.Equal;

(* currently limited to 64-bit addresses *)

CONST Usage = "[-m[odel] hlp|mby] [-o -|<filename>]";
TYPE   Models     =                          {  Hlp,   Mby  };
CONST  ModelNames = ARRAY Models OF TEXT     { "hlp", "mby" };

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN Params.Get(0) & ": usage: " & Usage
  END DoUsage;

TYPE
  MyVisitor = AddrVisitor.T OBJECT
    nxtField    : Pos :=  0;
    nxtInternal : Neg := -1;
    fields : FieldDataSeq.T;
    maxAddr := CompAddr.Zero;
  OVERRIDES
    internal := MakeInternal;
    field    := MakeField;
  END;

TYPE
  Internal = ContainerData.T;
  Field    = FieldData.T;

PROCEDURE MakeInternal(v        : MyVisitor;
                       name, tn : TEXT;
                       type     : AddrVisitor.Type;
                       array    : AddrVisitor.Array;
                       parent   : AddrVisitor.Internal)
  : AddrVisitor.Internal =
  BEGIN
    WITH res = NEW(Internal,
                   id := v.nxtInternal,
                   up := parent) DO
      DEC(v.nxtInternal);
      RETURN res
    END
  END MakeInternal;

PROCEDURE MakeField(v          : MyVisitor;
                    nm         : TEXT;
                    at         : CompRange.T;
                    lsb, width : CARDINAL;
                    parent     : AddrVisitor.Internal) =
  VAR
    upId := FIRST(Neg);
  BEGIN
    IF parent # NIL THEN upId := NARROW(parent, Internal).id END;
    
    WITH (*byte = BigInt.Add(BigInt.Mul(BigInt.New(at.pos.word), Eight),
                           BigInt.New(at.pos.bit DIV 8)),*)
         byte = Word.Plus(Word.Times(at.pos.word, 8), at.pos.bit DIV 8),
                             
         lsb  = at.pos.bit MOD 8,
         res = Field {
                   id := v.nxtField,
                   byte := byte,
                   lsb := lsb,
                   wid := width,
                   up := upId } DO
      (*NARROW(parent,Internal).children.addhi(res);*)
      INC(v.nxtField);
      v.maxAddr := CompAddr.Max(v.maxAddr, CompRange.Lim(at));
      v.fields.addhi(res)
    END
  END MakeField;
  
VAR
  map : MemoryMap.T;
  base := CompAddr.T { 0, 0 };
  model := Models.Mby;
  ofn := "mapfields.out";
  wr : Wr.T;
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-o") THEN
        ofn := pp.getNext()
      END;
      
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

  IF TE(ofn, "-") THEN
    wr := Stdio.stdout
  ELSE
    wr := FileWr.Open(ofn)
  END;
  
  Debug.Out("Building address map...");
  CASE model OF
    Models.Hlp => map := NEW(HlpMapAddr.H).init(base)
  |
    Models.Mby => map := NEW(MbyMapAddr.H).init(base)
  END;

  Debug.Out("Visiting address map...");
  WITH v = NEW(MyVisitor, fields := NEW(FieldDataSeq.T).init()) DO
    map.visit(v);

    Debug.Out("Fields  " & Fmt.Int(v.nxtField));
    Debug.Out("Parents " & Fmt.Int(-v.nxtInternal));
    Debug.Out("MaxAddr " & CompAddr.Format(v.maxAddr, bytes := TRUE));

    Debug.Out("Converting address map...");
    WITH a = NEW(REF ARRAY OF Field, v.fields.size()) DO
      FOR i := FIRST(a^) TO LAST(a^) DO
        a[i] := v.fields.get(i);
      END;

      Debug.Out("Writing address map...");
      Pickle.Write(wr, a);
      Wr.Close(wr)
    END
  END
END Main.
