MODULE Main;
IMPORT MemoryMap;
IMPORT AddrVisitor;
IMPORT mby_top_map_addr AS MbyMapAddr;
IMPORT hlp_top_map_addr AS HlpMapAddr;
IMPORT mby_ppe_rx_top_map_addr AS RxPpeMapAddr;
IMPORT mby_ppe_tx_top_map_addr AS TxPpeMapAddr;
IMPORT CompAddr;
IMPORT Fmt; FROM Fmt IMPORT F;
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
IMPORT CardSeq;
IMPORT OSError, AL;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

(* currently limited to 64-bit addresses *)

CONST Usage = "[-m[odel] hlp|mby] [-o -|<filename>]";
TYPE  Models     =                      {  Hlp,   Mby,   RxPpe,    TxPpe   };
CONST ModelNames = ARRAY Models OF TEXT { "hlp", "mby", "rx-ppe", "tx-ppe" };

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN Params.Get(0) & ": usage: " & Usage
  END DoUsage;

TYPE
  MyVisitor = AddrVisitor.T OBJECT
    nxtField    : Pos :=  0;
    nxtInternal : Neg := -1;
    fields : FieldDataSeq.T;
    internals : CardSeq.T;
    maxAddr := CompAddr.Zero;
  OVERRIDES
    internal := MakeInternal;
    field    := MakeField;
  END;

TYPE
  Internal = ContainerData.T;
  Field    = FieldData.T;

PROCEDURE MakeInternal(v        : MyVisitor;
                       <*UNUSED*>name, tn : TEXT;
                       <*UNUSED*>type     : AddrVisitor.Type;
                       <*UNUSED*>array    : AddrVisitor.Array;
                       parent   : AddrVisitor.Internal)
  : AddrVisitor.Internal =
  BEGIN
    WITH res = NEW(Internal,
                   id := v.nxtInternal,
                   up := parent) DO
      IF -v.nxtInternal # v.internals.size() THEN
        Debug.Error(Fmt.F("-v.nxtInternal = %s # v.internals.size() = %s",
                          Fmt.Int(-v.nxtInternal), Fmt.Int(v.internals.size())))
      END;
      IF parent = NIL THEN
        v.internals.addhi(0)
      ELSE
        v.internals.addhi(-NARROW(parent,Internal).id)
      END;
      DEC(v.nxtInternal);
      RETURN res
    END
  END MakeInternal;

VAR doDebug := Debug.DebugThis("fieldvisitor");
    
PROCEDURE MakeField(v          : MyVisitor;
                    nm         : TEXT;
                    at         : CompRange.T;
                    <*UNUSED*>lsb : CARDINAL;
                    width      : CARDINAL;
                    parent     : AddrVisitor.Internal) =
  VAR
    upId := FIRST(Neg);
  BEGIN
    IF doDebug THEN
      Debug.Out(F("nm %s @ %s", nm, CompRange.Format(at)))
    END;
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
    Debug.Out("Writing to " & ofn);
    TRY
      wr := FileWr.Open(ofn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error("Couldn't open output file \"" & ofn & "\" : OSError.E : " & AL.Format(x))
    END
  END;
  
  Debug.Out("Building address map...");
  CASE model OF
    Models.Hlp   => map := NEW(HlpMapAddr.H  ).init(base)
  |
    Models.Mby   => map := NEW(MbyMapAddr.H  ).init(base)
  |
    Models.RxPpe => map := NEW(RxPpeMapAddr.H).init(base)
  |
    Models.TxPpe => map := NEW(TxPpeMapAddr.H).init(base)
  END;

  Debug.Out("Visiting address map...");
  WITH v = NEW(MyVisitor,
               fields := NEW(FieldDataSeq.T).init(),
               internals := NEW(CardSeq.T).init()) DO
    v.internals.addhi(0); (* this is the root *)
    map.visit(v);

    Debug.Out("Fields  " & Fmt.Int(v.nxtField));
    Debug.Out("Parents " & Fmt.Int(-v.nxtInternal));
    Debug.Out("MaxAddr " & CompAddr.Format(v.maxAddr, bytes := TRUE));

    Debug.Out("Converting address map and tree...");
    WITH a = NEW(REF ARRAY OF Field, v.fields.size()),
         b = NEW(REF ARRAY OF CARDINAL, v.internals.size()) DO
      FOR i := FIRST(a^) TO LAST(a^) DO
        a[i] := v.fields.get(i);
      END;

      FOR i := FIRST(b^) TO LAST(b^) DO
        b[i] := v.internals.get(i)
      END;

      TRY
        Debug.Out("Writing address map... " & Fmt.Int(NUMBER(a^)));
        Pickle.Write(wr, a);
        Debug.Out("Writing internal tree... " & Fmt.Int(NUMBER(b^)));
        Pickle.Write(wr, b);
        Wr.Close(wr)
      EXCEPT
        Wr.Failure(x) => Debug.Error("I/O error writing output : Wr.Failure : " & AL.Format(x))
      |
        Pickle.Error(x) => Debug.Error("Pickle error writing output : Pickle.Error : " & x )
      END
    END
  END
END Main.
