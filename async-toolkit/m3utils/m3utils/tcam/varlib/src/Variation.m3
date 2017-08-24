MODULE Variation;
IMPORT Pathname, Rd, FileRd, SpiceFormat;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Text;
IMPORT Perturbation, PerturbationRep, PerturbationList;
IMPORT TextReader, TextUtils;
IMPORT LibData, LibDataSet, LibDataSetDef;
IMPORT TextLibDataSetTbl;
IMPORT TextPairSeq, TextPair;
IMPORT Wr, OSError;
IMPORT FloatMode, Lex;
IMPORT TextLongRealTbl AS TextLRTbl;
IMPORT Scan;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
CONST DefVerbose = FALSE;
CONST VarVerbose = FALSE;
VAR Verbose := DefVerbose;
      
REVEAL
  T = Public BRANDED Brand & " T" OBJECT
    tbl : TextLibDataSetTbl.T;
    globalParams : TextLRTbl.T;
  OVERRIDES
    makeParamDict   := MakeParamDict;
    wrModel         := WrModel;
    init            := Init;
    setGlobalParam  := SetGlobalParam;
  END;
    
  Default = PublicDef BRANDED Brand OBJECT
    plist : PerturbationList.T;

  OVERRIDES
    init            := InitDef;
    setPerturbation := SetPerturbation;
    havePerturbation:= HavePerturbation;
  END;

PROCEDURE SetGlobalParam(t : T; nm : TEXT; to : LONGREAL) =
  BEGIN
    EVAL t.globalParams.put(nm, to)
  END SetGlobalParam;

PROCEDURE SetPerturbation(t : Default; p : Perturbation.Default) =
  BEGIN
    t.plist := PerturbationList.Cons(p, t.plist)
  END SetPerturbation;
  
PROCEDURE StartsWith(READONLY b : ARRAY OF CHAR; s : TEXT) : BOOLEAN =
  BEGIN
    FOR i := 0 TO Text.Length(s)-1 DO
      IF i > LAST(b) OR Text.GetChar(s, i) # b[i] THEN
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END StartsWith;
  
PROCEDURE N(r : TextReader.T; VAR tok : TEXT) : BOOLEAN =
  CONST
    WS = " \t";
  BEGIN
    RETURN r.next(WS, skipNulls := TRUE, chunk := tok)
  END N;

PROCEDURE AddToLib(t : T; libNm : TEXT; stuff : LibData.T) =
  VAR
    set : LibDataSet.T;
  BEGIN
    IF NOT t.tbl.get(libNm, set) THEN
      set := NEW(LibDataSetDef.T).init(); EVAL t.tbl.put(libNm, set)
    END;
    EVAL set.insert(stuff)
  END AddToLib;

TYPE Parsing = { Normal, Variation };
       
PROCEDURE Init(t : T; spFn : Pathname.T; varParser : VarParser) : T RAISES { Rd.Failure, OSError.E }=

  PROCEDURE MakeReader(READONLY b : ARRAY OF CHAR;
                       varSyntax := FALSE) : TextReader.T =
    VAR
      str, strA : TEXT;
    BEGIN
      IF varSyntax THEN
        strA := NIL;
        str := Text.FromChars(b)
      ELSE
        strA := TextUtils.ToLower(Text.FromChars(b));
        str  := TextUtils.Replace(strA, "=", " = ")
      END;
      IF Verbose THEN Debug.Out(F("Building reader for \"%s\"",str)) END;
      RETURN NEW(TextReader.T).init(str)
    END MakeReader;

  PROCEDURE StartLib(r : TextReader.T) =
    VAR
      w, v : TEXT;
    BEGIN
      IF Verbose THEN Debug.Out("StartLib") END;
      EVAL N(r, w); (* .lib *)
      EVAL N(r, w); (* filename or model name *)
      IF N(r, v) THEN
        (* w was filename, v is child model *)
        Debug.Out(F("Library %s got child model %s", curLib, v));
        AddToLib(t, curLib, NEW(LibData.LibRef, nm := v))
      ELSE
        curLib := w
      END
    END StartLib;

  PROCEDURE StartVariation() =
    BEGIN
      Verbose := VarVerbose;
      IF Verbose THEN Debug.Out("StartVariation") END
    END StartVariation;

  PROCEDURE EndVariation() =
    BEGIN
      IF Verbose THEN Debug.Out("EndVariation") END;
      Verbose := DefVerbose
    END EndVariation;
    
  PROCEDURE EndLib(r : TextReader.T) =
    BEGIN
      curLib := NIL (* make sure we dont add stuff to the wrong lib *)
    END EndLib;
    
  PROCEDURE Model(r : TextReader.T) =
    VAR
      w : TEXT;
      nm : TEXT;
      type : TEXT;
      params := NEW(TextPairSeq.T).init();
      pair : TextPair.T;
      gotEq := FALSE;
    BEGIN
      IF Verbose THEN Debug.Out("Model") END;
      EVAL N(r, w); (* .model *)
      EVAL N(r, nm);
      EVAL N(r, type);

      IF NOT (TE(type, "pmos") OR TE(type, "nmos")) THEN
        (* dont bother with other models plz *)
        RETURN
      END;
      Debug.Out(F("Library %s got model %s of type %s", curLib, nm, type));

      pair.k1 := NIL;
      WHILE N(r, w) DO
        IF TE(w, "=") THEN
          <*ASSERT pair.k1 # NIL*>
          <*ASSERT pair.k2 = NIL*>
        ELSIF pair.k1 = NIL THEN
          pair.k1 := w
        ELSIF pair.k2 = NIL THEN
          pair.k2 := w;
          <*ASSERT pair.k1 # NIL*>
          <*ASSERT pair.k2 # NIL*>

          IF Verbose THEN
            Debug.Out(F("Adding param \"%s\" <- %s", pair.k1, pair.k2))
          END;
          params.addhi(pair);
          pair.k1 := NIL;
          pair.k2 := NIL
        ELSE
          <*ASSERT FALSE*>
        END
      END;

      Debug.Out(F("Adding model \"%s\" to lib \"%s\"", nm, curLib));
      AddToLib(t, curLib, NEW(LibData.ModelRef,
                              type   := type,
                              nm     := nm,
                              params := params))
    END Model;
    
  VAR
    rd : Rd.T;
    buff := NEW(REF ARRAY OF CHAR, 0);
    lNo : CARDINAL := 0;
    curLib : TEXT;
    parsing := Parsing.Normal;
    os : Parsing;
  BEGIN
    t.tbl := NEW(TextLibDataSetTbl.Default).init();
    t.globalParams := NEW(TextLRTbl.Default).init();
    Debug.Out(F("Variation.Init parsing \"%s\"", spFn));
    rd := FileRd.Open(spFn);

    LOOP
      WITH len = SpiceFormat.GetLine(rd, buff, lNo) DO
        IF len = -1 THEN EXIT END;

        WITH b = SUBARRAY(buff^, 0, len) DO
          IF Verbose THEN Debug.Out(F("Got line \"%s\"", Text.FromChars(b))) END;
          os := parsing;
          CASE parsing OF
            Parsing.Variation =>
            IF StartsWith(b, ".End_Variation") THEN
              EndVariation();
              parsing := Parsing.Normal
            ELSE
              ParseVariationStmt(MakeReader(b, varSyntax := TRUE))
            END
          |
            Parsing.Normal =>
            IF    StartsWith(b, ".lib") THEN
              StartLib(MakeReader(b))
            ELSIF StartsWith(b, ".endl") THEN
              EndLib(MakeReader(b))
            ELSIF StartsWith(b, ".Variation") THEN
              StartVariation();
              parsing := Parsing.Variation
            ELSIF StartsWith(b, ".model") THEN
              Model(MakeReader(b))
            END
          END;
          IF varParser # NIL AND
             os = parsing AND
             parsing = Parsing.Variation THEN
            varParser.line(Text.FromChars(b))
          END
        END
      END
    END;
    Rd.Close(rd);
    RETURN t
  END Init;

PROCEDURE InitDef(def : Default;
                  fromHsp : Pathname.T;
                  varParser : VarParser) : T
  RAISES { Rd.Failure, OSError.E } =
  BEGIN
    def.plist := NIL;
    RETURN T.init(def, fromHsp, varParser)
  END InitDef;
  
PROCEDURE ParseVariationStmt(r : TextReader.T) =
  VAR
    tok : TEXT;
    gotAny := N(r, tok);
  BEGIN
    IF NOT gotAny THEN RETURN END;

    IF TE(tok, "Parameter") THEN
      WITH pn = N(r, tok) DO
        Debug.Out(F("Got parameter \"%s\"", tok))
      END
    END
  END ParseVariationStmt;

PROCEDURE MakeParamDict(t : T;
                        fromLib, baseTranTypeName : TEXT) : TextLRTbl.T =
  VAR
    dict := NEW(TextLRTbl.Default).init();
  BEGIN
    (* copy globals *)
    VAR iter := t.globalParams.iterate();
        s : TEXT;
        q : LONGREAL;
    BEGIN
      WHILE iter.next(s, q) DO EVAL dict.put(s, q) END
    END;
    
    WITH model = SearchTranModel(t, fromLib, baseTranTypeName) DO
      IF model = NIL THEN
        Debug.Error(F("Couldn't find model / library \"%s\" / \"%s\"",
                      baseTranTypeName, fromLib))
      END;
      (* correct model *)

      (* fill in dict here *)
      (* this stuff is a hack, doesnt handle symbolic params *)
      FOR i := 0 TO model.params.size()-1 DO
        WITH p = model.params.get(i) DO
          TRY
            WITH lr = Scan.LongReal(p.k2) DO
              EVAL dict.put(p.k1, lr)
            END
          EXCEPT
            Lex.Error, FloatMode.Trap => (* skip this one *)
          END
        END
      END
    END;
    RETURN dict
  END MakeParamDict;

PROCEDURE HavePerturbation(t : Default;
                           baseTranTypeName : TEXT;
                           named : TEXT;
                           VAR p : Perturbation.T) : BOOLEAN =
  VAR
    pp := t.plist;
  BEGIN
    WHILE pp # NIL DO
      WITH h = NARROW(pp.head, Perturbation.Default) DO
        IF TE(h.model, baseTranTypeName) AND TE(h.var, named) THEN
          p := h;
          RETURN TRUE
        END
      END;
      pp := pp.tail
    END;
    RETURN FALSE
  END HavePerturbation;
  
PROCEDURE WrModel(t    : T;
                  wr   : Wr.T;
                  fromLib, baseTranTypeName, newTranTypeName : TEXT;
                  READONLY params : ARRAY OF Param)
  RAISES { Wr.Failure } =

    
  PROCEDURE EmitParam(model : LibData.ModelRef; idx : CARDINAL) RAISES { Wr.Failure } =
    VAR
      pert : Perturbation.T;
    BEGIN
      WITH p = model.params.get(idx) DO
        Wr.PutText(wr, p.k1);
        Wr.PutText(wr, "=");
        IF t.havePerturbation(baseTranTypeName, p.k1, pert) THEN
          NARROW(pert, Perturbation.Default).dict := dict;
          WITH base = Scan.LongReal(p.k2),
               pv   = pert.calc(),
               new  = base + pv DO
            Wr.PutText(wr, Fmt.LongReal(new, Fmt.Style.Sci));
            Wr.PutText(wr, "")
          END
        ELSE
          Wr.PutText(wr, p.k2);
          Wr.PutText(wr, "")
        END
      END
    END EmitParam;

  VAR
    dict := t.makeParamDict(fromLib, baseTranTypeName);

  BEGIN
    Debug.Out(F("Searching for model in library \"%s\" baseTranType \"%s\"",
                fromLib, baseTranTypeName));
    WITH model = SearchTranModel(t, fromLib, baseTranTypeName) DO
      IF model = NIL THEN
        Debug.Error(F("Couldn't find model / library \"%s\" / \"%s\"",
                      baseTranTypeName, fromLib))
      END;

      
      FOR i := FIRST(params) TO LAST(params) DO
        EVAL dict.put(params[i].nm, params[i].v)
      END;
      
      (* now dump data *)
      Wr.PutText(wr, ".model ");
      Wr.PutText(wr, newTranTypeName);
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, model.type);
      Wr.PutChar(wr, ' ');
      EmitParam(model, 0);
      Wr.PutChar(wr, '\n');
      FOR i := 1 TO model.params.size()-1 DO
        Wr.PutText(wr, "+ ");
        EmitParam(model, i);
        Wr.PutChar(wr, '\n');
      END        
    END
  END WrModel;

PROCEDURE SearchTranModel(t : T; fromLib, modelName : TEXT) : LibData.ModelRef =
  VAR
    set : LibDataSet.T;
    ld : LibData.T;
  BEGIN
    IF NOT t.tbl.get(fromLib, set) THEN
      RETURN NIL
    END;
    WITH iter = set.iterate() DO
      WHILE iter.next(ld) DO
        TYPECASE ld OF
          LibData.LibRef(lr) =>
          WITH try = SearchTranModel(t, lr.nm, modelName) DO
            IF try # NIL THEN RETURN try END
          END
        |
          LibData.ModelRef(mr) =>
          IF TE(modelName, mr.nm) THEN RETURN mr END
        END
      END
    END;
    RETURN NIL
  END SearchTranModel;

PROCEDURE SetP(VAR a : ARRAY OF Param;
               READONLY nm : ARRAY OF TEXT;
               READONLY to : ARRAY OF LONGREAL) =
  VAR
    success : CARDINAL;
  BEGIN
    <*ASSERT NUMBER(nm) = NUMBER(to)*>
    FOR i := FIRST(nm) TO LAST(nm) DO
      success := 0;
      FOR ai := FIRST(a) TO LAST(a) DO
        IF TE(a[ai].nm,nm[i]) THEN a[ai].v := to[i]; INC(success) END
      END;
      IF    success > 1 THEN
        Debug.Error("Variation.SetP: multiple matches for " & nm[i]) 
      ELSIF success = 0 THEN
        Debug.Error("Variation.SetP: couldnt find " & nm[i])
      END
    END
  END SetP;

BEGIN END Variation.
