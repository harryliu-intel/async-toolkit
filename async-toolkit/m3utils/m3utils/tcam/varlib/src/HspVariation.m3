MODULE HspVariation;
IMPORT Pathname;
IMPORT Variation;
IMPORT Debug;
IMPORT TextReader;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Text;
IMPORT OSError, Rd;
IMPORT cmdParseExt, cmdLexExt;
IMPORT TextRd;
IMPORT AssignmentList;
IMPORT TextSet, TextSetDef;
IMPORT TextLongRealTbl AS TextLRTbl;
IMPORT VarExpr;
IMPORT TextRefTbl;
IMPORT Perturbation, PerturbationRep;
IMPORT Assignment;

CONST TE = Text.Equal;
      LR = LongReal;
TYPE
  VarParser = Variation.VarParser OBJECT
    me : T;
  OVERRIDES
    line := VPLine;
  END;

CONST WS = " \t";

PROCEDURE N(r : TextReader.T; VAR t : TEXT) : BOOLEAN =
  BEGIN RETURN r.next(WS, t, skipNulls := TRUE) END N;

PROCEDURE DefParam(tok : TEXT) =
  BEGIN
    Debug.Out(F("HspVariation.DefParam param \"%s\"", tok))
  END DefParam;

PROCEDURE HandleModel(vp   : VarParser;
                      type : TEXT;
                      nm   : TEXT;
                      rest : TEXT) =
  VAR
    rd := NEW(TextRd.T).init(rest);
    parser := NEW(cmdParseExt.T);
  BEGIN
    Debug.Out(F("HandleModel \"%s\" \"%s\" \"%s\"", type, nm, rest));
    EVAL parser.setLex(NEW(cmdLexExt.T).setRd(rd)).parse();
    EVAL vp.me.lstTab.put(nm,parser.lst)
  END HandleModel;
  
PROCEDURE VPLine(vp : VarParser; ln : TEXT) =
  VAR
    r := NEW(TextReader.T).init(ln);
    tok : TEXT;
    hadW := N(r, tok);
  BEGIN
    Debug.Out("VPLine " & ln);
    IF NOT hadW THEN RETURN END;

    Debug.Out("tok : " & tok);

    IF    TE(tok, "Nmos") OR TE(tok, "Pmos") THEN
      VAR
        nm : TEXT;
        rest : TEXT;
        hadM := N(r, nm);
      BEGIN
        <*ASSERT hadM*>
        rest := r.nextE("");
        HandleModel(vp, tok, nm, rest)
      END
    ELSIF TE(tok, "Parameter") THEN
      WITH haveTerm = N(r, tok) DO
        IF haveTerm THEN DefParam(tok) END
      END
    ELSE
      (* skip *)
    END
  END VPLine;

REVEAL
  T = Public BRANDED Brand OBJECT
    lstTab  : TextRefTbl.T;
  OVERRIDES
    getVars          := GetVars;
    init             := Init;
    havePerturbation := HavePerturbation;
  END;

TYPE
  TextSetFacade = TextSet.T OBJECT
    tbl : TextLRTbl.T OVERRIDES member := TSFMember
  END;

PROCEDURE TSFMember(tsf : TextSetFacade; q : TEXT) : BOOLEAN =
  VAR
    dummy : LONGREAL;
  BEGIN
    RETURN tsf.tbl.get(q, dummy)
  END TSFMember;
  
PROCEDURE GetVars(t : T; fromLib, baseTranTypeName : TEXT) : REF ARRAY OF TEXT =
  VAR
    definedVars := t.makeParamDict(fromLib, baseTranTypeName);
    r : REFANY;
    p : AssignmentList.T;
    dependsOn := NEW(TextSetDef.T).init();
  BEGIN
    IF NOT t.lstTab.get(baseTranTypeName, r) THEN
      Debug.Error(F("Unknown tran type \"%s\"", baseTranTypeName))
    END;
    p := r;
    Debug.Out("HspVariation.GetVars: pre-defined vars from base : " &
      Int(definedVars.size()));
    WHILE p # NIL DO
      WITH defSet    = NEW(TextSetFacade, tbl := definedVars) DO
        p.head.val.getDeps(dependsOn, excluding := defSet)
      END;
      p := p.tail
    END;

    (* remove GeoVars, FixedParams *)

    RemoveParams(dependsOn, GeoVars);
    RemoveParams(dependsOn, FixedParams);
    
    VAR
      iter := dependsOn.iterate();
      nm : TEXT;
      res := NEW(REF ARRAY OF TEXT, dependsOn.size());
      i := 0;
    BEGIN
      WHILE iter.next(nm) DO
        res[i] := nm;
        INC(i)
      END;
      RETURN res
    END
  END GetVars;

PROCEDURE RemoveParams(s : TextSet.T; READONLY a : ARRAY OF TEXT) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      EVAL s.delete(a[i])
    END
  END RemoveParams;

PROCEDURE Init(t         : T;
               fromHsp   : Pathname.T;
               varParser : Variation.VarParser) : Variation.T
  RAISES { Rd.Failure, OSError.E } =
  BEGIN
    <*ASSERT varParser = NIL*> (* ??? *)
    t.lstTab := NEW(TextRefTbl.Default).init();
    RETURN Variation.T.init(t, fromHsp, NEW(VarParser, me := t))
  END Init;

TYPE
  MyPerturbation = Perturbation.Default OBJECT
    ass : Assignment.T;
  OVERRIDES
    calc := MyCalc;
  END;

TYPE
  DictFacade = VarExpr.Getter OBJECT
    dict : TextLRTbl.T;
  OVERRIDES
    v := DFV;
  END;

PROCEDURE DFV(df : DictFacade; nm : TEXT) : LONGREAL =
  VAR
    q : LONGREAL;
  BEGIN
    WITH hadIt = df.dict.get(nm, q) DO
      IF NOT hadIt THEN Debug.Error(F("Unknown variable \"%s\"", nm)) END;
      RETURN q
    END
  END DFV;

PROCEDURE MyCalc(p : MyPerturbation) : LONGREAL =
  VAR
    bv : LONGREAL;
  BEGIN
    WITH hadIt = p.dict.get(p.ass.tgt, bv) DO
      IF NOT hadIt THEN
        Debug.Error(F("Calc'ing perturbation for \"%s\" : no base value!",
                      p.ass.tgt))
      END
    END;
    IF p.ass.pct THEN
      WITH c = p.ass.val.eval(NEW(DictFacade, dict := p.dict)),
           v = c/100.0d0*bv DO
        Debug.Out(F("Pert for %s : pctg. %s base %s pert %s",
                    p.ass.tgt, LR(c), LR(bv), LR(v)));
        RETURN v
      END
    ELSE
      WITH v = p.ass.val.eval(NEW(DictFacade, dict := p.dict)) DO
        Debug.Out(F("Pert for %s : abs. pert %s base %s pct %s %",
                    p.ass.tgt, LR(v), LR(bv), LR(v/bv*100.0d0)));
        RETURN v
      END
    END
  END MyCalc;
  
PROCEDURE HavePerturbation(t                       : T;
                           baseTranTypeName, named : TEXT;
                           VAR                   p : Perturbation.T) : BOOLEAN =
  VAR
    q : AssignmentList.T;
    r : REFANY;
  BEGIN
    IF NOT t.lstTab.get(baseTranTypeName, r) THEN
      Debug.Error("No variation for tran type " & baseTranTypeName)
    END;

    q := r;
    WHILE q # NIL DO
      IF TE(q.head.tgt, named) THEN
        Debug.Out(F("HspVariation.HavePerturbation : Found perturbation for model \"%s\" var \"%s\"",
                    baseTranTypeName, named));
        p := NEW(MyPerturbation, ass := q.head);
        RETURN TRUE
      END;
      q := q.tail
    END;
    RETURN FALSE
  END HavePerturbation;
  
BEGIN END HspVariation.
