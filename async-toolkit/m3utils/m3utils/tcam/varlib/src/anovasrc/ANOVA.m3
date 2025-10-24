(* $Id: ANOVA.m3,v 1.13 2006/03/18 00:18:47 mika Exp $ *)

MODULE ANOVA;

IMPORT ANOVAClass, ANOVASuper;
FROM ANOVAClass IMPORT UnitVector, ComponentMul, Normalize, Dot;

IMPORT FactorialDesign, IndexedDataTbl, FactorialDatumList, LongrealList;
IMPORT IndexedAttachmentsTbl;
IMPORT FactorialIndex;
IMPORT IntSet, IntSetDef;
IMPORT OrthoPoly;
IMPORT FactorialValues, FactorialBindings;
IMPORT TextRd, Fmt;
IMPORT TextLongRealTbl, cmdParseExt, cmdLexExt;
IMPORT RefList;

REVEAL
  T = ANOVASuper.T BRANDED Brand OBJECT
    data : IndexedDataTbl.T;
    attachments : IndexedAttachmentsTbl.T;
    count : CARDINAL := 0;
    design : FactorialDesign.T;
    values : REF ARRAY OF REF ARRAY OF CARDINAL;
    responses : REF ARRAY OF LONGREAL;
  OVERRIDES
    mainEffCoeffs := MainEffCoeffs;
    valueEffCoeffs := ValueEffCoeffs;

    init := Init;
    mapMatching := MapMatching;
    attachmentMapMatching := AttachmentMapMatching;

    errorSS := ErrorSS;
    size := Size;
    slots := Slots;
    varyingIndices := VaryingIndices;
    mapMatchingSamples := MapMatchingSamples;
    levels := Levels;
    typeOf := TypeOf;
    isQuantitative := IsQuantitative;
    interactionEffect := InteractionEffect;
    valueVersusAveEffect := ValueVersusAveEffect;
    numVars := NumVars;
    numValues := NumValues;
    
    matching := Matching;

  END;

PROCEDURE NumValues(t : T) : CARDINAL = 
  BEGIN RETURN NUMBER(t.values^) END NumValues;

TYPE RarrL = REF ARRAY OF LONGREAL;

PROCEDURE Levels(t : T; treatment : CARDINAL) : CARDINAL =
  VAR
    i : FactorialIndex.T;
    p : LongrealList.T;
    iter := t.data.iterate();
    max := 0;
  BEGIN
    WHILE iter.next(i,p) DO
      max := MAX(max, i.index(treatment))
    END;
    RETURN max + 1
  END Levels;

PROCEDURE TypeOf(t : T; idx : CARDINAL) : FactorialValues.Type =
  BEGIN
    RETURN FactorialValues.TypeOf(t.design.values[idx])
  END TypeOf;

PROCEDURE IsQuantitative(t : T; idx : CARDINAL) : BOOLEAN =
  BEGIN
    RETURN t.typeOf(idx) IN 
      SET OF FactorialValues.Type { FactorialValues.Type.Int, 
                                    FactorialValues.Type.LR
                                  };
  END IsQuantitative;
                        
PROCEDURE MapMatchingSamples(t : T; 
                             READONLY treatment, atLevel : ARRAY OF CARDINAL;
                             VAR increment, S, SS : LONGREAL) =
  VAR
    i : FactorialIndex.T;
    p : LongrealList.T;
    iter := t.data.iterate();
    success : BOOLEAN;
  BEGIN
    WHILE iter.next(i,p) DO
      success := TRUE;
      FOR k := FIRST(treatment) TO LAST(treatment) DO
        IF i.index(treatment[k]) # atLevel[k] THEN
          success := FALSE;
          EXIT
        END
      END;
      IF success THEN
        WHILE p # NIL DO
          increment := increment + 1.0d0;
          S := S + p.head;
          SS := S + p.head * p.head;
          p := p.tail
        END
      END
    END
  END MapMatchingSamples;

PROCEDURE VaryingIndices(t : T) : IntSet.T =
  VAR
    f : REF ARRAY OF CARDINAL;
    i : FactorialIndex.T;
    p : LongrealList.T;
    iter := t.data.iterate();
    s := NEW(IntSetDef.T).init();
  BEGIN
    IF NOT iter.next(i,p) THEN
      RETURN s
    END;
    f := i.indices();

    WHILE iter.next(i,p) DO
      VAR
        t := i.indices();
      BEGIN
        FOR p := FIRST(f^) TO LAST(f^) DO
          IF t[p] # f[p] THEN
            EVAL s.insert(p) 
          END
        END
      END
    END;
    RETURN s
  END VaryingIndices;

PROCEDURE Size(t : T) : CARDINAL =  BEGIN RETURN t.count END Size;
 
PROCEDURE Slots(t : T) : CARDINAL = BEGIN RETURN t.data.size() END Slots;

PROCEDURE ErrorSS(t : T) : LONGREAL =
  (* error given last model *)
  VAR
    res := 0.0d0;
    i : FactorialIndex.T;
    p : LongrealList.T;
    iter := t.data.iterate();
  BEGIN
    WHILE iter.next(i,p) DO
      VAR
        sum := 0.0d0;
        n := 0.0d0;
      BEGIN
        WHILE p # NIL DO
          res := res + p.head * p.head;
          sum := sum + p.head;
          n := n + 1.0d0;
          p := p.tail
        END;

        res := res - (sum/n)*sum
      END
    END;
    RETURN res
  END ErrorSS;

PROCEDURE Matching(t : T; READONLY overI, overV : ARRAY OF CARDINAL) : CARDINAL =
  VAR
    n := 0;
    i : FactorialIndex.T;
    p : LongrealList.T;
    iter := t.data.iterate();
    success : BOOLEAN;
  BEGIN
    WHILE iter.next(i,p) DO

      success := TRUE;
      FOR j := FIRST(overI) TO LAST(overI) DO
        IF i.index(overI[j]) # overV[j] THEN
          success := FALSE;
          EXIT
        END
      END;
      IF success THEN
        WHILE p # NIL DO
          INC(n);
          p := p.tail
        END
      END
    END;
    RETURN n
  END Matching;

PROCEDURE MapMatching(t : T; 
                      READONLY overI, overV : ARRAY OF CARDINAL;
                      mapper : Mapper) =
  VAR
    iter := t.data.iterate();
    i : FactorialIndex.T;
    p : LongrealList.T;
    success : BOOLEAN;
  BEGIN
    WHILE iter.next(i,p) DO

      success := TRUE;
      FOR j := FIRST(overI) TO LAST(overI) DO
        IF i.index(overI[j]) # overV[j] THEN
          success := FALSE;
          EXIT
        END
      END;
      IF success THEN
        WHILE p # NIL DO
          mapper.map(p.head); p := p.tail
        END
      END
    END
  END MapMatching;
       
PROCEDURE AttachmentMapMatching(t : T; 
                      READONLY overI, overV : ARRAY OF CARDINAL;
                      mapper : AttachmentMapper) =
  VAR
    iter := t.attachments.iterate();
    i : FactorialIndex.T;
    p : RefList.T := NIL;
    success : BOOLEAN;
  BEGIN
    WHILE iter.next(i,p) DO

      success := TRUE;
      FOR j := FIRST(overI) TO LAST(overI) DO
        IF i.index(overI[j]) # overV[j] THEN
          success := FALSE;
          EXIT
        END
      END;
      IF success THEN
        WHILE p # NIL DO
          mapper.map(p.head); p := p.tail
        END
      END
    END
  END AttachmentMapMatching;

PROCEDURE Init(t : T; design : FactorialDesign.T; 
               data : FactorialDatumList.T;
               responseExpr : TEXT;
               attacher : Attacher) : T =
  BEGIN
    t.design := design;
    t.data := NEW(IndexedDataTbl.Default).init();
    t.attachments := NEW(IndexedAttachmentsTbl.Default).init();

    t.values := NEW(REF ARRAY OF REF ARRAY OF CARDINAL,
                    FactorialDatumList.Length(data));
    t.responses := NEW(REF ARRAY OF LONGREAL, NUMBER(t.values^));

    VAR
      p := data;
      i := 0;
    BEGIN
      WHILE p # NIL DO
        VAR
          s : LongrealList.T := NIL;
          idx := NEW(FactorialIndex.T).init(p.head.vi^);
        BEGIN
          EVAL t.data.get(idx,s);
          INC(t.count);

          (* parse cmd-line expr in curr env. *)

          <* FATAL FactorialBindings.NoSuchVar *>
          VAR
            tbl := NEW(TextLongRealTbl.Default).init();
            parser := NEW(cmdParseExt.T);
            rd := NEW(TextRd.T).init(responseExpr);
            b := design.getIndexedBindings(idx);
          BEGIN
            (* bindings *)
            VAR
              iter := b.iterateAll();
              var : FactorialBindings.Var;
              val : LONGREAL;
            TYPE
              Type = FactorialBindings.Type;
            BEGIN
              WHILE iter.next(var) DO
                CASE var.type OF 
                  Type.Boolean => 
                    IF b.getBool(var.name) THEN
                      val := 1.0d0
                    ELSE
                      val := 0.0d0
                    END
                |
                  Type.Integer => val := FLOAT(b.getInt(var.name),
                                               LONGREAL)
                |
                  Type.LongReal => val := b.getLR(var.name)
                |
                  Type.Text => val := FLOAT(b.getIndex(var.name),
                                            LONGREAL)
                END;
                EVAL tbl.put(var.name,val)
              END
            END;

            (* responses *)
            FOR i := FIRST(design.responses^) TO LAST(design.responses^) DO
              EVAL tbl.put("R" & Fmt.Int(i), p.head.r[i]);
              EVAL tbl.put(design.responses[i], p.head.r[i])
            END;
            parser.symtab := tbl;
            EVAL parser.setLex(NEW(cmdLexExt.T).setRd(rd)).parse();
            t.responses[i] := parser.val;
            s := LongrealList.Cons(parser.val, s);
          END;

          EVAL t.data.put(idx,s);
          IF attacher # NIL THEN
            VAR
              s : RefList.T := NIL;
              b := design.getIndexedBindings(idx);
            BEGIN
              EVAL t.attachments.get(idx,s);

              (* where do the bindings come from? *)

              s := RefList.Cons(attacher.callback(p.head, b),s);
              EVAL t.attachments.put(idx,s)
            END
          END;

          t.values[i] := p.head.vi;

          INC(i)
        END;
        p := p.tail
      END
    END;
    RETURN t
  END Init;

PROCEDURE MainEffCoeffs(t : T; 
                        varIdx : CARDINAL; 
                        order : CARDINAL) : RarrL =
  VAR
    vals := t.design.values[varIdx].n;
    polyvals := NEW(REF ARRAY OF ARRAY OF LONGREAL, vals, vals);
    x := NEW(RarrL, vals);
    w := NEW(RarrL, vals);
    wi := FLOAT(t.size(),LONGREAL)/FLOAT(vals,LONGREAL);

    res := NEW(RarrL, NUMBER(t.responses^));
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      x[i] := t.design.values[varIdx].toLongreal(i);
      w[i] := wi
    END;

    OrthoPoly.Values(x^,w^,polyvals^);

    FOR i := FIRST(t.responses^) TO LAST(t.responses^) DO
      WITH thisp = t.values[i] DO
        res[i] := polyvals[order,thisp[varIdx]]
      END
    END;
    
    RETURN res
  END MainEffCoeffs;

PROCEDURE ValueEffCoeffs(t : T;
                         varIdx : CARDINAL; 
                         value : CARDINAL) : RarrL =
  VAR
    res := NEW(RarrL, NUMBER(t.responses^));
    high := FLOAT(t.levels(varIdx)-1,LONGREAL);
    lo := -1.0d0;
  BEGIN    
    FOR i := FIRST(res^) TO LAST(res^) DO
      IF t.values[i,varIdx] = value THEN
        res[i] := high
      ELSE
        res[i] := lo
      END
    END;
    RETURN res
  END ValueEffCoeffs;
  
PROCEDURE InteractionEffect(t : T;
                        READONLY orders : ARRAY OF Order) : LONGREAL =
  VAR
    coeffs := t.interactionCoeffs(orders);
    totResponse := Dot(t.responses^,coeffs^);
  BEGIN
    RETURN totResponse 
  END InteractionEffect;

PROCEDURE ValueVersusAveEffect(t : T;
                               index, value : CARDINAL) : LONGREAL =
  VAR
    coeffs := UnitVector(NUMBER(t.values^));
  BEGIN
    FOR i := 0 TO t.numVars()-1 DO
      IF i = index THEN
        ComponentMul(t.valueEffCoeffs(i,value)^,
                     coeffs^,
                     coeffs^)
      ELSE
        ComponentMul(t.mainEffCoeffs(i,0)^,
                     coeffs^,
                     coeffs^)
      END
    END;
    Normalize(coeffs^);
    RETURN Dot(t.responses^,coeffs^)
   END ValueVersusAveEffect;

PROCEDURE NumVars(t : T) : CARDINAL = 
  BEGIN RETURN NUMBER(t.design.values^) END NumVars;

BEGIN END ANOVA.





