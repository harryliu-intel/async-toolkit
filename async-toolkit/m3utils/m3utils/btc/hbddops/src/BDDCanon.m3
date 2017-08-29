(* $Id$ *)

MODULE BDDCanon;
IMPORT BDD;
IMPORT BDDDepender;
FROM BDDOpsH IMPORT XFormat;
IMPORT RefList;
IMPORT Debug;
IMPORT BDDSet;
IMPORT Fmt; FROM Fmt IMPORT Int, F;
IMPORT BDDOpsH;
IMPORT Word;

VAR doDebug := TRUE AND (Debug.GetLevel() > 5);

PROCEDURE Dbg(z : TEXT) = BEGIN Debug.Out(z, 5) END Dbg;

REVEAL
  T = Public BRANDED Brand OBJECT
    exprs : RefList.T;
    dep  : BDDDepender.T;
    xc, vc := 0;
    invertIn, invertOut : BOOLEAN;
  OVERRIDES
    init := Init;
    add  := Add;
    size := Size;
  END;

PROCEDURE Size(t : T) : CARDINAL = 
  BEGIN RETURN RefList.Length(t.exprs) END Size;
  
TYPE
  Rec = OBJECT
    b        : BDD.T;
    vars     : REF ARRAY OF BDD.T;
    inVert   : REF ARRAY OF BOOLEAN;
    outVert  : BOOLEAN;
  END;

PROCEDURE Init(t : T; invertIn, invertOut : BOOLEAN) : T =
  BEGIN
    t.invertIn := invertIn;
    t.invertOut := invertOut;
    t.exprs := NIL;
    t.dep := NEW(BDDDepender.T).init();
    RETURN t
  END Init;

PROCEDURE Add(t : T; x : BDD.T) : Canonical =
  VAR
    deps := t.dep.depends(x);
    p := t.exprs;
    canon : Canonical;
  BEGIN
    WHILE p # NIL DO
      WITH rec = NARROW(p.head, Rec) DO
        IF deps.size() = NUMBER(rec.vars^) THEN
          IF Matches(t, x, deps, rec, canon) THEN RETURN canon END
        END
      END;
      p := p.tail
    END;
    WITH new = MakeRec(t, x, deps, canon) DO
      t.exprs := RefList.Cons(new, t.exprs);
      RETURN canon
    END
  END Add;

PROCEDURE MakeRec(t         : T; 
                  x         : BDD.T; 
                  deps      : BDDSet.T; 
                  VAR canon : Canonical) : Rec =
  VAR
    xv := ToArray(deps);
    nv := NEW(REF ARRAY OF BDD.T, NUMBER(xv^));
  BEGIN
    FOR i := FIRST(nv^) TO LAST(nv^) DO
      nv[i] := BDD.New(F("canon_%s_%s", Int(t.xc), Int(i)))
    END;
    INC(t.xc);

    WITH canonB = SubstituteArray(xv^, nv^, x, NoInversion),
         ea = NEW(REF ARRAY OF BOOLEAN, NUMBER(xv^)) DO

      IF doDebug THEN
        Dbg("New canonical expression = " & XFormat(canonB))
      END;

      FOR i := FIRST(ea^) TO LAST(ea^) DO ea[i] := FALSE END;
      
      canon := NEW(Canonical, 
                   canon := canonB, 
                   canLit := nv, 
                   actLit := xv,
                   outVert := FALSE,
                   inVert := ea);
      RETURN NEW(Rec, vars := nv, b := canonB)
    END
  END MakeRec;

PROCEDURE ToArray(deps : BDDSet.T) : REF ARRAY OF BDD.T =
  VAR
    arr := NEW(REF ARRAY OF BDD.T, deps.size());
    iter := deps.iterate();
  BEGIN
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      EVAL iter.next(arr[i])
    END;
    RETURN arr
  END ToArray;

PROCEDURE NextInversion(t       : T; 
                        ni      : CARDINAL;
                        VAR inv : Inversion) : BOOLEAN =
  VAR
    incr : [1..2];
    next : Inversion;
  BEGIN
    IF t.invertOut THEN incr := 1 ELSE incr := 2 END;
    
    next := inv + incr;

    IF NOT t.invertIn AND next > 1 THEN RETURN FALSE END;

    IF next = Word.LeftShift(1, ni + 1) THEN RETURN FALSE END;

    inv := next;

    RETURN TRUE
  END NextInversion;

TYPE Inversion = Word.T;
CONST NoInversion : Inversion = 0;

PROCEDURE InputInverted(inversion : Inversion; i : CARDINAL) : BOOLEAN =
  BEGIN
    RETURN Word.Extract(inversion, i+1, 1) = 1
  END InputInverted;

PROCEDURE OutputInverted(inversion : Inversion) : BOOLEAN =
  BEGIN
    RETURN Word.Extract(inversion, 0, 1) = 1
  END OutputInverted;

PROCEDURE ExtractInVert(n         : CARDINAL;
                        inversion : Inversion) : REF ARRAY OF BOOLEAN =
  BEGIN
    WITH res = NEW(REF ARRAY OF BOOLEAN, n) DO
      FOR i := 0 TO n-1 DO
        res[i] := InputInverted(inversion, i)
      END;
      RETURN res
    END
  END ExtractInVert;

PROCEDURE Matches(t         : T;
                  x         : BDD.T;
                  deps      : BDDSet.T;
                  rec       : Rec;
                  VAR canon : Canonical) : BOOLEAN =

  PROCEDURE InitialPermutation() =
    BEGIN
      FOR i := FIRST(iArr^) TO LAST(iArr^) DO
        iArr[i] := i
      END
    END InitialPermutation;

  PROCEDURE InitialInversion() = 
    BEGIN inversion := NoInversion END InitialInversion;

  VAR
    xArr := ToArray(deps);
    cArr := rec.vars;

    tArr := NEW(REF ARRAY OF BDD.T, NUMBER(xArr^));
    
    iArr := NEW(REF ARRAY OF CARDINAL, NUMBER(xArr^));

    inversion : Inversion;

  BEGIN
    InitialPermutation();

    LOOP
      PermuteArray(xArr^, iArr^, tArr^);
      InitialInversion();
      LOOP
        IF SubstituteMatch(tArr^, cArr^, x, rec.b, inversion) THEN
          canon := NEW(Canonical, 
                       canon := rec.b, 
                       canLit := cArr,
                       actLit := tArr,
                       inVert := ExtractInVert(NUMBER(xArr^), inversion),
                       outVert := OutputInverted(inversion));
          RETURN TRUE
        END;
        IF NOT NextInversion(t, NUMBER(xArr^), inversion) THEN EXIT END
      END;
      IF NOT NextPermutation(iArr^) THEN RETURN FALSE END
    END
  END Matches;

PROCEDURE NextPermutation(VAR s : ARRAY OF CARDINAL) : BOOLEAN =
  VAR
    i, j : [-1..LAST(CARDINAL)];
  BEGIN
    i := -1;
    FOR k := LAST(s)-1 TO FIRST(s) BY -1 DO
      IF s[k] < s[k+1] THEN i := k; EXIT END
    END;
    IF i = -1 THEN RETURN FALSE END;
    
    FOR k := LAST(s) TO i+1 BY -1 DO
      IF s[k] > s[i] THEN j := k; EXIT END
    END;
    
    VAR t := s[j]; BEGIN s[j] := s[i]; s[i] := t END; (* swap *)
    
    FOR k := i+1 TO (i+1+LAST(s)) DIV 2 DO
      WITH z = k-(i+1), l = LAST(s)-z DO
        VAR t := s[k]; BEGIN s[k] := s[l]; s[l] := t END
      END
    END;
    
    RETURN TRUE
  END NextPermutation;

PROCEDURE PermuteArray(READONLY a : ARRAY OF BDD.T;
                       READONLY c : ARRAY OF CARDINAL;
                       VAR      o : ARRAY OF BDD.T) =
  BEGIN
    IF doDebug THEN
      VAR str := "";
      BEGIN
        
        FOR i := FIRST(c) TO LAST(c) DO
          str := str & Int(c[i]) & " "
        END;
        Dbg("Permutation " & str)
      END
    END;

    FOR i := FIRST(c) TO LAST(c) DO
      o[i] := a[c[i]]
    END
  END PermuteArray;

PROCEDURE SubstituteMatch(READONLY t, c : ARRAY OF BDD.T; 
                          x             : BDD.T;
                          b             : BDD.T;
                          inversion     : Inversion) : BOOLEAN =
  BEGIN 
    WITH y = SubstituteArray(t, c, x, inversion),
         r = (y = b) DO
      IF doDebug THEN
        Dbg("SubstituteMatch inv = " & Int(inversion));
        Dbg("SubstituteMatch x   = " & XFormat(x));
        Dbg("SubstituteMatch y   = " & XFormat(y));
        Dbg("SubstituteMatch b   = " & XFormat(b));
        Dbg("SubstituteMatch r   = " & Fmt.Bool(r))
      END;
      RETURN r
    END
  END SubstituteMatch;

PROCEDURE SubstituteArray(READONLY t, c : ARRAY OF BDD.T; 
                          x             : BDD.T;
                          inversion     : Inversion) : BDD.T =
  VAR
    y := x;
    n : BDD.T;
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO
      IF InputInverted(inversion, i) THEN
        n := BDD.Not(c[i]) 
      ELSE
        n := c[i]
      END;
      y := BDDOpsH.Substitute(y, t[i], n)
    END;
    IF OutputInverted(inversion) THEN
      RETURN BDD.Not(y) 
    ELSE
      RETURN y 
    END
  END SubstituteArray;

BEGIN END BDDCanon.
