MODULE SyntaxType;
IMPORT Word;
IMPORT SyntaxTypeSystem;
IMPORT SyntaxTypeSyntaxTypeTbl;
IMPORT Text, Boolean, Cardinal;
IMPORT SyntaxTypeArraySort;

TYPE System = SyntaxTypeSystem.T;
     Array  = ARRAY OF T;

REVEAL
  System = SyntaxTypeSystem.Public BRANDED SyntaxTypeSystem.Brand OBJECT
    tbl : SyntaxTypeSyntaxTypeTbl.T;
  METHODS
    initT(t : T) : T := SystemInitT;
  OVERRIDES
    init := InitSystem;
    makeTerminal := MakeTerminal;
    makeString := MakeString;
    makeOptional := MakeOptional;
    makeList := MakeList;
    makeDisjunction := MakeDisjunction;
    makeSequence := MakeSequence;
    emptyDisjunction := EmptyDisjunction;
    emptySequence := EmptySequence;
  END;

PROCEDURE SystemInitT(system : System; t : T) : T =
  BEGIN
    IF NOT system.tbl.get(t, t) THEN
      t.hashV := t.hash();
      EVAL system.tbl.put(t, t)
    END;
    RETURN t
  END SystemInitT;

PROCEDURE InitSystem(system : System) : System =
  BEGIN
    system.tbl := NEW(SyntaxTypeSyntaxTypeTbl.Default,
                      keyEqual := DeepEqual).init();
    RETURN system
  END InitSystem;

PROCEDURE DeepEqual(<*UNUSED*>tbl : SyntaxTypeSyntaxTypeTbl.T;
                    READONLY a, b : T) : BOOLEAN =
  (* 
     This is used ONLY by system.tbl

     this allows system.tbl to do a check whether we already have the
     same structure, allowing clients of our interface to use 
     reference equality (exported as Equal, below)
  *)
  BEGIN
    RETURN Compare(a, b) = 0
  END DeepEqual;
  
REVEAL
  T = BRANDED Brand OBJECT
    hashV : Word.T := 0;
    system : System;
  METHODS
    hash() : Word.T;
    equal(a : T) : BOOLEAN;
  END;
  
TYPE
  Terminal = T BRANDED OBJECT
    type : TEXT;
  OVERRIDES
    hash  := HashTerminal;
  END;

  String = T BRANDED OBJECT
    string : TEXT;
  OVERRIDES
    hash  := HashString;
  END;

  Optional = T BRANDED OBJECT
    of : T;
  OVERRIDES
    hash  := HashOptional;
  END;
  
  List = T BRANDED OBJECT
    of : T;
    emptyOk : BOOLEAN;
  OVERRIDES
    hash  := HashList;
  END;

  Disjunction = T BRANDED OBJECT
    x : REF ARRAY OF T;
  OVERRIDES
    hash  := HashDisjunction;
  END;

  Sequence = T BRANDED OBJECT
    x : REF ARRAY OF T;
  OVERRIDES
    hash  := HashSequence;
  END;

PROCEDURE HashTerminal(a : Terminal) : Word.T =
  BEGIN RETURN Text.Hash(a.type) END HashTerminal;

PROCEDURE HashString(a : String) : Word.T =
  BEGIN RETURN Text.Hash(a.string) END HashString;

PROCEDURE HashOptional(a : Optional) : Word.T =
  BEGIN RETURN Word.Times(17, a.of.hash()) END HashOptional;

PROCEDURE HashList(a : List) : Word.T =
  BEGIN RETURN Word.Times(11, a.of.hash()) END HashList;

PROCEDURE HashDisjunction(a : Disjunction) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a.x^) TO LAST(a.x^) DO
      res := Word.Plus(res, Word.Times(3, a.x[i].hash()))
    END;
    RETURN res
  END HashDisjunction;

PROCEDURE HashSequence(a : Sequence) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a.x^) TO LAST(a.x^) DO
      res := Word.Plus(res, Word.Times(7, a.x[i].hash()))
    END;
    RETURN res
  END HashSequence;

PROCEDURE Compare(a, b : T) : [-1..1] =
  (* DeepEqual depends on this being a full check *)
  BEGIN
    WITH aTc = TYPECODE(a),
         bTc = TYPECODE(b) DO
      IF    aTc < bTc THEN
        RETURN -1
      ELSIF aTc > bTc THEN
        RETURN +1
      ELSE
        TYPECASE a OF
          Terminal(at) =>
          WITH bt = NARROW(b, Terminal) DO
            RETURN Text.Compare(at.type, bt.type)
          END
        |
          String(at) =>
          WITH bt = NARROW(b, String) DO
            RETURN Text.Compare(at.string, bt.string)
          END
        |
          Optional(at) =>
          WITH bt = NARROW(b, Optional) DO
            RETURN Compare(at.of, bt.of)
          END
        |
          List(at) =>
          WITH bt = NARROW(b, List),
               ae = at.emptyOk,
               be = bt.emptyOk,
               ec = Boolean.Compare(ae, be) DO
            IF ec # 0 THEN
              RETURN ec
            ELSE
              RETURN Compare(at.of, bt.of)
            END
          END
        |
          Disjunction(at) =>
          WITH bt = NARROW(b, Disjunction) DO
            WITH cc = Cardinal.Compare(NUMBER(at.x^), NUMBER(bt.x^)) DO
              IF cc # 0 THEN
                RETURN cc
              ELSE
                FOR i := FIRST(at.x^) TO LAST(at.x^) DO
                  WITH dd = Compare(at.x[i], bt.x[i]) DO
                    IF dd # 0 THEN RETURN dd END
                  END
                END
              END
            END;
            RETURN 0
          END
          |
          Sequence(at) =>
          WITH bt = NARROW(b, Sequence) DO
            WITH cc = Cardinal.Compare(NUMBER(at.x^), NUMBER(bt.x^)) DO
              IF cc # 0 THEN
                RETURN cc
              ELSE
                FOR i := FIRST(at.x^) TO LAST(at.x^) DO
                  WITH dd = Compare(at.x[i], bt.x[i]) DO
                    IF dd # 0 THEN RETURN dd END
                  END
                END
              END
            END;
            RETURN 0
          END                      
        ELSE
          <*ASSERT FALSE*>
        END
      END
    END
  END Compare;
          
PROCEDURE MakeTerminal(system : System; term : TEXT) : T =
  BEGIN
    RETURN system.initT(NEW(Terminal, system := system, type := term))
  END MakeTerminal;

PROCEDURE MakeString(system : System; string : TEXT) : T =
  BEGIN
    RETURN system.initT(NEW(String, system := system, string := string))
  END MakeString;

PROCEDURE MakeOptional(system : System; of : T) : T =
  BEGIN
    RETURN system.initT(NEW(Optional, system := system, of := of))
  END MakeOptional;

PROCEDURE MakeList(system : System; of : T; emptyOk : BOOLEAN) : T =
  BEGIN
    RETURN system.initT(NEW(List, system := system, of := of, emptyOk := emptyOk))
  END MakeList;

PROCEDURE MakeDisjunction(s : System; a, b : T) : T =
  VAR
    x : REF ARRAY OF T;
  BEGIN
    IF ISTYPE(a, Disjunction) AND ISTYPE(b, Disjunction) THEN
      WITH ad = NARROW(a, Disjunction),
           bd = NARROW(b, Disjunction) DO
        x := NEW(REF Array, NUMBER(ad.x^) + NUMBER(bd.x^));
        SUBARRAY(x^,             0, NUMBER(ad.x^)) := ad.x^;
        SUBARRAY(x^, NUMBER(ad.x^), NUMBER(bd.x^)) := bd.x^;
      END
    ELSIF ISTYPE(b, Disjunction) THEN
      RETURN MakeDisjunction(s, b, a)
    ELSIF ISTYPE(a, Disjunction) THEN
      WITH ad = NARROW(a, Disjunction) DO
        x := NEW(REF Array, NUMBER(ad.x^) + 1);
        SUBARRAY(x^, 1, NUMBER(ad.x^)) := ad.x^;
        x[0] := b
      END
    ELSE
      x := NEW(REF Array, 2);
      x[0] := a;
      x[1] := b
    END;
      
    SyntaxTypeArraySort.Sort(x^);
    
    RETURN s.initT(NEW(Disjunction, system := s, x := x))
  END MakeDisjunction;

PROCEDURE MakeSequence(s : System; a, b : T) : T =
  VAR
    x : REF Array;
  BEGIN
    IF    ISTYPE(a, Disjunction) THEN
      (* fill in *)
      WITH ad = NARROW(a, Disjunction) DO
        VAR
          res := EmptyDisjunction(s);
        BEGIN
          FOR i := FIRST(ad.x^) TO LAST(ad.x^) DO
            res := MakeDisjunction(s,
                                   res,
                                   MakeSequence(s, ad.x[i], b))
          END;
          RETURN res
        END
      END
    ELSIF ISTYPE(b, Disjunction) THEN
      WITH bd = NARROW(b, Disjunction) DO
        VAR
          res := EmptyDisjunction(s);
        BEGIN
          FOR i := FIRST(bd.x^) TO LAST(bd.x^) DO
            res := MakeDisjunction(s,
                                   res,
                                   MakeSequence(s, a, bd.x[i]))
          END;
          RETURN res
        END
      END
    ELSIF ISTYPE(a, Sequence) AND ISTYPE(b, Sequence) THEN
      WITH ad = NARROW(a, Sequence),
           bd = NARROW(b, Sequence) DO
        x := NEW(REF Array, NUMBER(ad.x^) + NUMBER(bd.x^));
        SUBARRAY(x^,             0, NUMBER(ad.x^)) := ad.x^;
        SUBARRAY(x^, NUMBER(ad.x^), NUMBER(bd.x^)) := bd.x^;
      END
    ELSIF ISTYPE(b, Sequence) THEN
      RETURN MakeSequence(s, b, a)
    ELSIF ISTYPE(a, Sequence) THEN
      WITH ad = NARROW(a, Sequence) DO
        x := NEW(REF Array, NUMBER(ad.x^) + 1);
        SUBARRAY(x^, 1, NUMBER(ad.x^)) := ad.x^;
        x[0] := b
      END
    ELSE
      x := NEW(REF Array, 2);
      x[0] := a;
      x[1] := b
    END;

    RETURN s.initT(NEW(Sequence, system := s, x := x))
  END MakeSequence;

PROCEDURE EmptyDisjunction(s : System) : T =
  BEGIN
    RETURN s.initT(NEW(Disjunction, system := s, x := NEW(REF Array, 0)))
  END EmptyDisjunction;

PROCEDURE EmptySequence(s : System) : T =
  BEGIN
    RETURN s.initT(NEW(Sequence, system := s, x := NEW(REF Array, 0)))
  END EmptySequence;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN
    IF a.hashV = 0 THEN a.hashV := a.hash() END;
    RETURN a.hashV 
  END Hash;

BEGIN END SyntaxType.
