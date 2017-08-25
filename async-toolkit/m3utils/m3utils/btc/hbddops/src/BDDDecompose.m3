(* $Id: BDDDecompose.m3,v 1.3 2015/02/26 05:02:16 mika Exp $ *)

MODULE BDDDecompose;
IMPORT BDD, BDDDepender, BDDPair;
IMPORT Debug;
IMPORT Random;
IMPORT Word;
IMPORT Fmt; FROM Fmt IMPORT F, Int;
IMPORT CardPair, CardSet, CardList, CardSetDef, CardPairRefTbl;
IMPORT BDDTruthTable;
FROM BDDOpsH IMPORT XFormat, Substitute, MakeCntrBdd;
FROM BDDSetToList IMPORT SetToArr;
FROM BoolArrayOps IMPORT MaxTwoDiffCols, ColsEq;

VAR doDebug := FALSE AND (Debug.GetLevel() > 5);

REVEAL
  T = Public BRANDED Brand OBJECT
    dep : BDDDepender.T;
  OVERRIDES
    init     := Init;
    attempt  := Attempt;
    minterms := Minterms;
  END;

PROCEDURE Init(t : T; dep : BDDDepender.T) : T =
  BEGIN t.dep := dep; RETURN t END Init;

PROCEDURE Dbg(z : TEXT) = BEGIN Debug.Out(z, 5) END Dbg;

TYPE Pair = BDDPair.T;

TYPE VResult = RECORD set : CardSet.T; tt : BDDTruthTable.T END;

PROCEDURE Attempt(t : T; x : BDD.T; pfx : TEXT) : Result =

  PROCEDURE RecurseBuild(r : Result) : BDD.T = 
    BEGIN
      IF doDebug THEN
        Dbg("Rebuild r.x = " & XFormat(r.x));
        Dbg("Rebuild r.v = " & XFormat(r.v))
      END;
      IF r.v = NIL THEN
        RETURN r.x 
      ELSE
        RETURN Substitute(RecurseBuild(r.next), r.v, r.x)
      END
    END RecurseBuild;

  BEGIN 
    WITH res = AttemptInternal(t, x, pfx, 0),
         z   = RecurseBuild(res) DO
      IF doDebug THEN
        Dbg("Attempt x = " & XFormat(x));
        Dbg("Attempt z = " & XFormat(z))
      END;
      <*ASSERT x=z*>
      RETURN res
    END
  END Attempt;

PROCEDURE AttemptInternal(t : T; 
                          x : BDD.T; 
                          pfx : TEXT; 
                          pc : CARDINAL) : Result =
  PROCEDURE SetLit(x : BDD.T; i : CARDINAL; to : [0..1]) : BDD.T =
    VAR
      lit := arr[i];
    BEGIN
      CASE to OF
        0 => RETURN BDD.MakeFalse(x, lit)
      |
        1 => RETURN BDD.MakeTrue(x, lit)
      END
    END SetLit;

  PROCEDURE PickRandom() =
    BEGIN FOR l := 0 TO sz-1 DO c[l] := rand.integer(0,1) END END PickRandom;

  PROCEDURE DoRandom(READONLY i, j, m : CARDINAL) : BOOLEAN =

    CONST 
      Probes = 10 (* 10 *); (* Knuth's t constant *)
    VAR
      r, s : BDD.T;
      res : ARRAY [0..7] OF BDD.T;
      pairs : ARRAY [0..3] OF BDDPair.T;
      good : BOOLEAN;
    BEGIN
      IF FALSE AND doDebug THEN 
        Dbg(F("Do(%s,%s,%s)", Int(i), Int(j), Int(m)))
      END;

      FOR cnt := 0 TO Probes-1 DO
        PickRandom();
        
        r := x;

        FOR k := 0 TO sz-1 DO
          IF k # i AND k # j AND k # m THEN
            r := SetLit(r, k, c[k])
          END
        END;

        
        IF FALSE AND doDebug THEN 
          Dbg("Probing with " & FmtV(c^)  & " constant part " & XFormat(r)) 
        END;

        FOR z := 0 TO 7 DO
          WITH ix = Word.Extract(z, 2, 1), 
               jx = Word.Extract(z, 1, 1),
               mx = Word.Extract(z, 0, 1) DO

            s := SetLit(SetLit(SetLit(r, i, ix), j, jx), m, mx);

            IF FALSE AND doDebug THEN 
              Dbg("Testing " & Int(z) & " val= " & XFormat(s))
            END;
            
            res[z] := s
          END
        END;

        FOR k := FIRST(pairs) TO LAST(pairs) DO
          pairs[k] := Pair { res[2*k], res[2*k+1] };
          
          IF FALSE AND doDebug THEN
            Dbg("pair " & Int(k) & " : " & XFormat(pairs[k][0]) & " " &
                                           XFormat(pairs[k][1]))
          END;
        END;

        (* how many distinct pairs are there? *)

        good := TRUE;

        FOR pp := 1 TO LAST(pairs) DO
          IF pairs[pp] # pairs[0] THEN
            FOR qq := pp+1 TO LAST(pairs) DO
              IF pairs[qq] # pairs[0] AND pairs[qq] # pairs[pp] THEN
                good := FALSE
              END
            END
          END
        END;

        IF FALSE AND doDebug THEN
          Dbg("pairs good = " & Fmt.Bool(good))
        END;

        IF NOT good THEN EXIT END
      END;
      RETURN good
    END DoRandom;

  PROCEDURE FmtV(READONLY c : ARRAY OF [0..1]) : TEXT =
    VAR txt := ""; BEGIN
      FOR l := 0 TO sz-1 DO txt := txt & Int(c[l]) END;
      RETURN txt
    END FmtV;


  PROCEDURE DoIJ(READONLY i, j : CARDINAL) : CardList.T =
    VAR
      bad : CardList.T := NIL;
    BEGIN
      FOR m := 0 TO sz-1 DO
        IF m # i AND m # j THEN
          IF NOT DoRandom(i,j,m) THEN bad := CardList.Cons(m, bad) END
        END
      END; (* FOR m *)
      
      IF doDebug THEN
        IF bad # NIL THEN
          VAR p := bad; 
              impl, bmpl := ""; 
          BEGIN
            WHILE p # NIL DO 
              impl := impl & " " & Int(p.head); 
              bmpl := bmpl & " " & BDD.Format(arr[p.head]); 
              
              p := p.tail 
            END;
            Dbg(Int(i) & " " &  Int(j) & "=>" & impl);
            Dbg(BDD.Format(arr[i]) & " " &  BDD.Format(arr[j]) & "=>" & bmpl)
          END
        END
      END;
      
      RETURN bad
    END DoIJ;

  PROCEDURE Grow(set : CardSet.T; a, b : CARDINAL) =
    VAR r : REFANY;
    BEGIN
      <*ASSERT a < b*>
      EVAL set.insert(a); EVAL set.insert(b);
      IF badTbl.get(CardPair.T { a, b }, r) THEN
        VAR 
          p : CardList.T := r;
          c : CARDINAL;
        BEGIN
          WHILE p # NIL DO
            IF NOT set.insert(p.head) THEN
              WITH iter = set.iterate() DO
                WHILE iter.next(c) DO
                  IF c # p.head THEN
                    Grow(set, MIN(c,p.head), MAX(c,p.head))
                  END
                END
              END
            END;
            p := p.tail
          END
        END
      END
    END Grow;

  PROCEDURE LookForBadPairs() = 
    BEGIN
      FOR i := 0 TO sz-2 DO
        FOR j := i+1 TO sz-1 DO
          WITH badlst = DoIJ(i,j) DO
            IF badlst # NIL THEN 
              EVAL badTbl.put(CardPair.T {i , j}, badlst) 
            END
          END
        END
      END
    END LookForBadPairs;

  PROCEDURE VerifyBadPairs() : VResult =
    BEGIN
      FOR i := 0 TO sz-2 DO
        FOR j := i+1 TO sz-1 DO
          WITH set = NEW(CardSetDef.T).init() DO
            Grow(set, i, j);
            
            WITH isCand = set.size() < sz DO
              
              IF doDebug THEN
                Dbg("Grow: " & Int(i) & " " & Int(j) & " -> " & 
                  FmtSet(set) & " isCand=" & Fmt.Bool(isCand))
              END;

              IF isCand THEN
                WITH tt = BDDTruthTable.Make(x, arr^, set) DO
                  IF MaxTwoDiffCols(tt.tt^) THEN
                    IF doDebug THEN
                      Dbg("Truth table is OK.")
                    END;
                    RETURN VResult { set, tt }
                  END
                END
              END


            END
          END
        END
      END;

      RETURN VResult { NEW(CardSetDef.T).init(), 
                       BDDTruthTable.T { NIL,NIL,NIL}}
      (* empty set *)

    END VerifyBadPairs;

  VAR
    deps := t.dep.depends(x);
    sz   := deps.size();
    arr  := SetToArr(deps);
    c    := NEW(REF ARRAY OF [0..1], sz);
    rand := NEW(Random.Default).init(fixed := TRUE);
    badTbl := NEW(CardPairRefTbl.Default).init();
  BEGIN

    IF doDebug THEN
      Dbg("===== Attempting to decompose " & XFormat(x));
      FOR i := 0 TO sz-1 DO
        Dbg("depends on " & XFormat(arr[i]))
      END
    END;

    LookForBadPairs();

    WITH vr = VerifyBadPairs() DO
      IF vr.set.size() # 0 THEN
        IF doDebug THEN 
          Dbg("Verified decomposable subset of size " & 
            Fmt.Int(vr.set.size()));
          VAR iter := vr.set.iterate(); 
              c : CARDINAL;
          BEGIN
            WHILE iter.next(c) DO Dbg(XFormat(arr[c])) END
          END
        END;

        RETURN ConvertResult(t, vr, pfx, pc)
      END
      
    END;

    RETURN NEW(Result, x := x, v := NIL, next := NIL)
  END AttemptInternal;

PROCEDURE ConvertResult(t    : T;
                        vr   : VResult; 
                        pfx  : TEXT; 
                        pc   : CARDINAL) : Result =

  PROCEDURE MakeBddFromRows(col : CARDINAL) : BDD.T =
    VAR
      res := BDD.False();
    BEGIN
      FOR row := 0 TO Word.LeftShift(1,NUMBER(vr.tt.rowVars^))-1 DO
        IF vr.tt.tt[row,col] THEN
          WITH rowBdd = MakeCntrBdd(vr.tt.rowVars^, row) DO
            
            IF FALSE AND doDebug THEN
              Dbg("col " & Fmt.Int(col) & 
                " ORing in rowBdd = " & XFormat(rowBdd))
            END;
            res := BDD.Or(res, rowBdd)
          END
        END
      END;
      RETURN res
    END MakeBddFromRows;

  VAR 
    colZero := BDD.False();
    colOne : BDD.T;
    colOneIdx := 0;
  BEGIN
    (* at this point we have at most two types of columns in the tt *)
    
    FOR i := FIRST(vr.tt.tt[0]) TO LAST(vr.tt.tt[0]) DO
      IF ColsEq(vr.tt.tt^, i, 0) THEN
        colZero := BDD.Or(colZero, MakeCntrBdd(vr.tt.colVars^, i))
      ELSE
        colOneIdx := i
      END
    END;
    colOne := BDD.Not(colZero);
    WITH colOneRows  = MakeBddFromRows(colOneIdx),
         colZeroRows = MakeBddFromRows(0),
         bit         = BDD.New(pfx & Fmt.Int(pc)),
         x           = BDD.Or(BDD.And(colOneRows,bit),
                              BDD.And(colZeroRows, BDD.Not(bit))) DO

      IF doDebug THEN
        Dbg("colZero     = " & XFormat(colZero));
        Dbg("colOne      = " & XFormat(colOne));
        Dbg("colZeroRows = " & XFormat(colZeroRows));
        Dbg("colOneRows  = " & XFormat(colOneRows));
        Dbg("bit         = " & XFormat(bit));
        Dbg("x           = " & XFormat(x))
      END;

      RETURN
        NEW(Result, x := colOne, v := bit,
                    next := AttemptInternal(t, x, pfx, pc + 1))
    END
  END ConvertResult;

PROCEDURE FmtSet(s : CardSet.T) : TEXT =
  VAR r := ""; 
      c : CARDINAL;
      iter := s.iterate();
  BEGIN
    WHILE iter.next(c) DO
      r := r & Int(c) & " " 
    END;
    RETURN r
  END FmtSet;

BEGIN END BDDDecompose.
