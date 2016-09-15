MODULE Main;

IMPORT CSVParse;
IMPORT Rd, FileRd;
IMPORT Debug;
FROM Fmt IMPORT F;
IMPORT Fmt;
IMPORT Scan;
IMPORT Text;
IMPORT Pathname;
IMPORT Lex, FloatMode;
IMPORT SBAddress AS Address;
IMPORT Range;
IMPORT SortedRangeRefTbl AS SortedRangeTbl;
IMPORT TextRangeTblTbl;
IMPORT BDD; FROM BDD IMPORT False, True;
IMPORT TextBDDTbl;
IMPORT BDDBDDTbl;
IMPORT Word;
FROM BDDOpsH IMPORT XFormat;
IMPORT BDDSet, BDDSetDef;
IMPORT SopBDD, SopBDDRep;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT SortedRangeTextTbl;
IMPORT RefList;
IMPORT Wr, FileWr, OSError;
IMPORT Thread;
IMPORT TextUtils;
IMPORT TextWr;
IMPORT AL;
IMPORT TextCardTbl, TextSeq;

<*FATAL Thread.Alerted*>

<*NOWARN*>CONST DefFn = "/p/hlp/yuqisong/hlp-pg-rtl/hlp-work/src/srdl/policy_group.csv";
CONST TE = Text.Equal;
      
VAR
  rd : Rd.T;

TYPE
  Field = { Name, Base, Length, Group };

CONST
  (* the following array is peculiar to HLP.  Should add facility to
     read the PGs from elsewhere for more flexibility. *)
  PolicyGroupArr = ARRAY [0..7] OF TEXT
  { "PG0",
    "PG1",
    "PG2",
    "PG3",
    "PG4",
    "PG5",
    "PG6",
    "PG7" };
  DefaultIdx = LAST(PolicyGroupArr) + 1;
  
TYPE
  (*PolicyGroupIdx = [FIRST(PolicyGroupArr) .. LAST(PolicyGroupArr)];*)
  ExtPolicyGroupIdx = [FIRST(PolicyGroupArr)  .. DefaultIdx];

PROCEDURE MapPGnameToNumber(str : TEXT) : ExtPolicyGroupIdx =
  BEGIN
    IF TE(str, "PG_DEFAULT") THEN
      RETURN DefaultIdx
    END;

    FOR i := FIRST(PolicyGroupArr) TO LAST(PolicyGroupArr) DO
      IF TE(str, PolicyGroupArr[i]) THEN RETURN i END
    END;

    Debug.Error("Unknown policy group \"" & str & "\" : cannot map");
    <*ASSERT FALSE*>
  END MapPGnameToNumber;
  
PROCEDURE Int16(x : INTEGER) : TEXT =
  BEGIN RETURN Fmt.Int(x, base := 16) END Int16;

CONST
  FieldNames = ARRAY Field OF TEXT { "name", "base", "length", "policy_group" };
  (* should assert these are present on line 1 *)

VAR
  pgs := NEW(TextRangeTblTbl.Default).init();
  allRanges : SortedRangeTbl.T := NewRL();
  reverse := NEW(SortedRangeTextTbl.Default).init();

PROCEDURE NewRL() : SortedRangeTbl.T =
  BEGIN RETURN NEW(SortedRangeTbl.Default).init() END NewRL;

PROCEDURE InsertOrdered(p : SortedRangeTbl.T; READONLY r : Range.T) =
  BEGIN
    (* we should check for clean and dirty overlap cases here and behave
       accordingly *)
    EVAL p.put(r, r.group)
  END InsertOrdered;
  
VAR totLen := 0;

    widest := ARRAY Field OF CARDINAL { 0 , .. };
    
PROCEDURE ProcessBuf(b : ARRAY Field OF TEXT) =
  VAR
    rl : SortedRangeTbl.T;
  BEGIN
    IF Debug.GetLevel() >= 100 THEN
      FOR f := FIRST(Field) TO LAST(Field) DO
        Debug.Out(F("ProcessBuf : %s : %s", FieldNames[f], b[f]))
      END
    END;

    FOR f := FIRST(Field) TO LAST(Field) DO
      WITH l = Text.Length(b[f]) DO
        IF l > widest[f] THEN
          Debug.Out(F("widest %s <- %s", FieldNames[f], b[f]));
          widest[f] := l
        END
      END
    END;

    WITH base = ParseLiteral(b[Field.Base]),
         len  = ParseLiteral(b[Field.Length]),
         grp  =              b[Field.Group]    DO
      IF Debug.GetLevel() >= 100 THEN
        Debug.Out(F("range start %s len %s pg %s",
                    Int16(base),
                    Int16(len),
                    grp))
      END;
      EVAL MapPGnameToNumber(grp); (* ensure we can map group *)
      INC(totLen, len);
      IF NOT pgs.get(grp, rl) THEN
        rl := NewRL();
        EVAL pgs.put(grp, rl)
      END;
      WITH range = NEW(Range.T) DO
        range^ := Range.B { base, len, grp };
        InsertOrdered(rl, range);
        InsertOrdered(allRanges, range);
        EVAL reverse.put(range, b[Field.Name]);
      END
    END
  END ProcessBuf;

PROCEDURE ParseLiteral(t : TEXT) : Address.T =
  BEGIN
    TRY
      RETURN Scan.Int(t)
    EXCEPT
      Lex.Error, FloatMode.Trap =>

      RETURN ParseHexLiteral(t)
    END
  END ParseLiteral;

PROCEDURE ParseHexLiteral(t : TEXT) : Address.T =

  PROCEDURE GetHexDigit(VAR d : [0..15]) : BOOLEAN =
    VAR c : CHAR;
    BEGIN
      IF Get(c) THEN
        CASE c OF
          '0' .. '9' => d := ORD(c) - ORD('0')     ; RETURN TRUE
        |
          'a' .. 'f' => d := ORD(c) - ORD('a') + 10; RETURN TRUE
        |
          'A' .. 'F' => d := ORD(c) - ORD('A') + 10; RETURN TRUE
        ELSE
          Push();
          RETURN FALSE
        END
      END;
      RETURN FALSE
    END GetHexDigit;

  PROCEDURE GetDecDigit(VAR d : [0..9]) : BOOLEAN =
    VAR
      x : [0..15];
    BEGIN
      IF NOT GetHexDigit(x) THEN RETURN FALSE END;
      IF x<=9 THEN
        d := x;
        RETURN TRUE;
      ELSE
        Push();
        RETURN FALSE
      END
    END GetDecDigit;

  PROCEDURE Get(VAR c : CHAR) : BOOLEAN =
    BEGIN
      IF p >= Text.Length(t) THEN RETURN FALSE END;
      c := Text.GetChar(t, p);
      INC(p);
      RETURN TRUE
    END Get;

  PROCEDURE Push() =
    BEGIN DEC(p) END Push;

  PROCEDURE Error() =
    BEGIN
      Debug.Error("cant parse hex \"" & t & "\"")
    END Error;
    
  VAR
    w : CARDINAL := 0;
    p : CARDINAL := 0;
    d : [0..9];
    h : [0..15];
    a : Address.T := 0;
    c : CHAR;
  BEGIN
    WHILE GetDecDigit(d) DO w := w * 10 + d END;
    IF NOT Get(c) OR c # '\'' THEN Error() END;
    IF NOT Get(c) OR c # 'h'  THEN Error() END;
    WHILE  GetHexDigit(h) DO a := a * 16 + h END;

    RETURN a
  END ParseHexLiteral;

VAR
  eqs    := NEW(BDDBDDTbl.Default).init();
  symTab := NEW(TextBDDTbl.Default).init();

PROCEDURE CreateBDD(nm : TEXT) : BDD.T =
  BEGIN
    WITH bdd = BDD.New(nm),
         hadIt = symTab.put(nm, bdd) DO
      <*ASSERT NOT hadIt*>
      RETURN bdd
    END
  END CreateBDD;

PROCEDURE MakeRangeBDD(base, len : Address.T) : BDD.T =
  BEGIN
    (* base <= addr AND addr < base + len *)
    Debug.Out(F("MakeRangeBDD(%s,%s)", Int16(base), Int16(len)));
    WITH lim = base+len,

         baseB = AddrToBDDArr(base),
         limB  = AddrToBDDArr(lim),

         geBase = BDD.Or(GT(addr^, baseB^),EQ(addr^,baseB^)),
         ltLim  = BDD.Not(BDD.Or(GT(addr^, limB^),EQ(addr^,limB^))) DO
      RETURN BDD.And(geBase, ltLim)
    END
  END MakeRangeBDD;

PROCEDURE BitGT(a, b : BDD.T) : BDD.T =
  (* a .gt. b = a AND NOT b *)
  BEGIN RETURN BDD.And(a, BDD.Not(b)) END BitGT;

TYPE Arr = ARRAY OF BDD.T;
     
PROCEDURE AddrToBDDArr(a : Address.T) : REF Arr =
  VAR
    res := NEW(REF Arr, BITSIZE(a));
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      CASE Word.Extract(a, i, 1) OF
        0 => res[i] := False()
      |
        1 => res[i] := True()
      ELSE
        <*ASSERT FALSE*>
      END
    END;
    RETURN res
  END AddrToBDDArr;

PROCEDURE GetBit(READONLY a : Arr; i : [-1..LAST(CARDINAL)]) : BDD.T =
  VAR res : BDD.T;
  BEGIN
    IF i >= FIRST(a) AND i <= LAST(a) THEN
      res := a[i]
    ELSE
      res := False()
    END;
    <*ASSERT res # NIL*>
    RETURN res
  END GetBit;
  
PROCEDURE EQ(READONLY a, b : Arr) : BDD.T =
  VAR res := True();
  BEGIN
    FOR i := 0 TO MAX(LAST(a),LAST(b)) DO
      WITH aBit = GetBit(a,i),
           bBit = GetBit(b,i) DO
        <*ASSERT aBit # NIL*>
        <*ASSERT bBit # NIL*>
        res := BDD.And(res, BDD.Equivalent(aBit, bBit))
      END
    END;
    RETURN res
  END EQ;

PROCEDURE GT(READONLY a, b : Arr) : BDD.T =
  VAR
    res := BDD.False();
  BEGIN
    (* invariant : a is greater than b so far *)
    FOR i := 0 TO MAX(LAST(a),LAST(b))  DO
      WITH aBit = GetBit(a, i),
           bBit = GetBit(b, i) DO
        res := BDD.Or(BitGT(aBit, bBit),
                      BDD.And(BDD.Not(BitGT(bBit,aBit)),
                              res))
      END
    END;
    RETURN res
  END GT;

  (**********************************************************************)

PROCEDURE BuildBoolean(rl : SortedRangeTbl.T) =
  VAR
    iter := rl.iterateOrdered(up:=TRUE);
    allCon := NEW(BDDSetDef.T).init();
    r : Range.T;
    dummy : REFANY;
  BEGIN
    WHILE iter.next(r,dummy) DO
      WITH bdd  = MakeRangeBDD(r.lo,r.len),
           con  = ExtractConjuncts(bdd),
           fmtd = "***" (*XFormat(bdd)*) DO
        Debug.Out(F("\nrange %s + %s : %s",
                    Int16(r.lo), Int16(r.len),
                    fmtd));
        VAR
          iter := con.iterate();
          b : BDD.T;
        BEGIN
          WHILE iter.next(b) DO
          END;
        END;

        allCon := allCon.unionD(con)

      END
    END;

    VAR
      pg := BDD.False();
      iter := allCon.iterate();
      b : BDD.T;
    BEGIN
      WHILE iter.next(b) DO
        pg := BDD.Or(pg, b)
      END;

      Debug.Out("pg total " & "***" (*XFormat(pg)*))
    END;
    
    Debug.Out(F("POLICY GROUP ranges %s conjuncts %s",
                Fmt.Int(rl.size()),
                Fmt.Int(allCon.size())))
  END BuildBoolean;

(*VAR MaxDebug := 1000;*)
    
PROCEDURE MergeRanges(s : SortedRangeTbl.T) =
  (* modifies merged ranges in allRanges, not in per-PG tables *)
  PROCEDURE Push(ran : Range.T) =
    BEGIN
      Debug.Out(F("MergeRanges pushing %s", Range.Format(ran)));
      EVAL allRanges.put(ran, ran.group)
    END Push;
  
  VAR
    iter := s.iterateOrdered();
    r, q : Range.T;
    qv := FALSE; (* q holds valid data *)
    dummy : REFANY;
  BEGIN
    WHILE iter.next(r,dummy) DO
      IF qv AND q.lo + q.len = r.lo THEN
        IF Debug.GetLevel() >= 100 THEN
          Debug.Out(F("merging ranges : %s <-> %s",
                      Range.Format(q), Range.Format(r)))
        END;
          
        q.len := q.len + r.len
      ELSIF qv THEN Push(q); q := r
      ELSE
        (* first iteration *)
        q := r;
        qv := TRUE
      END
    END;
    IF qv THEN Push(q) END;
  END MergeRanges;

PROCEDURE ScatterRanges() =
  VAR rIter := pgs.iterate();
      iter := allRanges.iterate();
      nm : TEXT;
      tbl : SortedRangeTbl.T;
      r : Range.T;
      ref : REFANY;
  BEGIN
    (* clear all ranges *)
    WHILE rIter.next(nm, tbl) DO
      tbl := NEW(SortedRangeTbl.Default).init();
      WITH hadIt = pgs.put(nm, tbl) DO <*ASSERT hadIt*> END
    END;
    
    WHILE iter.next(r, ref) DO
      VAR z : SortedRangeTbl.T;
          hadIt := pgs.get(ref, z);
      BEGIN
        <*ASSERT hadIt*>

        WITH hadIt2 = z.put(r, ref) DO
          <*ASSERT NOT hadIt2*>
        END
        
      END
    END
  END ScatterRanges;
  
PROCEDURE MergeGroups() =
  VAR
    iter := pgs.iterate();
    nm : TEXT;
    s : SortedRangeTbl.T;
  BEGIN
    Debug.Out(">>>>>>>>>>>>>>>>>>>>  MERGE GROUPS  >>>>>>>>>>>>>>>>>>>>");

    allRanges := NEW(SortedRangeTbl.Default).init();
    
    WHILE iter.next(nm, s) DO
      Debug.Out("Merging ranges in group " & nm);
      MergeRanges(s)
    END;

    ScatterRanges();

    Debug.Out("<<<<<<<<<<<<<<<<<<<<  MERGE GROUPS  <<<<<<<<<<<<<<<<<<<<");

  END MergeGroups;

PROCEDURE CheckForOverlaps(checkMerges : BOOLEAN) : BOOLEAN =

  PROCEDURE CheckForOverlap(r : Range.T) =
    VAR
      jter := allRanges.iterateOrdered();
      q, rdum : Range.T;
      nn : REFANY;
    BEGIN
      (* this code is inefficient but maybe adaptable .. *)
      jter.seek(r);
      (* at next entry *)

      (* skip r itself *)
      WITH hadIt = jter.next(q,nn) DO <*ASSERT hadIt AND Range.Equal(r, q)*> END;
      
      IF jter.next(q, nn) THEN
        IF checkMerges AND TE(q.group, r.group) AND Range.CanMerge(q, r, rdum) THEN
          Debug.Error(F("Uncaught merge opportunity : %s <-> %s",
                        Range.Format(r), Range.Format(q)))
        END;

        IF Range.Overlap(q, r) THEN
          VAR pr := FALSE;
              qn, rn := "--UNKNOWN--";
              w := "";
          BEGIN
            IF reverse.get(q, qn) THEN pr := TRUE END;
            IF reverse.get(r, rn) THEN pr := TRUE END;

            IF pr THEN
              w := F("\nOVERLAP REGISTER(S) INVOLVED: %s <-> %s", qn, rn)
            END;
            Debug.Warning(F("FOUND OVERLAP : %s <-> %s%s", Range.Format(q), Range.Format(r),w));
            
          END;
          success := FALSE
        END
      END
    END CheckForOverlap;
    
  VAR
    iter := allRanges.iterateOrdered();
    r : Range.T;
    n : REFANY;
    success := TRUE;
  BEGIN
    Debug.Out(">>>>>>>>>>>>>>>>>>>>  CHECKING FOR OVERLAPS  >>>>>>>>>>>>>>>>>>>>");
    WHILE iter.next(r, n) DO CheckForOverlap(r) END;

    Debug.Out("<<<<<<<<<<<<<<<<<<<<  CHECKING FOR OVERLAPS  <<<<<<<<<<<<<<<<<<<<");
    RETURN success

  END CheckForOverlaps;

PROCEDURE AttemptElimOverlaps() =
  VAR
    iter := allRanges.iterateOrdered();
    q, r : Range.T;
    first := TRUE;
    overlaps := NEW(SortedRangeTbl.Default).init();
    n : REFANY;
  BEGIN
    Debug.Out(">>>>>>>>>>>>>>>>>>>>  ELIMINATING OVERLAPS  >>>>>>>>>>>>>>>>>>>>");
    (* invariant : q is beginning of current overlap *)
    WHILE iter.next(r, n) DO
      IF first THEN
        first := FALSE;
        q := r
      ELSIF TE(q.group, r.group) AND Range.Overlap(q, r) THEN
        VAR lst : REFANY := NIL;
        BEGIN
          EVAL overlaps.get(q, lst);
          lst := RefList.Cons(r, lst);
          EVAL overlaps.put(q, lst)
        END
      ELSE (* no overlap case *)
        q := r
      END
    END;

    iter := overlaps.iterateOrdered();
    
    VAR
      nm : TEXT;
      lst : REFANY;
      p : RefList.T;
      max : CARDINAL;
      dum : REFANY;
    BEGIN
      WHILE iter.next(q, lst) DO
        nm := "--UNKNOWN--";
        EVAL reverse.get(q, nm);
        p := lst;
        max := q.lo + q.len;
        WHILE p # NIL DO
          WITH r = NARROW(p.head, Range.T) DO
            max := MAX(max, r.lo + r.len);
            WITH hadIt = allRanges.delete(r,dum) DO <*ASSERT hadIt*> END;
          END;
          p := p.tail
        END;
        Debug.Out(F("Eliminating overlaps for %s @ %s, %s overlaps %s -> %s",
                    nm, Int16(q.lo), Fmt.Int(RefList.Length(lst)), Int16(q.len), Int16(max-q.lo)));
        WITH hadIt = allRanges.delete(q,dum) DO <*ASSERT hadIt*> END;
        q.len := max - q.lo
      END
    END;

    ScatterRanges();
    
    Debug.Out("<<<<<<<<<<<<<<<<<<<<  ELIMINATING OVERLAPS  <<<<<<<<<<<<<<<<<<<<");

  END AttemptElimOverlaps;

PROCEDURE ExtendIntoGaps() : BOOLEAN =

  PROCEDURE ExtendRange(r : Range.T) =
    VAR
      jter := allRanges.iterateOrdered();
      q, rr : Range.T;
      nn : REFANY;
      z := new.size();
    BEGIN
      (* this code is inefficient but maybe adaptable .. *)
      jter.seek(r);
      (* at next entry *)

      (* skip r itself *)
      WITH hadIt = jter.next(q,nn) DO <*ASSERT hadIt AND Range.Equal(r, q)*> END;
      
      IF jter.next(q, nn) THEN
        IF  q.lo > r.lo + r.len THEN
          extended := TRUE;
          rr := r;
          
          r.len := q.lo - r.lo;
          Debug.Out(F("Extending range into gap: %s -> %s", Range.Format(rr),
                    Range.Format(r)));
        END;


      END;
      
      EVAL new.put(r, r.group);
      <*ASSERT new.size() = z+1*>
    END ExtendRange;
    
  VAR
    iter := allRanges.iterateOrdered();
    new := NEW(SortedRangeTbl.Default).init();
    r : Range.T;
    n : REFANY;
    extended : BOOLEAN;
  BEGIN
    Debug.Out(">>>>>>>>>>>>>>>>>>>>  EXTENDING RANGES  >>>>>>>>>>>>>>>>>>>>");

    WHILE iter.next(r, n) DO ExtendRange(r) END;

    Debug.Out(F("allRanges %s new %s", Fmt.Int(allRanges.size()), Fmt.Int(new.size())));
    
    <*ASSERT allRanges.size() = new.size()*>
    
    allRanges := new;
    reverse := Rehash(reverse); (* hashes might be screwed up *)
    
    ScatterRanges(); (* maintain invariant that allRanges is union of pgs *)
    
    Debug.Out("<<<<<<<<<<<<<<<<<<<<  EXTENDING RANGES  <<<<<<<<<<<<<<<<<<<<");

    RETURN extended

  END ExtendIntoGaps;

PROCEDURE Rehash(tbl : SortedRangeTextTbl.T) : SortedRangeTextTbl.T =
  VAR
    new := NEW(SortedRangeTextTbl.Default).init();
    iter := tbl.iterate();
    r : Range.T;
    n : TEXT;
  BEGIN
    WHILE iter.next(r, n) DO EVAL new.put(r, n) END;
    RETURN new
  END Rehash;

PROCEDURE ExtractConjuncts(b : BDD.T) : BDDSet.T =
  VAR
    res := NEW(BDDSetDef.T).init();
    q : BDD.T;
  BEGIN
    WITH sop = SopBDD.ConvertBool(b),
         sim = sop (*.invariantSimplify(tr,tr,tr)*) DO
      FOR c := FIRST(sim.rep^) TO LAST(sim.rep^) DO
        (* for each conjunct *)
        WITH con = sim.rep[c] DO
          q := BDD.True();
          FOR i := FIRST(con^) TO LAST(con^) DO
            VAR lit : BDD.T; BEGIN
              IF con[i].mode THEN
                lit := con[i].var
              ELSE
                lit := BDD.Not(con[i].var)
              END;
              q := BDD.And(q, lit)
            END
          END
        END;
        Debug.Out(F("conjunct %s", XFormat(q)));
        EVAL res.insert(q)
      END
    END;
    RETURN res
  END ExtractConjuncts;

PROCEDURE MakeL1(<*UNUSED*>lnum : CARDINAL; from : CARDINAL; lim : CARDINAL) =
  TYPE
    Usage = { True, False, Skip };
  VAR
    n := lim-from;
    p := NEW(REF ARRAY OF Usage, n);
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO p[i] := FIRST(Usage) END;

    LOOP
      VAR b := BDD.True(); BEGIN
        FOR i := FIRST(p^) TO LAST(p^) DO
          CASE p[i] OF
            Usage.True => b := BDD.And(b, addr[from+i])
          |
            Usage.False => b := BDD.And(b, BDD.Not(addr[from+i]))
          |
            Usage.Skip => (* skip *)
          END
        END;
        VAR
          q : BDD.T;
        BEGIN
          IF eqs.get(b, q) THEN
            Debug.Out(F("Already have %s <- %s", XFormat(q), XFormat(b)))
          ELSE
            q :=  BDD.New(F("t%s", Fmt.Int(tCnt))); INC(tCnt);
            Debug.Out(F("Created %s <- %s", XFormat(q), XFormat(b)));
            EVAL eqs.put(b, q)
          END
        END
      END;

      (* increment usage array *)
      VAR
        j := FIRST(p^);
      BEGIN
        LOOP
          IF p[j] = LAST(Usage) THEN
            p[j] := FIRST(Usage)
          ELSE
            INC(p[j]);
            EXIT
          END;
          INC(j);
          IF j = NUMBER(p^) THEN RETURN (* done with all *) END
        END
      END
    END
  END MakeL1;

PROCEDURE PrintStats(build := FALSE) =
  VAR
    iter := pgs.iterate();
    nm : TEXT;
    rl : SortedRangeTbl.T;
  BEGIN
    Debug.Out(">>>>>>>>>>>>>>>  STATS  >>>>>>>>>>>>>>>");
    WHILE iter.next(nm, rl) DO
      Debug.Out(F("%s : %s", nm, Fmt.Int(rl.size())));
      
      IF build THEN BuildBoolean(rl) END
    END;

    Debug.Out(F("totLen = %s", Fmt.Int(totLen)));
    Debug.Out("<<<<<<<<<<<<<<<  STATS  <<<<<<<<<<<<<<<");

  END PrintStats;

PROCEDURE MakeInputGroups() =
  CONST
    L1Width = 4;
  VAR
    lnum := 0;
  BEGIN
    FOR i := 0 TO width-1 BY L1Width DO
      MakeL1(lnum, i, MIN(i+L1Width, width));
      INC(lnum)
    END
  END MakeInputGroups;

PROCEDURE DebugDump() =
  VAR iter := allRanges.iterateOrdered();
      r : Range.T;
      ref : REFANY;
  BEGIN
    Debug.Out(">>>>>>>>>>>>>>>>>>>>  DEBUG DUMP  >>>>>>>>>>>>>>>>>>>>");
    WHILE iter.next(r, ref) DO
      Debug.Out(Range.Format(r))
    END;
    Debug.Out("<<<<<<<<<<<<<<<<<<<<  DEBUG DUMP  <<<<<<<<<<<<<<<<<<<<")
  END DebugDump;

PROCEDURE AssertNoGaps() =
  VAR iter := allRanges.iterateOrdered();
      q, r : Range.T;
      ref : REFANY;
      qv := FALSE;
  BEGIN
    Debug.Out(">>>>>>>>>>>>>>>>>>>>  CHECKING  >>>>>>>>>>>>>>>>>>>>");
    WHILE iter.next(r, ref) DO
      IF qv THEN
        IF q.lo + q.len # r.lo THEN
          Debug.Error(F("gap between consecutive ranges : %s <-> %s", Range.Format(q), Range.Format(r)))
        END;
        
        IF TE(r.group, q.group) THEN
          Debug.Error(F("consecutive groups match: %s <-> %s", Range.Format(q), Range.Format(r)))
        END
      END;
      q := r; qv := TRUE
    END;
    Debug.Out("<<<<<<<<<<<<<<<<<<<<  CHECKING  <<<<<<<<<<<<<<<<<<<<")
  END AssertNoGaps;

(**********************************************************************)

TYPE
  Sections = { Prolog, Decls, Code, Epilog };
  Streams = ARRAY Sections OF Wr.T ;
  
PROCEDURE FmtSVCard(a : Address.T) : TEXT =
  BEGIN
    RETURN "'h" & Fmt.Int(a, base := 16)
  END FmtSVCard;

PROCEDURE DumpSV(pn : Pathname.T) RAISES { Wr.Failure, OSError.E } =
  VAR
    lo : ARRAY [FIRST(ExtPolicyGroupIdx)..LAST(ExtPolicyGroupIdx)+1] OF CARDINAL;
    p : CARDINAL := 0;

  PROCEDURE O(str : TEXT; ptgt : Wr.T := NIL) RAISES { Wr.Failure } =
    BEGIN
      IF ptgt = NIL THEN ptgt := cur END;
      Wr.PutText(ptgt, str);
      Wr.PutChar(ptgt, '\n')
    END O;

  PROCEDURE Iterate(idx : ExtPolicyGroupIdx; VAR pp : CARDINAL)
    RAISES { Wr.Failure } =
    VAR
      iter := allRanges.iterateOrdered();
      r : Range.T;
      n : REFANY;
      nm : TEXT;
    BEGIN
      WHILE iter.next(r, n) DO
        VAR
          pgi : ExtPolicyGroupIdx;
          fmt, str : TEXT;
        BEGIN
          pgi := MapPGnameToNumber(r.group);
          IF pgi = idx THEN
            WITH hadIt = reverse.get(r, nm) DO <*ASSERT hadIt*> END;
            
            fmt := "  // %-" & Fmt.Int(widest[Field.Name]) & "s " &
                       "%-" & Fmt.Int(widest[Field.Group]) & "s " &
                       "%9s <= addr < (+%6s) %9s";
            
            str := F(fmt, 
                     nm,
                     r.group,
                     FmtSVCard(r.lo),
                     FmtSVCard(r.len),
                     FmtSVCard(r.lo+r.len));
            O("");
            O(str);

            O(F("  assign m[%s] = %s;",
                Fmt.Int(pp),
                RangeExpr("i_addr", r.lo, r.lo+r.len)));
            INC(pp)
          END
        END
      END;
    END Iterate;

  PROCEDURE DumpPgListDebug() RAISES { Wr.Failure } =
    BEGIN
      O(F(" // policy group PG_DEFAULT"));
      FOR i := FIRST(PolicyGroupArr) TO LAST(PolicyGroupArr) DO
        O(F(" // policy group %5s \"%s\"", Fmt.Int(i), PolicyGroupArr[i]));
      END
    END DumpPgListDebug;

  PROCEDURE DeclareMinterms() RAISES { Wr.Failure } =
    BEGIN
      O(F("  logic[%s-1:0]  m;", Fmt.Int(allRanges.size())))
    END DeclareMinterms;
    
  PROCEDURE DeclareOneHot() RAISES { Wr.Failure } =
    BEGIN
      O(F("  logic[%s-1:0] pg1;", Fmt.Int(NUMBER(ExtPolicyGroupIdx))))
    END DeclareOneHot;

  PROCEDURE EmitMinterms() RAISES { Wr.Failure } =
    BEGIN
      lo[FIRST(lo)] := 0;
      FOR i := FIRST(ExtPolicyGroupIdx) TO LAST(ExtPolicyGroupIdx) DO
        Iterate(i, p);
        lo[i+1] := p
      END
    END EmitMinterms;

  PROCEDURE EmitOneHotCombBlock(idx : CARDINAL) RAISES { Wr.Failure } =
    VAR
      l := lo[idx];
      h := lo[idx+1]-1; 
    BEGIN
      O(F(""));
      IF h >= l THEN
        (* nonempty range *)
        O(F("  assign pg1[%s] = |(m[%s:%s]);",
            Fmt.Int(idx), Fmt.Int(h), Fmt.Int(l)))
      ELSE
        O(F("  assign pg1[%s] = '0;", Fmt.Int(idx)))
      END
    END EmitOneHotCombBlock;
    
  PROCEDURE EmitOneHotCombBlocks() RAISES { Wr.Failure } =
    BEGIN
      FOR i := FIRST(ExtPolicyGroupIdx) TO LAST(ExtPolicyGroupIdx) DO
        EmitOneHotCombBlock(i)
      END
    END EmitOneHotCombBlocks;

  PROCEDURE EmitEncodePGBlock() RAISES { Wr.Failure } =
    BEGIN
      O(F(  ""));
      O(F(  "  always_comb begin : generate_pg"));
      O(F(  ""));
      O(F(  "    pg = DEFAULT_PG;"));
      O(F(  ""));
      O(F(  "    if      (0)  /* skip */ ;"));
      FOR i := FIRST(PolicyGroupArr) TO LAST(PolicyGroupArr) DO
        O(F("    else if (pg1[%-3s]) pg = %s;", Fmt.Int(i), Fmt.Int(i)))
      END;
      O(F(  "    else if (pg1[%-3s]) pg = %s;", Fmt.Int(DefaultIdx), "DEFAULT_PG"));
      O(F(  "  end : generate_pg"));
    END EmitEncodePGBlock;

  VAR
    tgt : Streams;

    cur : Wr.T;
  BEGIN

    FOR i := FIRST(tgt) TO LAST(tgt) DO tgt[i] := TextWr.New() END;
    
    cur := tgt[Sections.Prolog];

    O(" // AUTOMATICALLY GENERATED DO NOT EDIT                          ");
    O(" // GENERATED BY genpg                                           ");
    O("                                                                 ");
    O("`include \"hlp_checkers_ext.vs\"                                 ");
    O("                                                                 ");
    DumpPgListDebug();
    O("                                                                 ");
    O("module hlp_sai_security_addr_pg                                  ");
    O(" #(                                                              ");
    O("  DEFAULT_PG = 0                                                 ");
    O("  )                                                              ");
    O("  (                                                              ");
    O("  input  logic                                   clk,            ");
    O("  input  logic                                   rst_n,          ");
    O("                                                                 ");
    O("  input  logic [hlp_pkg::W_MGMT_ADDR-1:0]        i_addr,         ");
    O("  input  logic                                   i_v,            ");
    O("                                                                 ");
    O("  // Management  connections                                     ");
    O("  output hlp_imn_pkg::pg_bit_t                   o_pg            ");
    O(");                                                               ");
    O("                                                                 ");
    O("                                                                 ");
    O("  //import packages                                              ");
    O("  import hlp_pkg::*;                                             ");
    O("  import hlp_register_constants_pkg::*;                          ");
    O("  import hlp_c_imn_pkg::*;                                       ");
    O("  import hlp_imn_pkg::*;                                         ");
    O("                                                                 ");
    O("  hlp_imn_pkg::pg_bit_t                   pg;                    ");
    O("                                                                 ");

    cur := tgt[Sections.Decls];
    DeclareMinterms();
    DeclareOneHot();

    cur := tgt[Sections.Code];
    EmitMinterms();
    EmitOneHotCombBlocks();
    EmitEncodePGBlock();

    EmitSharedExpressions(tgt);
    
    cur := tgt[Sections.Epilog];
    O("                                                                 ");
    O("  always_ff @(posedge clk or negedge rst_n) begin                ");
    O("    if      (~rst_n)                                             ");
    O("      o_pg <= DEFAULT_PG;                                        ");
    O("    else if (i_v)                                                ");
    O("      o_pg <= pg;                                                ");
    O("  end // always_ff begin                                         ");
    O("endmodule                                                        ");

    WITH wr = FileWr.Open(pn) DO
      FOR i := FIRST(tgt) TO LAST(tgt) DO
        Wr.PutText(wr, TextWr.ToText(tgt[i]))
      END;
      Wr.Close(wr)
    END
  END DumpSV;

VAR exprTbl := NEW(TextCardTbl.Default).init();
    exprSeq := NEW(TextSeq.T).init();

PROCEDURE CombName(c : CARDINAL) : TEXT =
  BEGIN RETURN "comb" & Fmt.Int(c) END CombName;

PROCEDURE EmitSharedExpressions(tgt : Streams) =
  BEGIN
    FOR i := 0 TO exprSeq.size()-1 DO
      Wr.PutText(tgt[Sections.Decls],
                 F("  logic %s;\n", CombName(i)));
      Wr.PutText(tgt[Sections.Code],
                 F("  assign %s = %s;\n", CombName(i), exprSeq.get(i)))
    END
  END EmitSharedExpressions;
  
PROCEDURE RangeExpr(var : TEXT; lo, lm : CARDINAL) : TEXT =
  VAR
    loW  : Word.T := lo;
    hiW  : Word.T := lm-1;
    di := -1;
  BEGIN
    (* (i_addr >= %s) & (i_addr < %s) *)
    FOR i := Word.Size-1 TO 0 BY -1 DO
      IF Word.Extract(loW, i, 1) # Word.Extract(hiW, i, 1) THEN
        di := i; EXIT
      END
    END;

    IF di = -1 THEN
      (* lo, hi words differ in every bit *)
      RETURN
        MemoizeExpr(F("(%s >= %s) & (%s < %s)", var, FmtSVCard(lo), FmtSVCard(lm)))
    ELSE
      (* words are equal from Word.Size downto di+1 
         words differ    from di        downto    0 *)
      
      WITH eqP = Word.Extract(loW, di+1, Word.Size-(di+1)),
           loD = Word.Extract(loW, 0   , di+1),
           hiD = Word.Extract(hiW, 0   , di+1) DO
        VAR res := "";
        BEGIN
          res := EqualsExpr(var, di+1, loW);

          IF loD # 0 THEN
            res := res & F(" & ") &
                       MemoizeExpr(F("(%s[%s:0] >= %s)", var, Fmt.Int(di), FmtSVCard(loD)))
          END;

          IF hiD # Word.LeftShift(1, di+1)-1 THEN
            res := res & 
                F(" & ") &
                MemoizeExpr(F("(%s[%s:0] <  %s)", var, Fmt.Int(di), FmtSVCard(hiD+1)))
          END;
          RETURN res
        END
      END
    END

  END RangeExpr;

PROCEDURE EqualsExpr(var : TEXT;
                     lsb  : CARDINAL;
                     val : Word.T) : TEXT =
  (* format infix expression denoting var = val starting at bit lb,
     with the various terms being easily shareable *)
  
  VAR
    first := TRUE;
    
  PROCEDURE Push(str : TEXT) =
    BEGIN
      IF first THEN res := str; first := FALSE ELSE res := res & " & " & str END
    END Push;
    
  CONST
    Step = 4;
  VAR
    msb := MaxSetBit(val);
    res : TEXT;
  BEGIN
    Debug.Out(F("FmtEquals(%s,%s,16_%s)", var, Fmt.Int(lsb), Fmt.Int(val, base := 16)));
    
    (* F("(%s[$bits(%s)-1:%s] == %s)",var,var,Fmt.Int(di+1),FmtSVCard(eqP)); *)
    FOR k := 0 TO Word.Size-Step BY Step DO
      WITH
        valChunk = Word.Extract(val, k, Step),
        chunkMsb = MIN(bits-1, k + Step-1), (* top of this chunk *)
        chunkLsb = MAX(k, lsb),             (* bottom of this chunk *)

        (* note that if lsb > k + Step - 1  then there is nothing to
           emit on this chunk since we havent reached the bits involved 
           in the equality test yet *)
        
        valCmp   = Word.RightShift(valChunk, chunkLsb - k),
        valCmpStr= FmtSVCard(valCmp),
        last     = chunkMsb >= msb               DO

        Debug.Out(F("  FmtEquals k=%s valChunk=16_%s chunkMsb=%s chunkLsb=%s valCmp=16_%s",
                    Fmt.Int(k),
                    Fmt.Int(valChunk, base := 16),
                    Fmt.Int(chunkMsb),
                    Fmt.Int(chunkLsb),
                    Fmt.Int(valCmp, base := 16)) &
                  F(" last=%s",                                                
                    Fmt.Bool(last)));
        IF chunkMsb >= chunkLsb THEN
          Push(MemoizeExpr(F("(%s[%s:%s] == %s)", var, Fmt.Int(chunkMsb), Fmt.Int(chunkLsb),
                             valCmpStr)))
        END;
        IF last THEN EXIT END
      END
    END;
    RETURN res
  END EqualsExpr;

PROCEDURE MemoizeExpr(expr : TEXT) : TEXT =
  VAR
    n : CARDINAL;
  BEGIN
    IF NOT exprTbl.get(expr, n) THEN
      n := exprSeq.size();
      EVAL exprTbl.put(expr, n);
      exprSeq.addhi(expr);
    END;
    RETURN CombName(n)
  END MemoizeExpr;

PROCEDURE MaxSetBit(w : Word.T) : [-1..Word.Size-1] =
  BEGIN
    FOR i := 0 TO Word.Size DO
      IF Word.RightShift(w, i) = 0 THEN RETURN i-1 END
    END;
    <*ASSERT FALSE*>
  END MaxSetBit;
  
(**********************************************************************)
  
VAR
  buf : ARRAY Field OF TEXT;
  csv : CSVParse.T;
  fn : Pathname.T;
  width := 28;
  addr  :=  NEW(REF Arr, width);
  tCnt := 0;
  skipHoles : BOOLEAN;
  attemptElimOverlaps : BOOLEAN;
  svOutput : Pathname.T := NIL;
  bits := -1;
BEGIN

  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-sv") THEN svOutput := pp.getNext() END;
      IF pp.keywordPresent("-bits") THEN bits := pp.getNextInt() END;
      skipHoles := pp.keywordPresent("-skipholes");
      attemptElimOverlaps := pp.keywordPresent("-elimoverlaps");
      pp.skipParsed();
      fn := pp.getNext();
      pp.finish();
    END;
    IF bits = -1 THEN Debug.Error("Must specify -bits") END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong.")
  END;

  
  TRY
    rd := FileRd.Open(fn);
    csv := NEW(CSVParse.T).init(rd);
    
    csv.startLine();
    csv.startLine();
    LOOP
      TRY
        csv.startLine();
        FOR i := FIRST(Field) TO LAST(Field) DO
          buf[i] := csv.cell()
        END;
        IF NOT TextUtils.HavePrefix(buf[FIRST(buf)], "//") THEN
          ProcessBuf(buf)
        END
      EXCEPT
        CSVParse.EndOfLine => (* wrong syntax *)
      END
    END
  EXCEPT
    Rd.Failure(x) =>
    Debug.Error("I/O error while reading input : Rd.Failure : "& AL.Format(x))
  |
    OSError.E(x) =>
    Debug.Error("Error while opening input : OSError.E : "& AL.Format(x))
  |
    Rd.EndOfFile =>
    (* done *)
    Rd.Close(rd)
  END;

  Debug.Out("allRanges : " & Fmt.Int(allRanges.size()));

  PrintStats();

  IF attemptElimOverlaps THEN
    AttemptElimOverlaps()
  END;
  
  IF NOT CheckForOverlaps(FALSE) THEN
    Debug.Error("There were overlaps.  Cant continue")
  END;
  
  MergeGroups();

  PrintStats();

  IF NOT CheckForOverlaps(TRUE) THEN
    Debug.Error("Internal program error---overlaps created")
  END;

  IF skipHoles THEN
    IF ExtendIntoGaps() THEN
      MergeGroups()
    END
  END;

  PrintStats();

  IF NOT CheckForOverlaps(TRUE) THEN
    Debug.Error("Internal program error---overlaps created")
  END;

  DebugDump();
  
  IF skipHoles THEN AssertNoGaps() END;

  (* wonder if we need any of this BDD stuff for the brave new program *)
  VAR
    addr_ :=  NEW(REF Arr, width);
  BEGIN
    FOR i := 0 TO width-1 DO
      addr [i] := CreateBDD(F("addr[%s]", Fmt.Int(i)));
      addr_[i] := CreateBDD(F("addr_[%s]", Fmt.Int(i)));
      EVAL eqs.put(BDD.Not(addr[i]), addr_[i])
    END
  END;

  MakeInputGroups();

  PrintStats((*build := TRUE*));

  IF svOutput # NIL THEN DumpSV(svOutput) END
  
END Main.
