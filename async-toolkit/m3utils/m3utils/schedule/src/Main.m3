(* 
   Program for calculating schedule for Red Rock Canyon switch.

   Follows requirements of Word document by J. Dama, R. Southworth, M. Atkin

   "RRC_High_Bandwidth_Schedules.docx"

   Does not produce output at the moment, except with debugging turned on.

   Author: Mika Nystrom <mika.nystroem@intel.com>

*)

MODULE Main;
IMPORT BDD;
IMPORT Debug;
FROM BDD IMPORT Implies, Not, And, Or, MakeTrue, MakeFalse;
IMPORT LongrealSetDef, LongrealSet;
IMPORT RefSeq;
IMPORT CardSeq, CardList, CardBDDTbl;
IMPORT Fmt; 
FROM Fmt IMPORT F;
IMPORT Rd, TextReader, Stdio, Scan, Text;
IMPORT LongRealSeq;
IMPORT Lex, FloatMode;
IMPORT Thread;
IMPORT Wx;
IMPORT BDDSet, BDDSetHash, BDDSetHashRefTbl;
IMPORT BDDIntPairTbl, BDDDepends, IntPair;
IMPORT Wr, FileWr;
IMPORT OSError, AL;

<*FATAL Thread.Alerted*>

CONST I = Fmt.Int;
      L = Fmt.LongReal;

PROCEDURE I3(i : INTEGER) : TEXT =
  BEGIN
    RETURN Fmt.Pad(I(i), length := 3)
  END I3;

CONST TE = Text.Equal;

CONST Verbose = FALSE;

VAR
  N, deltaGapMax, minPortRep     : CARDINAL;
  underrunMax, minRepBits, clock : LONGREAL;
  jitterSpeed                    : LONGREAL;
  singleSpeed                    : LONGREAL;

(* we use internal port numbering, see end of file *)

VAR ch    : REF ARRAY OF ARRAY OF BDD.T; 
    (* ch[sp,sl] <=> speed[sp] in slot sl *)
    minct : REF ARRAY OF CARDINAL;
    deps  : REF ARRAY OF ARRAY OF CardList.T;

    ports : REF ARRAY OF LONGREAL; (* port speeds, indexed by internal ports *)

VAR equations : RefSeq.T;
TYPE Eq = OBJECT label : TEXT; eq : BDD.T END;

VAR eqc := 0;

PROCEDURE Assert(label : TEXT; what : BDD.T) =
  BEGIN
    equations.addhi(NEW(Eq, 
                        label := F("[%s,sz=%s]",I(eqc),I(BDD.Size(what))) & 
                                     label, 
                        eq := what));
    INC(eqc)
  END Assert;

PROCEDURE ClearEquations() =
  BEGIN
    equations := NEW(RefSeq.T).init()
  END ClearEquations;

TYPE 
  Equations = OBJECT
    tbl : CardBDDTbl.T;
    up  : Equations;
  METHODS
    get(idx : CARDINAL) : BDD.T         := EGet;
    init(e : Equations := NIL) : Equations := EInit;
    modify(idx : CARDINAL; to : BDD.T)  := EModify;
  END;

PROCEDURE EGet(e : Equations; idx : CARDINAL) : BDD.T =
  VAR 
    r : BDD.T;
    upCnt := 0;
    ptr := e;

  BEGIN
    IF e.tbl.get(idx, r) THEN
      RETURN r
    ELSE
      ptr := e;

      WHILE ptr.up # NIL DO
        IF ptr.tbl.get(idx,r) THEN
          EXIT
        ELSE
          ptr := ptr.up;
          INC(upCnt)
        END
      END;

      IF ptr.up = NIL THEN 
        r := NARROW(equations.get(idx),Eq).eq
      END;
      
      RETURN r
    END
  END EGet;

PROCEDURE EInit(e, from : Equations) : Equations =
  BEGIN
    e.tbl := NEW(CardBDDTbl.Default).init(); 
    e.up := from;
    RETURN e
  END EInit;

PROCEDURE EModify(e : Equations; idx : CARDINAL; to : BDD.T) =
  BEGIN
    EVAL e.tbl.put(idx,to)
  END EModify;
  
(**********************************************************************)

PROCEDURE Msg(i : CARDINAL) : TEXT =
  BEGIN RETURN NARROW(equations.get(i),Eq).label END Msg;

PROCEDURE AssertShannon(b, v : BDD.T) =
  VAR
    mf := MakeFalse(b, v);
    mt := MakeTrue (b, v);
    sh := Or(And(v, mt), And(Not(v), mf));
  BEGIN
    <* ASSERT sh = b *>
  END AssertShannon;

TYPE
  JitterState = RECORD
    old, oldLeader : Jitter;
  END;

PROCEDURE SaveJitter(try : XCard) : JitterState =
  VAR res : JitterState;
  BEGIN
    IF try # -1 AND JitterEnforced(try) THEN
      res.old       := jitter[try];
      res.oldLeader := jitter[res.old.leader];
    END;
    RETURN res
  END SaveJitter;

PROCEDURE ResetJitter(READONLY state : JitterState; 
                      try            : XCard) =
  BEGIN
    IF try # -1 AND JitterEnforced(try) THEN
      (* undo update of jitter *)
      jitter[try] := state.old;
      jitter[state.old.leader] := state.oldLeader
    END
  END ResetJitter;

PROCEDURE AssertJitterOK(try : XCard) =
  (* check that we dont have bugs, else abort program *)
  BEGIN
    IF try # -1 AND JitterEnforced(try) THEN
      WITH gJitter = jitter[jitter[try].leader] DO
        <*ASSERT gJitter.groupMax - gJitter.groupMin <= deltaGapMax *>
      END
    END
  END AssertJitterOK;

PROCEDURE JitterEnforced(sp : XCard) : BOOLEAN =
  BEGIN
    RETURN sp # -1 AND ports[sp] >= 0.0d0 AND ports[sp] <= jitterSpeed
  END JitterEnforced;

PROCEDURE SatisfyAssertions(READONLY ni : ARRAY OF CARDINAL) : BOOLEAN =

  PROCEDURE Recurse(p         : CARDINAL; 
                    e         : Equations) : BOOLEAN =

    VAR f      : Equations;
        reason : TEXT;

    PROCEDURE MakeTrue(sp, sl : CARDINAL) : BOOLEAN =
      VAR 
        p := deps[sp,sl];
      BEGIN
        (*
        Debug.Out(F("MakeTrue(%s,%s)", I(sp), I(sl)));
        *)
        WHILE p # NIL DO
          WITH idx = p.head,
               old = f.get(idx),
               new = BDD.MakeTrue(old, ch[sp,sl]) DO
            IF new = BDD.False() THEN 
              (*
              Debug.Out(F("MakeTrue %s,%s FAIL (%s)",I(sp),I(sl),Msg(idx)));
              *)
              reason := NARROW(equations.get(idx),Eq).label;
              RETURN FALSE 
            END;
            IF old # new THEN f.modify(idx, new) END
          END;
          p := p.tail
        END;
        RETURN TRUE
      END MakeTrue;

    PROCEDURE MakeFalse(sp, sl : CARDINAL) : BOOLEAN =
      VAR 
        p := deps[sp,sl];
      BEGIN
        (*
        Debug.Out(F("MakeFalse(%s,%s), ndeps=%s", I(sp), I(sl), I(CardList.Length(p))));
        *)
        WHILE p # NIL DO
          WITH idx = p.head,
               old = f.get(idx),
               new = BDD.MakeFalse(old, ch[sp,sl]),
               msg = NARROW(equations.get(idx),Eq).label DO
            (*Debug.Out(msg);*)
            (*AssertShannon(old, ch[sp,sl]);*)
            IF new = BDD.False() THEN 
              (*
              Debug.Out(F("MakeFalse %s,%s FAIL (%s)",I(sp),I(sl),Msg(idx)));
              Debug.Out("old " & BDD.Format(old));
              Debug.Out("new " & BDD.Format(new));
              *)
              reason := msg ;
              RETURN FALSE 
            END;
            IF old # new THEN f.modify(idx, new) END
          END;
          p := p.tail
        END;
        RETURN TRUE
      END MakeFalse;

    PROCEDURE PhysRepeatOK(p : CARDINAL; sp : XCard) : BOOLEAN =
      (* q runs over the physRepeatMinInterval-1 preceding placements *)
      VAR 
        q := p;
        pPhys : CARDINAL; 
        pPhysQ : CARDINAL;
      BEGIN
        WITH intSp = slots[p] DO
          IF intSp = -1 THEN RETURN TRUE END;
          pPhys := intPhys[intSp]
        END;
        pPhysQ := pPhys DIV quadWidth;
        FOR q := p-1 TO MAX(p-(physRepeatMinInterval-1),1) BY -1 DO
          IF slots[q] # -1 THEN
            WITH qPhys = intPhys[slots[q]],
                 qPhysQ = qPhys DIV quadWidth DO
              IF pPhysQ = qPhysQ THEN
                reason := "physical repeat rate";
                RETURN FALSE
              END
            END
          END
        END;
        RETURN TRUE
      END PhysRepeatOK;

    PROCEDURE SetSlot(p  : CARDINAL;
                      sp : XCard) : BOOLEAN =
      
      PROCEDURE Print(txt : TEXT) =
        BEGIN
          IF printAttempt THEN
            VAR
              wx := Wx.New();
            BEGIN
              Wx.PutInt(wx, attempts);
              Wx.PutText(wx, " : ");
              FOR i := 0 TO p DO
                Wx.PutInt(wx, slots[i]);
                Wx.PutChar(wx, ' ')
              END;
              Wx.PutText(wx, "[slot ");
              Wx.PutInt(wx, p);
              Wx.PutText(wx, "] ");
              Wx.PutText(wx, txt);
              Debug.Out(Wx.ToText(wx))
            END;
          END
        END Print;
        
      VAR
        printAttempt := TRUE;
        print : BOOLEAN;
      BEGIN

        slots[p] := sp;

        print := Verbose OR attempts MOD 1000 = 0  ;

        IF NOT PhysRepeatOK(p, sp) THEN
          INC(attempts);
          IF print THEN Print(reason) END;
          RETURN FALSE
        END;

        FOR i := 0 TO NUMBER(ports^)-1 DO
          IF i = sp THEN 
            IF NOT MakeTrue(i,p) THEN 
              INC(attempts);
              IF print THEN Print(reason) END;
              RETURN FALSE
            END
          ELSE
            IF NOT MakeFalse(i,p) THEN 
              INC(attempts);
              IF print THEN Print(reason) END;
              RETURN FALSE 
            END
          END
        END;
        (*
        Debug.Out(F("SetSlot %s -> %s SUCCESS",I(p),I(sp)));
        *)

        INC(attempts);
        IF print THEN Print("OK") END;
        RETURN TRUE
      END SetSlot;

    PROCEDURE OKtoTry(try : XCard; VAR whyNot : TEXT) : BOOLEAN=
      VAR
        cnt := 0;

      BEGIN
        FOR i := 0 TO p-1 DO
          IF slots[i] = try THEN INC(cnt) END
        END;
        
        IF    try = -1 AND cnt = n1cnt THEN
          whyNot := "out of idle slots";
          RETURN FALSE
        ELSIF try # -1 AND cnt = ni[try] THEN
          whyNot := "out of slots for that port";
          RETURN FALSE
        ELSE
          (* should assert cnts not too much *)
          RETURN TRUE
        END
      END OKtoTry;

    BEGIN
      IF p = nslots THEN
        Debug.Out("Success!"); 
        RETURN TRUE
      END;

      (* each slot can have NUMBER(ports^)+1 values (idle=-1, plus any port) *)
      (* the first value we try is the previous port in the schedule + 1.
         then we try prev + 2, ... , all the way back to prev.

         we do this by calculating prev + incr, where incr goes from 2 to 
         the required number.

         we then calculate the try out of that ... *)

      FOR incr := 2 TO NUMBER(ports^)+2 DO
        VAR 
          prev, try : XCard;
          whyNot : TEXT;
          oldJitter : JitterState;
        BEGIN
          (* strategy: 
             1. calculate the port number of the previous port in the 
             schedule...*)

          IF p = 0 THEN 
            prev := NUMBER(ports^)-1
          ELSE
            prev := slots[p-1]
          END;

          try := (prev+incr) MOD(NUMBER(ports^)+1) - 1;

          (* ... we should try setting slot "p" to "try" *)

          AssertJitterOK(try); (* invariant not violated *)

          IF attempts > maxAttempts THEN RETURN FALSE END;

          (* save state (optimization) *)
          oldJitter := SaveJitter(try);
          IF OKtoTry(try,whyNot) AND SetJitterOK(p,try,(*OUT*)whyNot) THEN
            f := NEW(Equations).init(e);
        
            IF SetSlot(p, try) THEN
              IF Recurse(p+1, f) THEN arr[p] := try; RETURN TRUE END;
            END;

          ELSE
            IF Verbose THEN
              Debug.Out(F("[skipping %s, %s]", I(try), whyNot))
            END
          END;

          (* restore state *)
          ResetJitter(oldJitter, try);
          AssertJitterOK(try) (* invariant not violated *)

        END
      END;
      RETURN FALSE
    END Recurse;

  PROCEDURE FormatJitter(READONLY arr : ARRAY OF Jitter; i : CARDINAL) : TEXT =
    VAR
      j := arr[i];
      l := arr[j.leader];
    BEGIN
      RETURN F("port %s leader %s ",
               I(i), I(j.leader)) &
             F("selfmin %s max %s grpmin %s max %s",
               I(j.selfMin), I(j.selfMax), I(l.groupMin), I(l.groupMax));
    END FormatJitter;

  PROCEDURE SetJitterOK(p          : CARDINAL; 
                        try        : XCard;
                        VAR whyNot : TEXT) : BOOLEAN =

    (* if we set slot [p] to [try], and that does not violate jitter
       requirements, then return TRUE

       else, set whyNot to a descriptive message why it is not OK, and 
       return FALSE 

       REQUIRES that all preceding slots are already defined.
    *)
    BEGIN
      IF try # -1 AND JitterEnforced(try) THEN

        AssertJitterOK(try);
        (* precondition of the check is that jitter is OK *)

        UpdateJitter(p, try);
        IF Verbose THEN
          Debug.Out(F("Jitter update %s @ %s: ",I(try),I(p)) & 
            FormatJitter(jitter^, try))
        END;
        WITH gJitter = jitter[jitter[try].leader] DO
          IF gJitter.groupMax - gJitter.groupMin > deltaGapMax THEN
            whyNot := "jitter condition violated (local)";
            RETURN FALSE
          END;

          (* here we can check the "future jitter condition" *)
          (* given current groupMax and groupMin, can we bridge the gap
             from the present position of sp@p to the first sp in the 
             list with an integral number of steps that do not violate the
             jitter conditions? *)

          (* the current jitter of the group is 
             gJitter.groupMax - gJitter.groupMin.

             in the future, we MIGHT have a max jitter as much as the current
             min + the max allowed delta
             
             ... OR a min jitter as little as the current max - the max
             allowed delta
             
             (Possibly not both, but we dont know which at this time)
          *)
          VAR
            futureMax := gJitter.groupMin + deltaGapMax;
            futureMin := gJitter.groupMax - deltaGapMax;
            steps := 1;
            cur := p;                     (* going from *)
            tgt := p;                     (* going to *)
            distance : CARDINAL;
            success := FALSE;
          BEGIN
            (* find the first occurrence of this port in the schedule,
               that is the one we are shooting for *)
            FOR i := 0 TO p-1 DO
              IF slots[i] = try THEN tgt := i; EXIT END
            END;

            IF tgt = p THEN
              (* single occurrence, quit now *)
              RETURN TRUE
            END;

            distance := nslots + tgt - cur;

            IF Verbose THEN
              Debug.Out(F("FutureGap cur %s tgt %s dist %s", 
                          I(cur), I(tgt), I(distance)) &
                        F(" futureMin %s futureMax %s", 
                          I(futureMin), I(futureMax)))
            END;

            REPEAT
              success := distance >= steps * futureMin AND
                         distance <= steps * futureMax;
              INC(steps)
            UNTIL success OR steps * futureMin > distance;

            IF NOT success THEN
              whyNot := "cannot bridge wrap with integral steps and maintain jitter condition";
              RETURN FALSE
            END
          END
        END
      END;

      IF p = nslots-1 THEN
        slots[p] := try;
        (* check ALL jitter groups across the break *)
        VAR
          finalJitter := NEW(REF ARRAY OF Jitter, NUMBER(jitter^));
          last, first : CARDINAL;
        BEGIN
          finalJitter^ := jitter^;
          FOR i := 0 TO NUMBER(ports^)-1 DO
            (* calc gap across the jump *)
            FOR j := nslots-1 TO 0 BY -1 DO
              IF slots[j] = i THEN last := j; EXIT END
            END;
            FOR j := 0 TO nslots-1 DO
              IF slots[j] = i THEN first := j; EXIT END
            END;
            WITH thisGap = first+nslots-last,
                 l       = finalJitter[i].leader DO
              IF Verbose THEN
                Debug.Out("finalJitter : " & FormatJitter(finalJitter^, i))
              END;

              finalJitter[i].selfMin := MIN(thisGap, finalJitter[i].selfMin);
              finalJitter[i].selfMax := MAX(thisGap, finalJitter[i].selfMax);
              finalJitter[l].groupMin := MIN(thisGap, finalJitter[l].groupMin);
              finalJitter[l].groupMax := MAX(thisGap, finalJitter[l].groupMax);

              IF finalJitter[l].groupMax - finalJitter[l].groupMin > deltaGapMax THEN
                whyNot := F("jitter condition violated port %s grp %s thisGap %s groupJitter %s", I(i), I(l), I(thisGap), I(finalJitter[l].groupMax - finalJitter[l].groupMin));
                RETURN FALSE
              END
            END              
          END
        END
      END;
      RETURN TRUE
    END SetJitterOK;

  PROCEDURE UpdateJitter(p   : CARDINAL;
                         try : CARDINAL) =
    VAR
      gap := LAST(CARDINAL);
      ldr := jitter[try].leader;

    PROCEDURE Update() = 
      BEGIN
        jitter[try].selfMin := MIN(jitter[try].selfMin, gap);
        jitter[try].selfMax := MAX(jitter[try].selfMax, gap);
        jitter[ldr].groupMin := MIN(jitter[ldr].groupMin, gap);
        jitter[ldr].groupMax := MAX(jitter[ldr].groupMax, gap);
      END Update;

    BEGIN
      IF Verbose THEN
        Debug.Out(F("UpdateJitter: p %s try %s p-1=%s", I(p), I(try), I(p-1)))
      END;
      FOR i := p-1 TO 0 BY -1 DO
        IF Verbose THEN
          Debug.Out(F("UpdateJitter: i=%s slots[i]=%s try=%s", 
                      I(i), I(slots[i]), I(try)))
        END;

        IF slots[i] = try THEN 
          gap := p-i; 
          IF Verbose THEN
            Debug.Out(F("p %s i %s gap %s", I(p), I(i), I(gap)))
          END;
          Update();
          EXIT 
        END
      END
    END UpdateJitter;

  VAR
    arr, slots := NEW(REF ARRAY OF [-1 ..LAST(CARDINAL)], nslots);
    n1cnt := nslots;
    slSeq := NEW(REF ARRAY OF CardSeq.T, NUMBER(ni));

  BEGIN
    FOR i := FIRST(ni) TO LAST(ni) DO
      n1cnt := n1cnt-ni[i];
      slSeq[i] := NEW(CardSeq.T).init()
    END;
    Debug.Out("-1 slots total: " & I(n1cnt));

    FOR i := FIRST(arr^) TO LAST(arr^) DO
      arr[i] := LAST(CARDINAL);
    END;

    VAR 
      done := Recurse(0, NEW(Equations).init());
      intPhysNm, intLogNm : TEXT;
    BEGIN
      IF done THEN
        FOR i := FIRST(arr^) TO LAST(arr^) DO
          IF arr[i] = -1 THEN
            intPhysNm := "IDLE";
            intLogNm  := "IDLE"
          ELSE
            intPhysNm := " " & I3(intPhys[arr[i]]);
            intLogNm  := " " & I3(intLog [arr[i]])
          END;
            
          Debug.Out(F("slot %s int-port %s phys-port %s log-port %s", 
                      I3(i), I3(arr[i]), intPhysNm, intLogNm
          ))
        END
      END;
      RETURN done
    END
  END SatisfyAssertions;

(**********************************************************************)

PROCEDURE RepTime(bitRate : LONGREAL) : LONGREAL =
  (* given a port running at bitRate b.p.s., what is the minimum
     required repetition time (on average)? 
     
     assumption: it is equal to 84 byte times 
  *)
  BEGIN RETURN minRepBits / bitRate END RepTime;

PROCEDURE AssertNoUnderrun(sp : CARDINAL) =
  BEGIN
    FOR sl := 0 TO nslots - 1 DO
      (* on the assumption that we are allocated to slot "sl", what else can
         we say? *)
      WITH deadlineCyc = RepTime(ports[sp]) * underrunMax * clock DO
        (*
        Debug.Out(F("AssertNoUnderrun(%s) deadline=%s in slot %s", I(sp), L(deadlineCyc), I(sl)));
        *)
        VAR x := BDD.False(); 
        BEGIN
          FOR i := 1 TO FLOOR(deadlineCyc) DO
            (*
            Debug.Out(F("sp=%s might have slots (%s and %s)", 
                        I(sp), I(sl), I((sl+i) MOD nslots)));
             *)
            x := Or(x, ch[sp, (sl+i) MOD nslots])
          END;
          Assert(F("no underrun %s", I(sp)), Implies(ch[sp,sl], x))
        END
      END;

      (* rep rate *)
      FOR i := 1 TO minPortRep-1 DO
        WITH x = Implies(ch[sp,sl], Not(ch[sp,(sl+i) MOD nslots])) DO
          Assert(F("min rep rate (port %s) = %s", I(sp), I(i)), x)
        END
      END
    END
  END AssertNoUnderrun;

PROCEDURE AssertPortAppears(sp : CARDINAL) =
  VAR
    x := BDD.False();
  BEGIN
    FOR sl := 0 TO nslots-1 DO
      x := Or(x, ch[sp,sl])
    END;
    Assert(F("port appears in schedule %s", I(sp)), x)
  END AssertPortAppears;

PROCEDURE AssertNoneInIdleSlot() =
  (* 0 is the idle slot, dont assign me to it *)
  BEGIN
    FOR sp := FIRST(ports^) TO LAST(ports^) DO
      WITH x = Not(ch[sp,0]) DO
        Assert(F("port %s not in idle slot", I(sp)), x)
      END
    END
  END AssertNoneInIdleSlot;

PROCEDURE DoPortSpeedAssertions(idx : CARDINAL) =
  VAR
    indices := NEW(CardSeq.T).init();
  BEGIN
    FOR j := FIRST(ports^) TO LAST(ports^) DO
      IF ports[j] = ports[idx] THEN
        indices.addhi(j);
        Debug.Out(F("Setting leader of %s to %s", I(j), I(indices.get(0))));
        jitter[j].leader := indices.get(0) (* record the group leader *)
      END
    END
  END DoPortSpeedAssertions;

PROCEDURE DoSpeedAssertions() =
  BEGIN
    jitter := NEW(REF ARRAY OF Jitter, NUMBER(ports^));

    (* scan ports and make cross-port assertions *)
    FOR i := FIRST(ports^) TO LAST(ports^) DO
      WITH thisSpeed = ports[i] DO
        IF NOT donePorts.member(thisSpeed) THEN
          Debug.Out("Doing speed assertions for speed " & L(thisSpeed));
          DoPortSpeedAssertions(i);
          EVAL donePorts.insert(thisSpeed)
        END
      END
    END;
  END DoSpeedAssertions;

TYPE 
  Jitter = RECORD
    selfMin := LAST(CARDINAL);
    selfMax := FIRST(CARDINAL);
    leader  : CARDINAL;
    groupMin := LAST(CARDINAL);  (* only def'd for leader *)
    groupMax := FIRST(CARDINAL); (* only def'd for leader *)
  END;

VAR jitter : REF ARRAY OF Jitter;

VAR minGap, maxGap : REF ARRAY OF LONGREAL;

PROCEDURE SlotFree(sl : CARDINAL) : BDD.T =
  VAR
    x := BDD.True();
  BEGIN
    FOR i := 0 TO NUMBER(ports^)-1 DO
      x := And(x, Not(ch[i,sl]))
    END;
    RETURN x
  END SlotFree;

PROCEDURE AssertNotTooManyFree(maxFree : CARDINAL) =
  VAR 
    x := BDD.True();
  BEGIN
    FOR sl := 0 TO maxFree-1 DO
      x := And(x, SlotFree(sl))
    END;
    FOR i := maxFree TO nslots-1 DO
      WITH y = Implies(x, Not(SlotFree(i))) DO
        (*
        Debug.Out(F("maxFree=%s i=%s : %s", I(maxFree), I(i), BDD.Format(y)));
        *)

        Assert(F("at most the first %s slots are free : %s", I(maxFree), I(i)),
               y)
      END
    END
  END AssertNotTooManyFree;

VAR donePorts : LongrealSet.T;

PROCEDURE Attempt(READONLY ni : ARRAY OF CARDINAL) : BOOLEAN =
  (* speed i has ni[i] entries in schedule *)

  VAR
    maxFree := nslots;
  BEGIN

    donePorts := NEW(LongrealSetDef.T).init();

    minGap := NEW(REF ARRAY OF LONGREAL, NUMBER(ni));
    maxGap := NEW(REF ARRAY OF LONGREAL, NUMBER(ni));

    ClearEquations();

    AssertNoneInIdleSlot();

    FOR i := FIRST(ni) TO LAST(ni) DO

      AssertPortAppears(i);

      AssertNoUnderrun(i);

      Debug.Out(F("i=%s ni[i]=%s minct[i]=%s", I(i), I(ni[i]), I(minct[i])));
      <* ASSERT ni[i] >= minct[i] *>
      WITH aveDelta = FLOAT(nslots, LONGREAL) / FLOAT(ni[i],LONGREAL) DO
        minGap[i] := MAX(FLOAT(minPortRep,LONGREAL), 
                         aveDelta - FLOAT(deltaGapMax,LONGREAL));
        maxGap[i] := aveDelta + FLOAT(deltaGapMax,LONGREAL);

        AssertNoSmallerGap         (i, minGap[i]);

        AssertAlwaysExistsSuccessor(i, minGap[i], maxGap[i])
      END;
      maxFree := maxFree - ni[i]
    END;

    Debug.Out("Max free slots allowed: " & I(maxFree));

    AssertNotTooManyFree(maxFree);

    DoSpeedAssertions();

    (*
    AssertAssignmentsAreExclusive();
    *)

    Debug.Out(F("There are %s unreduced equations", I(equations.size())));

    TRY
    (* dump equations to disk for reference *)
    VAR 
      wr := FileWr.Open("equations.dat");
    BEGIN
      FOR i := 0 TO equations.size()-1 DO
        WITH eq = NARROW(equations.get(i),Eq) DO
          Wr.PutText(wr, F("%s %s %s\n", I(i), I(BDD.Size(eq.eq)), eq.label))
        END
      END;
      Wr.Close(wr)
    END;
    EXCEPT
      OSError.E(al) => Debug.Warning("Couldnt open equations output file, skipping: " & AL.Format(al))
    END;

    ReduceEquations();

    RecordDependencies();

    RETURN SatisfyAssertions(ni)
  END Attempt;

TYPE Phase = { LimitedAttempts, UnlimitedAttempts };

VAR
  phase := Phase.UnlimitedAttempts;

VAR
  maxAttempts : CARDINAL;
  doAttempts  : CARDINAL := 0;

PROCEDURE PrintDependSet(s : BDDSet.T) =
  VAR iter := s.iterate();
      b : BDD.T;
      p : IntPair.T;
      lookup : BOOLEAN;
  BEGIN
    WHILE iter.next(b) DO
      lookup := bddTbl.get(b, p);
      <*ASSERT lookup*>
      Debug.Out(F("(%s,%s)", I(p.k1), I(p.k2)))
    END
  END PrintDependSet;

VAR print := -1; pMu := NEW(MUTEX);

PROCEDURE ReduceEquations() =
  VAR
    arr2 := NEW(REF ARRAY OF BDDSet.T, equations.size());
    tbl := NEW(BDDSetHashRefTbl.Default).init();
  BEGIN
    FOR a := FIRST(arr2^) TO LAST(arr2^) DO
      arr2[a] := NEW(BDDSetHash.T).init()
    END;

    Debug.Out("Calculating dependencies...");
    FOR i := 0 TO equations.size()-1 DO
      LOCK pMu DO print := i END;
      VAR 
        eq : Eq := equations.get(i);
      BEGIN
        arr2[i] := BDDDepends.Depends(eq.eq);
      END
    END;

    print := -1;

    (* now hash everything *)
    FOR i := 0 TO equations.size() -1 DO
      VAR new := NEW(BDDSetHash.T).init();
          iter := arr2[i].iterate();
          b : BDD.T;
      BEGIN
        WHILE iter.next(b) DO
          EVAL new.insert(b)
        END;
        arr2[i] := new
      END
    END;

    FOR i := 0 TO equations.size()-1 DO
      VAR 
        old, new : REFANY := NIL;
      BEGIN
        EVAL tbl.get(arr2[i], old);
        new := CardList.Cons(i, old);
        EVAL tbl.put(arr2[i], new)
      END
    END;

    Debug.Out(I(tbl.size()) & " reduced equations");

    VAR newEquations := NEW(RefSeq.T).init(); 
        iter := tbl.iterate();
        s : BDDSetHash.T;
        r : REFANY;
    BEGIN
      WHILE iter.next(s, r) DO
        VAR p := NARROW(r, CardList.T); 
            q := BDD.True();
            t := "";
        BEGIN
          WHILE p # NIL DO
            WITH eq = NARROW(equations.get(p.head),Eq) DO
              q := BDD.And(eq.eq, q);
              t := t & eq.label;
              IF p.tail # NIL THEN t := t & " AND " END;
              p := p.tail
            END
          END;
          newEquations.addhi(NEW(Eq, label := t, eq := q))
          (* at this point we have all the info needed for 
             RecordDependencies..*)
        END
      END;
      equations := newEquations
    END
  END ReduceEquations;

PROCEDURE RecordDependencies() =
  VAR
    b : BDD.T;
    x : BOOLEAN;
    k : IntPair.T;

  BEGIN
    deps := NEW(REF ARRAY OF ARRAY OF CardList.T, NUMBER(ch^), NUMBER(ch[0]));
    FOR i := FIRST(deps^) TO LAST(deps^) DO
      FOR j := FIRST(deps[0]) TO LAST(deps[0]) DO
        deps[i,j] := NIL
      END
    END;
    FOR i := 0 TO equations.size() -1 DO
      WITH d    = BDDDepends.Depends(NARROW(equations.get(i),Eq).eq),
           iter = d.iterate() DO
        WHILE iter.next(b) DO
          x := bddTbl.get(b, k);
          <*ASSERT x*>
          deps[k.k1,k.k2] := CardList.Cons(i, deps[k.k1,k.k2])
        END
      END
    END
  END RecordDependencies;

PROCEDURE AssertNoSmallerGap(sp  : CARDINAL;
                             gap : LONGREAL) =
  BEGIN
    Debug.Out(F("AssertNoSmallerGap(sp=%s gap=%s)", I(sp), L(gap)));
    FOR sl := 0 TO nslots-1 DO
      FOR j := sl+1 TO sl + CEILING(gap)-1 DO
        WITH str = F("sp=%s ruling out slots (%s and %s)", 
                     I(sp), I(sl), I(j MOD nslots)) DO
          Assert(str, Implies(ch[sp,sl], Not(ch[sp,j MOD nslots])));
          (*
          Debug.Out(str)
          *)
        END
      END
    END
  END AssertNoSmallerGap;

PROCEDURE AssertAlwaysExistsSuccessor(sp : CARDINAL;
                                      minGap, maxGap : LONGREAL) =
  VAR
    x : BDD.T;
  BEGIN
    Debug.Out(F("AssertAlwaysExistsSuccessor(sp=%s minGap=%s maxGap=%s)", I(sp), L(minGap), L(maxGap)));
    FOR sl := 0 TO nslots-1 DO
      x := BDD.False();
      FOR j := sl+CEILING(minGap) TO sl+FLOOR(maxGap) DO
        x := Or(x, ch[sp, j MOD nslots]);
        (*
        Debug.Out(F("sp=%s might have slots (%s and %s)", 
                    I(sp), I(sl), I(j MOD nslots)));
                    *)
      END;
      Assert(F("sp=%s has slot %s -> has slot in [%s,%s])",
               I(sp), I(sl), I(sl+CEILING(minGap)), I(sl+FLOOR(maxGap))), 
             Implies(ch[sp,sl], x))
    END
  END AssertAlwaysExistsSuccessor;

VAR slack : INTEGER;

VAR ct : REF ARRAY OF CARDINAL;

VAR bddTbl : BDDIntPairTbl.T;

PROCEDURE Solve() : TEXT (* NIL on success *)=

  VAR
    tt := 1.0d0 / clock * FLOAT(nslots,LONGREAL); 
    (* total time to run through schedule *)
    
    s_ccnt : CARDINAL := 0;

  BEGIN
    (* calculate how many entries each speed needs *)
    minct  := NEW(REF ARRAY OF CARDINAL, NUMBER(ports^));
    ct     := NEW(REF ARRAY OF CARDINAL, NUMBER(ports^));
    FOR i := FIRST(minct^) TO LAST(minct^) DO
      WITH rt   = RepTime(ports[i]),
           cnt  = tt / rt,
           ccnt = CEILING(cnt) DO
        Debug.Out(F("Speed idx %s spd %s rt %s cnt %s ccnt %s",
                    I(i), L(ports[i]), L(rt), L(cnt), I(ccnt)));
        minct[i] := ccnt;
        s_ccnt := s_ccnt + ccnt
      END
    END;

    slack := nslots - s_ccnt - 1;
    
    IF slack < 0 THEN RETURN "not enough slots" END;

    ch := NEW(REF ARRAY OF ARRAY OF BDD.T, NUMBER(ports^), nslots);
    bddTbl := NEW(BDDIntPairTbl.Default).init();
    FOR i := FIRST(ch^) TO LAST(ch^) DO
      FOR j := FIRST(ch[0]) TO LAST(ch[0]) DO
        ch[i,j] := BDD.New(F("ch[%s,%s]",I(i),I(j)));
        EVAL bddTbl.put(ch[i,j], IntPair.T { i,j })
      END
    END;

    VAR 
      extra := NEW(REF ARRAY OF CARDINAL, NUMBER(ports^));
      success : BOOLEAN;
    BEGIN
      FOR appslack := 0 TO slack DO
        (* apportion "appslack" among ports *)
        Zero(extra^);
        Debug.Out("Apportioning slot slack of " & I(appslack));
        extra[0] := appslack;
        
        REPEAT
          Debug.Out("slack is apportioned " & FmtSlack(extra^));
          AddSlack(ct^, minct^, extra^);
          success := Attempt(ct^);
          IF success THEN RETURN NIL END
        UNTIL success OR NOT StepSlack(extra^);
      END;
      RETURN "No schedule found"
    END
  END Solve;

PROCEDURE FmtSlack(READONLY arr : ARRAY OF CARDINAL) : TEXT =
  VAR
    s := "";
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO
      s := s & " " & I(arr[i])
    END;
    RETURN s
  END FmtSlack;

PROCEDURE StepSlack(VAR arr : ARRAY OF CARDINAL) : BOOLEAN =
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr)-1 DO
      IF arr[i] # 0 THEN
        INC(arr[i+1]); DEC(arr[i]); RETURN TRUE
      END
    END;
    RETURN FALSE
  END StepSlack;

PROCEDURE AddSlack(VAR c          : ARRAY OF CARDINAL;
                   READONLY a, b  : ARRAY OF CARDINAL) =
  BEGIN
    FOR i := FIRST(c) TO LAST(c) DO c[i] := a[i] + b[i] END
  END AddSlack;

PROCEDURE Zero(VAR a : ARRAY OF CARDINAL) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO a[i] := 0 END
  END Zero;

VAR nslots : CARDINAL;

PROCEDURE ParseInput() =
  VAR rd := Stdio.stdin;
      reader : TextReader.T;
      portseq    := NEW(LongRealSeq.T).init();
      portnumseq := NEW(CardSeq.T).init();
      
  CONST 
    White = "\t ";

  PROCEDURE Get() : TEXT RAISES { TextReader.NoMore } = 
    BEGIN RETURN reader.nextE(White, skipNulls := TRUE) END Get;

  VAR
    cmd : TEXT;
    lNo := 0;
  BEGIN
    TRY
      LOOP
        WITH line   = Rd.GetLine(rd) DO
          INC(lNo);
          reader := NEW(TextReader.T).init(line);
          IF reader.next(White, cmd, skipNulls := TRUE) AND 
             Text.GetChar(cmd, 0) # '#' THEN
            IF TE(cmd, "CLOCK")       THEN 
              clock := Scan.LongReal(Get()) 
            ELSIF TE(cmd, "SINGLESPEED") THEN 
              singleSpeed := Scan.LongReal(Get()) 
            ELSIF TE(cmd, "JITTERSPEED") THEN 
              jitterSpeed := Scan.LongReal(Get()) 
            ELSIF TE(cmd, "UNDERRUNMAX") THEN 
              underrunMax := Scan.LongReal(Get()) 
            ELSIF TE(cmd, "MINREPBITS") THEN 
              minRepBits := Scan.LongReal(Get()) 
            ELSIF TE(cmd, "MINPORTREP") THEN
              minPortRep := Scan.Int(Get())
            ELSIF TE(cmd, "DELTAGAPMAX") THEN
              deltaGapMax := Scan.Int(Get())
            ELSIF TE(cmd, "N") THEN
              N := Scan.Int(Get())
            ELSIF TE(cmd, "QUADWIDTH") THEN
              quadWidth := Scan.Int(Get())
            ELSIF TE(cmd, "MINPHYSREP") THEN
              physRepeatMinInterval := Scan.Int(Get())
            ELSIF TE(cmd, "ATTEMPTS") THEN
              phase := Phase.LimitedAttempts;
              doAttempts := Scan.Int(Get())
            ELSIF TE (cmd, "PORT") THEN
              WITH physNum = Scan.Int(Get()),
                   logNum = Scan.Int(Get()),
                   speed   = Scan.LongReal(Get()) DO
                portnumseq.addhi(physNum);
                portseq.addhi(speed);

                intPhys[portnumseq.size()-1] := physNum;
                intLog [portnumseq.size()-1] := logNum;

                IF speed > singleSpeed THEN
                  IF physNum MOD 4 # 0 THEN
                    Debug.Error(F("High-speed port %s spd %s unaligned!",
                                  I(physNum), L(speed)))
                  END;
                  FOR i := 0 TO 3 DO
                    MarkPhysPort(physNum+i, physNum)
                  END
                ELSE
                  MarkPhysPort(physNum, physNum)
                END
              END
            ELSE
              Debug.Error("Unknown command \"" & cmd & "\"")
            END
          END
        END    
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    |
      Rd.Failure, Lex.Error, FloatMode.Trap, TextReader.NoMore => 
      Debug.Error("Parse error in input, line " & I(lNo))
    END;
    ports := NEW(REF ARRAY OF LONGREAL, portseq.size());
    FOR i := 0 TO portseq.size()-1 DO
      ports[i] := portseq.get(i)
    END
  END ParseInput;

CONST (* these are hard-coded but could be put in config file at some
         cost in code complexity *)
  PhysPorts = 48;
  LogPorts  = 64;

TYPE XCard = [-1..LAST(CARDINAL)]; (* really lazy usage... *)

PROCEDURE MarkPhysPort(pp, as : CARDINAL) =
  BEGIN
    IF physUse[pp] # -1 THEN
      Debug.Error(F("Physical port overlap at port %s", I(pp)))
    END;
    physUse[pp] := as
  END MarkPhysPort;

VAR
  (* three port-numbering schemes in use.

     1. internal port numbering (internal to this program)
        from 0 to #ports-1, in order of input file (also called 
        speed or "sp" in older parts of the code)

     2. physical ports demanded by input file
     
     3. logical ports demanded by input file 
     
  *)

  physUse := ARRAY [0..PhysPorts-1] OF XCard { -1, .. };
  (* usage of physical ports *)

  intPhys := ARRAY [0..LogPorts-1] OF XCard { -1, .. };
  (* mapping of program-internal ports to physical ports *)
  
  intLog  := ARRAY [0..LogPorts-1] OF XCard { -1, .. };
  (* mapping of program-internal ports to logical ports *)
  
VAR
  attempts : CARDINAL;

VAR physRepeatMinInterval := 4;
    quadWidth             := 4;

PROCEDURE RunThroughScheduleLengths() : BOOLEAN =
  BEGIN
    FOR i := 1 TO N DO
      Debug.Out(F("\n======================================================================\nTrying %s slots", I(i)));
      nslots := i;

      IF phase = Phase.LimitedAttempts AND doAttempts # 0 THEN
        maxAttempts := attempts + doAttempts*i*i;
      ELSE 
        maxAttempts := LAST(CARDINAL)
      END;

      WITH s = Solve() DO
        IF s # NIL THEN
          Debug.Out("Failed: " & s)
        ELSE
          Debug.Out("Success!"); RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END RunThroughScheduleLengths;
  
BEGIN

 (* EVAL Thread.Fork(NEW(Thread.Closure, apply := PApply));*)

  ParseInput();

  Debug.Out(I(NUMBER(ports^)) & " ports");
  FOR i := FIRST(ports^) TO LAST(ports^) DO
    Debug.Out(F("port %s speed %s", I(i), L(ports[i])));
  END;

  attempts := 0;

  LOOP
    IF RunThroughScheduleLengths() THEN 
      (* done *)
      EXIT
    END;

    IF phase = LAST(Phase) THEN 
      EXIT
    ELSE
      INC(phase)
    END
  END;
  
  Debug.Out(I(attempts) & " schedules examined.")

END Main.

