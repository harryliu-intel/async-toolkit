(* $Id$ *)

MODULE SX EXPORTS SX, SXSelect;
IMPORT SXClass;
IMPORT Thread, ThreadF;
IMPORT XTime AS Time;
IMPORT IntSet, IntSetDef;
IMPORT IntRefTbl;
IMPORT Debug;
IMPORT Fmt;
IMPORT RefList;
IMPORT SXRef;
IMPORT Integer, RefanyArraySort;
IMPORT Word;

(* methods for garbage collecting...?  Would imply using WeakRef. *)

(* 
   XXX XXX

   I'm worried about race conditions in the locking.  Some of these
   don't matter, if the signals are delivered "eventually", but some 
   probably are bad.  The problem is that we can go to sleep with an 
   idea that the old value has changed, but with an actual new value
   of the variable.  Might have to use the single global lock for
   sleeping.  Or grab it recursively, up and down the tree.

   Also the locking orders are bad.  If a user does

   c := BinaryOp(b,a)
   ...
   LOCK a.mu LOCK b.mu
     
   you can get deadlock just because of the computation of c.

   Likewise, you'll get problems if you LOCK dependent expressions (maybe).

   The solution to these problems lies in abandoning Modula-3's 
   block-structured LOCK operation (unfortunately) and making a 
   lock operation that calculates the proper locking order, probably
   based on an id field defined in this module.  All user code would
   then have the following appearance...

   Lock(onVars);
   TRY
     doCalcs(onVars)
   FINALLY
     Unlock(onVars)
   END

   XXX XXX
*)

REVEAL
  T = SXClass.Private BRANDED Brand OBJECT
    id : CARDINAL;
    selecters : IntSet.T; (* set of thread IDs *)
    c : Thread.Condition;
    when : Time.T;
    dependers : RefList.T; (* of type T *)
    dMu : MUTEX;
  OVERRIDES
    depends := Depends;
    propagate := Propagate;
    touch := Touch;
    init := InitT;
    wait := WaitT;
  END;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.id END Hash;

PROCEDURE Depends(t : T; depender : T) =
  BEGIN
    LOCK t.dMu DO
      t.dependers := RefList.Cons(depender,t.dependers)
    END
  END Depends;

PROCEDURE WaitT(t : T) =
  BEGIN Thread.Wait(t.mu,t.c) END WaitT;

VAR idMu := NEW(MUTEX);
VAR nextId : CARDINAL := 1;

PROCEDURE InitT(t : T) : T =
  BEGIN 
    LOCK idMu DO
      t.id := nextId; INC(nextId) 
    END;

    Debug.Out("SX.InitT, id = " & Fmt.Int(t.id));

    t.mu := NEW(MUTEX); t.c := NEW(Thread.Condition);
    t.dMu := NEW(MUTEX);
    t.dependers := NIL;
    t.selecters := NEW(IntSetDef.T).init();
    RETURN t 
  END InitT;

PROCEDURE Propagate(t : T; when : Time.T; locked : BOOLEAN) = 
  
  PROCEDURE DoIt() = 
    BEGIN
      IF t.selecters.size() > 0 THEN
        VAR
          s := t.selecters.iterate();
          i : INTEGER;
          c : REFANY;
        BEGIN
          WHILE s.next(i) DO
            IF threadCondTbl.get(i,c) THEN
              Thread.Broadcast(NARROW(c,ThreadLock).c)
            ELSE
              Debug.Warning("No condition variable found for thread #" & 
                Fmt.Int(i))
            END
          END
        END
      END
    END DoIt;

  VAR
    d : RefList.T;
  BEGIN

    Thread.Broadcast(t.c);

    LOCK t.dMu DO
      d := t.dependers;
      WHILE d # NIL DO
        NARROW(d.head,T).touch(when,locked);
        d := d.tail
      END
    END;

    IF locked THEN
      DoIt()
    ELSE
      LOCK t.mu DO DoIt() END
    END
  END Propagate;

PROCEDURE Touch(t : T; when : Time.T; locked : BOOLEAN) =
  BEGIN
    IF t.recalc(when) THEN t.propagate(when,locked) END
  END Touch;

PROCEDURE Wait(READONLY on : ARRAY OF T; touched : REF ARRAY OF BOOLEAN) = 
  <* FATAL Exception *>
  BEGIN WaitE(on, NIL, touched) END Wait;

PROCEDURE Wait1(on : T) = BEGIN Wait(ARRAY OF T { on }, NIL) END Wait1;

PROCEDURE WaitE(READONLY on : ARRAY OF T; 
                except : SXRef.T;
                touched : REF ARRAY OF BOOLEAN) RAISES { Exception } =

  PROCEDURE CheckExcept() RAISES { Exception } = 
    BEGIN
      IF except # NIL THEN
        TRY
          WITH val = except.value() DO
            IF val # NIL THEN RAISE Exception(val) END
          END
        EXCEPT
          Uninitialized => (* skip *)
        END;
        
        EVAL except.selecters.insert(ThreadF.MyId())
      END
    END CheckExcept;

  VAR
    r : REFANY;
    updates : REF ARRAY OF CARDINAL;
  BEGIN
    (* set up my waiting condition *)
    LOCK gMu DO
      IF NOT threadCondTbl.get(ThreadF.MyId(),r) THEN
        r := NEW(ThreadLock, tMu := NEW(MUTEX), c := NEW(Thread.Condition));
        EVAL threadCondTbl.put(ThreadF.MyId(),r)
      END
    END;

    CheckExcept();

    (*
    Debug.Out("Adding thread " & Fmt.Int(ThreadF.MyId()) & 
      " to " & Fmt.Int(NUMBER(on)) & " selecter lists");
    *)

    FOR i := FIRST(on) TO LAST(on) DO
      WITH t = on[i] DO
        EVAL t.selecters.insert(ThreadF.MyId())
      END
    END;

    WITH l = NARROW(r,ThreadLock) DO

      (* here we need to atomically unlock all the variables we're
         waiting on -- a kind of multi-wait *)
      LOCK l.tMu DO 
        (* record # of updates, if applicable *)
        IF touched # NIL THEN
          updates := NEW(REF ARRAY OF CARDINAL, NUMBER(touched^));
          FOR i := 0 TO MIN(NUMBER(updates^),NUMBER(on))-1 DO
            updates[i] := on[i].numUpdates()
          END
        END;

        (* unlock variables *)
        Unlock(on);
        IF except # NIL THEN Thread.Release(except.mu) END;

        Thread.Wait(l.tMu,l.c); 

        (* lock them again *)
        Lock(on);

        IF touched # NIL THEN
          FOR i := FIRST(touched^) TO LAST(touched^) DO
            touched[i] := (i <= LAST(updates^)) AND 
                          on[i].numUpdates() # updates[i]
          END
        END;

        IF except # NIL THEN Thread.Acquire(except.mu) END;
      END
    END;

    (* done waiting, delete me from wait list *)
    (*
    Debug.Out("Deleting thread " & Fmt.Int(ThreadF.MyId()) & 
      " from " & Fmt.Int(NUMBER(on)) & " selecter lists");
    *)

    FOR i := FIRST(on) TO LAST(on) DO
      WITH t = on[i] DO
        EVAL t.selecters.delete(ThreadF.MyId())
      END
    END;

    CheckExcept()
  END WaitE;

TYPE 
  ThreadLock = OBJECT
    c : Thread.Condition;
    tMu : MUTEX;
  END;

VAR gMu := NEW(MUTEX);
VAR threadCondTbl := NEW(IntRefTbl.Default).init(); 
    (* holds cond vars for each thread *)

PROCEDURE IdCompare(a, b : REFANY) : [-1..1] = 
  BEGIN
    WITH an = NARROW(a, T), bn = NARROW(b, T) DO
      RETURN Integer.Compare(an.id,bn.id) 
    END
  END IdCompare;

PROCEDURE Lock(READONLY arr : Array) =
  VAR
    a := NEW(REF ARRAY OF REFANY, NUMBER(arr));
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO 
      a[i] := arr[i]
    END;
    RefanyArraySort.Sort(a^,IdCompare);

    FOR i := FIRST(a^) TO LAST(a^) DO
      (* allow specifying same var multiple times *)
      IF i = FIRST(a^) OR a[i] # a[i-1] THEN
        Thread.Acquire(NARROW(a[i],T).mu)
      END
    END
  END Lock;

PROCEDURE Lock1(t : T) = BEGIN Thread.Acquire(t.mu) END Lock1;

PROCEDURE Unlock1(t : T) = BEGIN Thread.Release(t.mu) END Unlock1;

PROCEDURE Unlock(READONLY arr : Array) =
  VAR
    a := NEW(REF ARRAY OF REFANY, NUMBER(arr));
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO 
      a[i] := arr[i]
    END;
    RefanyArraySort.Sort(a^,IdCompare);

    FOR i := FIRST(a^) TO LAST(a^) DO
      (* allow specifying same var multiple times *)
      IF i = FIRST(a^) OR a[i] # a[i-1] THEN
        Thread.Release(NARROW(a[i],T).mu)
      END
    END
  END Unlock;

BEGIN 
  mu := NEW(MUTEX);
END SX.

