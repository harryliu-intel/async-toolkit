(* $Id$ *)

GENERIC MODULE SXType(Elem);
IMPORT Time, SXClass, SX;
FROM SX IMPORT Uninitialized;

REVEAL 
  T = Public BRANDED Brand OBJECT
    v : Elem.T;
    updates : CARDINAL := 0;
    name : TEXT;
  OVERRIDES 
    update := Update;
    updateLocked := UpdateLocked;
    value := GetValue;
    waitFor := WaitFor;
    numUpdates := NumUpdates;
    uninitialize := Uninitialize;
    attachName := AttachName;
  END;

  Var = PublicVar BRANDED Brand & " Var" OBJECT 
  OVERRIDES
    set := SetVar;
    setLocked := SetVarLocked;
    initVal := InitVal;
  END;

  Const = PublicConst BRANDED Brand & " Const" OBJECT
  OVERRIDES
    init := InitConst;
  END;

PROCEDURE Uninitialize(t : T) = BEGIN t.updates := 0 END Uninitialize;

PROCEDURE InitVal(var : Var; val : Elem.T) : Var =
  BEGIN
    WITH me = NARROW(T.init(var),Var) DO
      me.v := val;
      me.updates := 1;
      RETURN me
    END
  END InitVal;

PROCEDURE AttachName(t : T; name : TEXT) =
  BEGIN t.name := name END AttachName;

PROCEDURE NumUpdates(t : T) : CARDINAL =
  BEGIN RETURN t.updates END NumUpdates;

PROCEDURE GetValue(t : T) : Elem.T RAISES { Uninitialized } = 
  BEGIN 
    IF t.updates = 0 THEN RAISE Uninitialized END;
    RETURN t.v 
  END GetValue;

PROCEDURE WaitFor(t : T; val : Elem.T) =
  BEGIN
    LOOP
      TRY
        IF Elem.Equal(t.value(),val) THEN RETURN END
      EXCEPT
        Uninitialized => (* skip *)
      END;
      t.wait()
    END
  END WaitFor;

(**********************************************************************)

PROCEDURE Update(v : T; newValue : Elem.T; when : Time.T) : BOOLEAN =
  BEGIN 
    LOCK v.mu DO RETURN v.updateLocked(newValue,when) END
  END Update;

PROCEDURE UpdateLocked(v : T; newValue : Elem.T; when : Time.T) : BOOLEAN =
  BEGIN 
    LOCK SX.mu DO
      IF v.v = newValue AND v.updates > 0 THEN
        RETURN FALSE
      ELSE
        v.v := newValue; v.updated := when; INC(v.updates);
        RETURN TRUE
      END
    END 
  END UpdateLocked;

PROCEDURE SetVar(v : Var; newValue : Elem.T; when : Time.T) =
  BEGIN 
    IF when = FIRST(Time.T) THEN when := Time.Now() END;
    IF v.update(newValue, when) THEN
      v.propagate(when)
    END
  END SetVar;

PROCEDURE SetVarLocked(v : Var; newValue : Elem.T; when : Time.T) =
  BEGIN
    IF when = FIRST(Time.T) THEN when := Time.Now() END;
    IF v.updateLocked(newValue, when) THEN
      v.propagate(when,TRUE)
    END
  END SetVarLocked;

PROCEDURE InitConst(c : Const; value : Elem.T) : Const = 
  BEGIN
    c.v := value;
    c.updates := 1;
    RETURN T.init(c)
  END InitConst;

PROCEDURE BaseCompare(a, b : Base) : INTEGER =
  BEGIN RETURN Elem.Compare(a,b) END BaseCompare;

PROCEDURE NewConst(v : Elem.T) : Const = 
  BEGIN RETURN NEW(Const).init(v) END NewConst;

BEGIN END SXType.
