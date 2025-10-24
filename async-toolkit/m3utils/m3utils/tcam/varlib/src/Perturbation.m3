MODULE Perturbation;
IMPORT PerturbationRep;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal;

REVEAL
  Default = PerturbationRep.Private BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
    v := GetValue;
  END;

PROCEDURE Init(t : Default; model, var : TEXT) : T =
  BEGIN
    t.model := model;
    t.var := var;
    RETURN t
  END Init;

PROCEDURE GetValue(t : Default; of : TEXT) : LONGREAL =
  VAR
    lr : LONGREAL;
  BEGIN
    <*ASSERT t.dict # NIL*>
    IF NOT t.dict.get(of, lr) THEN
      Debug.Error(F("No value for \"%s\" in perturbation of \"%s\" for model \"%s\"", of, t.var, t.model))
    END;
    Debug.Out(F("GetValue %s -> %s", of, LongReal(lr)));
    RETURN lr
  END GetValue;

BEGIN END Perturbation.
