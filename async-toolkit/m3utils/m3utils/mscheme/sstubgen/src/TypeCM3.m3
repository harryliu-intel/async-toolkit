(* $Id$ *)
MODULE TypeCM3 EXPORTS Type;
IMPORT Atom, Value;

VAR nullAtm: Atom.T;

BEGIN
  nullAtm := Atom.FromText("");
  longint := NEW(Subrange, name := NEW(Qid, intf := nullAtm,
                                       item := Atom.FromText("LONGINT")),
                 min := NEW(Value.Longint, val := FIRST(LONGINT)),
                 max := NEW(Value.Longint, val := LAST(LONGINT)));
  longint.base := longint;
END TypeCM3.
