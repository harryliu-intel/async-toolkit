(* $Id$ *)

MODULE HTMLList;
IMPORT HTML;

TYPE
  ListFields = REF ARRAY OF HTML.T;

REVEAL
  T = Public BRANDED "HTML List" OBJECT
    fields : ListFields  := NIL;
  OVERRIDES
    add := Add;
    format := Format;
  END;

PROCEDURE Add(self : T; stuff : HTML.Stuff) =
  VAR
    newfields : ListFields;
  BEGIN
    (* this is used e.g. in Hack below *)
    IF stuff = NIL THEN RETURN END;

    IF self.fields = NIL THEN self.fields := NEW(ListFields,0) END;
    newfields := NEW(ListFields, NUMBER(self.fields^) + 1);
    
    SUBARRAY(newfields^,0,NUMBER(newfields^)-1) :=
        self.fields^;

    newfields[LAST(newfields^)] := HTML.Wrap(stuff);

    self.fields := newfields
  END Add;

PROCEDURE Format(self : T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(self.fields^) TO LAST(self.fields^) DO
      res := res & self.fields[i].format() 
    END;
    RETURN res
  END Format;

PROCEDURE Hack(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10 : HTML.Stuff) : T =
  VAR
    res := NEW(T);
    args := ARRAY [1..10]OF HTML.Stuff { a1, a2, a3, a4, a5, a6, a7, a8, a9, a10 };
  BEGIN

    FOR i := FIRST(args) TO LAST(args) DO res.add(args[i]) END;
    RETURN res
  END Hack;

BEGIN END HTMLList.
