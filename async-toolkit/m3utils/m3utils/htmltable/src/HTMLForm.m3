MODULE HTMLForm;
IMPORT HTML;
IMPORT Request,HTMLLink;

TYPE
  FormFields = REF ARRAY OF HTML.T;

REVEAL
  T = Public BRANDED "HTML Form" OBJECT
    fields : FormFields  := NIL;
    action : TEXT := NIL;
  OVERRIDES
    init := Init;
    add := Add;
    format := Format;
  END;

PROCEDURE Init(self : T; action : TEXT; request : Request.T) : T =
  BEGIN self.action := HTMLLink.MakeURL(action,request); RETURN self END Init;

PROCEDURE Add(self : T; stuff : HTML.Stuff) =
  VAR
    newfields : FormFields;
  BEGIN
    IF self.fields = NIL THEN self.fields := NEW(FormFields,0) END;

    newfields := NEW(FormFields, NUMBER(self.fields^) + 1);
    
    SUBARRAY(newfields^,0,NUMBER(newfields^)-1) :=
        self.fields^;

    newfields[LAST(newfields^)] := HTML.Wrap(stuff);

    self.fields := newfields
  END Add;

PROCEDURE Format(self : T) : TEXT =
  VAR
    res := "<form method=POST action=\"" & self.action & "\">";
  BEGIN
    FOR i := FIRST(self.fields^) TO LAST(self.fields^) DO
      res := res & self.fields[i].format() 
    END;
    res := res & "</form>\n";
    RETURN res
  END Format;

BEGIN END HTMLForm.
