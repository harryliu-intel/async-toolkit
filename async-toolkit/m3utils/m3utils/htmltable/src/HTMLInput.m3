(* $Id$ *)

MODULE HTMLInput;
IMPORT Process;

CONST
  Tags = ARRAY Type OF TEXT { "button", "checkbox", "file", "hidden", "image",
                              "password", "radio", "reset", "submit", "text" };
  

REVEAL
  T = Public BRANDED "HTML Input" OBJECT
    type : Type;
    name, value, src, style, onkeypress, onkeyup : TEXT := NIL; 
  OVERRIDES
    init := Init;
    format := Format;
  END;

(* XXX unfinished *)
PROCEDURE Init(self : T; type : Type; 
               name, value, src, style, onkeypress, onkeyup : TEXT) : T =
  BEGIN
    (* basically validate the input *)
    self.type := type;
    self.name := name;
    self.src := src;
    self.value := value;
    self.style := style;
    self.onkeypress := onkeypress;
    self.onkeyup := onkeyup;

    IF self.type # Type.reset AND self.type # Type.submit AND 
      self.type # Type.image THEN <* ASSERT name # NIL *> END;

    CASE type OF 
    | Type.button, Type.text, Type.submit, Type.hidden, Type.password => 
    ELSE 
      Process.Crash("HTMLInput of type \"" & Tags[type] & "\" not yet supported.")
    END;
    RETURN self
  END Init;

PROCEDURE Format(self : T) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    res := "<input type=" & Tags[self.type];
    IF self.name # NIL THEN   res := res & " name=\""  & self.name & "\"" END;
    IF self.value # NIL THEN  res := res & " value=\"" & self.value & "\"" END;
    IF self.src # NIL THEN    res := res & " src="   & self.src END;
    IF self.style # NIL THEN res := res & " style=" & self.style END;
    IF self.onkeypress # NIL THEN
      res := res & " onkeypress=\"" & self.onkeypress & "\""
    END;
    IF self.onkeyup # NIL THEN
      res := res & " onkeyup=\"" & self.onkeyup & "\""
    END;

    res := res & ">\n";
    RETURN res
  END Format;

BEGIN END HTMLInput.
