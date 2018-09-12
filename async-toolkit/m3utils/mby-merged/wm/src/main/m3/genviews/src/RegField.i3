INTERFACE RegField;
IMPORT RdlNum;
IMPORT RegComponent;

CONST
  Unspecified = LAST(CARDINAL);
  
TYPE
  T = RegComponent.T OBJECT
    width, lsb := Unspecified;
    defVal : RdlNum.T := NIL;
  METHODS
    name(debug := TRUE) : TEXT;
    (* if debug is set to TRUE the identifier shall include a comment
       (for the target language) showing its original form;
       if debug is set to FALSE only the identifier shall be returned *)
  END;

PROCEDURE Compare(a, b : T) : [-1..1];
  (* sort by lsb *)
  
CONST
  Brand = "RegField";

END RegField.
