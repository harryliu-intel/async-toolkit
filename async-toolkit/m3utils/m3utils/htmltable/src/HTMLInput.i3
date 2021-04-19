(* $Id$ *)

INTERFACE HTMLInput;
IMPORT HTML;

TYPE
  T <: Public;

  Type = { button, checkbox, file, hidden, image, password, radio,
           reset, submit, text };
  

  Public = HTML.T OBJECT
  METHODS
    init(type : Type; name, value, src, style, onkeypress, onkeyup : TEXT := NIL) : T;
  END;

END HTMLInput.
