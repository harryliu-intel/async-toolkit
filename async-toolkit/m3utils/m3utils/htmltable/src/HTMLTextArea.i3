INTERFACE HTMLTextArea;
IMPORT HTML;

TYPE
  T <: Public;

  Public = HTML.T OBJECT
  METHODS
    init(name, contents: TEXT := NIL; rows, cols := -1) : T;
  END;

END HTMLTextArea.
