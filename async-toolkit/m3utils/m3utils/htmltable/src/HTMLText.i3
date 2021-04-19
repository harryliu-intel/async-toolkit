INTERFACE HTMLText;
IMPORT HTML;

TYPE
  T <: Public;

  Public = HTML.T OBJECT
  METHODS
    init(text : TEXT) : T;
  END;

END HTMLText.
