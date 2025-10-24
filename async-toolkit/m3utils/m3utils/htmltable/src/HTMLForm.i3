(* $Id$ *)
INTERFACE HTMLForm;
IMPORT HTML,Request;

TYPE
  T <: Public;

  Public = HTML.T OBJECT
  METHODS
    add(stuff : HTML.Stuff);
    init(toURL : TEXT; request : Request.T) : T;
  END;

END HTMLForm.
