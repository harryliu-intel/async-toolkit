(* this interface would be nice -- one day *)

INTERFACE XMLParseStream;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    parse();

    (* the following are abstracts, all must be overridden *)
    start(el : TEXT) : Disp;
    attr(tag, attr : TEXT) : Disp;
    end();
    charData(READONLY data : ARRAY OF CHAR) : Disp;
  END;

  Disp = { Continue,  (* keep parsing *)
           Abort,     (* dont parse me *)
           Pop        (* stop parsing my parent *)
  };

  FileStream <: T OBJECT METHODS
    init(pn : Pathname.T) : FileStream;
  END;
  
CONST Brand = "XMLParseStream";

END XMLParseStream.
