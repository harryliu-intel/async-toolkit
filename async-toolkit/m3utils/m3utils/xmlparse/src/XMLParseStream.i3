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

  (* current implementation quirk:

     returning Abort or Pop on parsing an item does not necessarily
     take effect immediately.  May need to handle following items that
     were unexpected.

     See long-winded comments in source code for more information. 
  *)

  FileStream <: T OBJECT METHODS
    init(pn : Pathname.T) : FileStream;
  END;

CONST DispNames = ARRAY Disp OF TEXT { "Continue", "Abort", "Pop" };
  
CONST Brand = "XMLParseStream";

END XMLParseStream.
