(* this interface would be nice -- one day *)

INTERFACE XMLParseStream;

TYPE
  El = OBJECT METHODS
    charData(txt   : TEXT);
    attr    (named : TEXT);
    end     ()            : El; (* chance to rewrite obj *)
  END;
  
  T = OBJECT METHODS
    start   (parent : El; nm : TEXT) : El;
  END;

  FileStream <: T OBJECT METHODS
    init(pn : Pathname.T) : FileStream;
  END;

  TextStream <: T OBJECT METHODS
    init(str : TEXT) : TextStream;
  END;

CONST Brand = "XMLParseStream";

END XMLParseStream.
