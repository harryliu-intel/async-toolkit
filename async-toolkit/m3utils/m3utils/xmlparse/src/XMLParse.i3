(* $Id$ *)

INTERFACE XMLParse;
IMPORT Pathname;

EXCEPTION NotFound;

TYPE
  Attr = RECORD tag, attr : TEXT END;

  T <: Public;

  Public = OBJECT METHODS
    getEl() : TEXT;
    getAttr(named : TEXT) : Attr RAISES { NotFound };
    getChild(named : TEXT) : T RAISES { NotFound };

    iterateAttrs() : AttrIterator;
    iterateChildren() : ChildIterator;

    nAttrs() : CARDINAL;
    nChildren() : CARDINAL;
  END;

  AttrIterator <: PubAttrIterator;
  ChildIterator <: PubChildIterator;

  PubAttrIterator = OBJECT METHODS
    next(VAR attr : Attr) : BOOLEAN;
  END;

  PubChildIterator = OBJECT METHODS
    next(VAR child : T) : BOOLEAN;
  END;

PROCEDURE DoIt(p : Pathname.T) : T;

CONST Brand = "XML Parse";

END XMLParse.
