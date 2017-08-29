(* $Id$ *)

INTERFACE XMLParse;
IMPORT Pathname;
IMPORT TextList;

EXCEPTION 
  NotFound; 
  Multiple;

(* 
   parse an XML document using DoIt.  Returns an object of type t.
   At each level of the object, there is a name ("el" in expat
   talk), children of the same type (may be iterated through), plus
   a number of attributes, which are pairs of TEXT (may also be
   iterated through)

   Components may, apart from being iterated through, also be looked
   up by name (children) or tag (attributes).  No method for looking
   up attributes by attribute yet.  Also looking up an attribute
   by name only returns the first such attribute.  If there may be
   multiple attributes of the same tag, the whole set must be
   iterated through.
*)

TYPE
  Attr = RECORD tag, attr : TEXT END;

  T <: Public;

  Public = OBJECT METHODS
    getEl() : TEXT;
    getCharData() : TEXT; (* character data in body of these tags *)
    getAttr(named : TEXT) : Attr RAISES { NotFound, Multiple };
    getChild(named : TEXT) : T RAISES { NotFound, Multiple };

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

PROCEDURE DoIt(p : Pathname.T; avoidTags : TextList.T := NIL) : T;

PROCEDURE DoText(t : TEXT) : T;

CONST Brand = "XML Parse";

END XMLParse.
