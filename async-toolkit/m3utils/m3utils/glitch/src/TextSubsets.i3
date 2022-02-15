INTERFACE TextSubsets;
IMPORT TextSet;

TYPE Elem = TEXT;
     Set  = TextSet.T;

PROCEDURE Iterate(s : Set) : Iterator;

TYPE
  Iterator <: PubIterator;

  PubIterator = OBJECT METHODS
    next(VAR ss : Set) : BOOLEAN;
  END;

CONST Brand = "TextSubsets";

END TextSubsets.
