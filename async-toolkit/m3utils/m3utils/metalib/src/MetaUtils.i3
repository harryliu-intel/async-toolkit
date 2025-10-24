INTERFACE MetaUtils;
IMPORT NameSet, SchemePair;
IMPORT Name, NameRefTbl, RefList;
IMPORT Scheme, Pathname;
IMPORT NameNameSetTbl;

PROCEDURE NRKeysToSet(tbl : NameRefTbl.T) : NameSet.T;

PROCEDURE MatchingRoots(set : NameSet.T; root : Name.T) : NameSet.T;

PROCEDURE SetToList(set : NameSet.T) : SchemePair.T;

PROCEDURE RefListToList(lst : RefList.T) : SchemePair.T;

PROCEDURE FileRdOpenOrFalse(pn : Pathname.T) : Scheme.Object;

PROCEDURE ListListToAliasTbl(p : SchemePair.T) : NameNameSetTbl.T;
  (* take a list of lists of aliases (Name.T) and convert to a
     table with a row for every alias, mapping to the set of names *)

END MetaUtils.
