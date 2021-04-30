INTERFACE UnixGroupList;
IMPORT UnixGidSet;
IMPORT Utypes;

TYPE T = UnixGidSet.T;

PROCEDURE Get(user : TEXT; VAR uid : Utypes.uid_t) : T;

EXCEPTION NotFound;
  
PROCEDURE GetUid(user : TEXT) : Utypes.uid_t RAISES { NotFound } ;

PROCEDURE GetGids(user : TEXT) : REF ARRAY OF Utypes.gid_t RAISES { NotFound };
  
CONST Brand = "UnixGroupList";

END UnixGroupList.
