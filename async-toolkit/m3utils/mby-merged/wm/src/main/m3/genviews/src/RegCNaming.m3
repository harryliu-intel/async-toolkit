MODULE RegCNaming;
IMPORT RegReg, RegRegfile, RegAddrmap, RegField;
IMPORT RegGenState;
IMPORT RegCGenState;
FROM RegCConstants IMPORT IdiomName;

PROCEDURE RegTypename(r : RegReg.T; gsP : RegGenState.T) : TEXT =
  <*UNUSED*>VAR
    gs : RegCGenState.T := gsP;
  BEGIN
    RETURN r.path & r.nm 
  END RegTypename;

PROCEDURE RegfileTypename(r : RegRegfile.T; gsP : RegGenState.T) : TEXT =
  <*UNUSED*>VAR
    gs : RegCGenState.T := gsP;
  BEGIN
    RETURN r.path & r.nm 
  END RegfileTypename;

PROCEDURE MapIntfName(a : RegAddrmap.T; gsP : RegGenState.T) : TEXT =
  <*UNUSED*>VAR
    gs : RegCGenState.T := gsP;
  BEGIN
    RETURN a.nm
  END MapIntfName;

PROCEDURE MapTypename(a : RegAddrmap.T; state : RegGenState.T) : TEXT =
  BEGIN
    RETURN MapIntfName(a, state) 
  END MapTypename;

PROCEDURE FieldName(f : RegField.T; debug : BOOLEAN) : TEXT =
  BEGIN RETURN IdiomName(f.nm,debug) END FieldName;

BEGIN END RegCNaming.
