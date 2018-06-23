MODULE RegScalaNaming;
IMPORT RegReg, RegRegfile, RegAddrmap, RegField;
IMPORT RegGenState;
IMPORT RegScalaGenState;
FROM RegScalaConstants IMPORT IdiomName;

PROCEDURE RegTypename(r : RegReg.T; gsP : RegGenState.T) : TEXT =
  VAR
    gs : RegScalaGenState.T := gsP;
  BEGIN
    RETURN r.path & r.nm 
  END RegTypename;

PROCEDURE RegfileTypename(r : RegRegfile.T; gsP : RegGenState.T) : TEXT =
  VAR
    gs : RegScalaGenState.T := gsP;
  BEGIN
    RETURN r.path & r.nm 
  END RegfileTypename;

PROCEDURE MapIntfName(a : RegAddrmap.T; gsP : RegGenState.T) : TEXT =
  VAR
    gs : RegScalaGenState.T := gsP;
  BEGIN
    RETURN a.nm
  END MapIntfName;

PROCEDURE MapTypename(a : RegAddrmap.T; state : RegGenState.T) : TEXT =
  BEGIN
    RETURN MapIntfName(a, state) & ".T"
  END MapTypename;

PROCEDURE FieldName(f : RegField.T; debug : BOOLEAN) : TEXT =
  BEGIN RETURN IdiomName(f.nm,debug) END FieldName;

BEGIN END RegScalaNaming.
