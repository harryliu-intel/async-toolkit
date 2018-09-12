MODULE RegModula3Naming;
IMPORT RegReg, RegRegfile, RegAddrmap, RegField;
IMPORT RegModula3;
IMPORT RegGenState, RegModula3GenState;
FROM RegModula3IntfNaming IMPORT MapIntfNameRW;
FROM RegModula3Constants IMPORT IdiomName;

PROCEDURE RegTypename(r : RegReg.T; gsP : RegGenState.T) : TEXT =
  VAR
    gs : RegModula3GenState.T := gsP;
  BEGIN
    RETURN r.path & r.nm & RegModula3.CompTypeSuffix[gs.th]
  END RegTypename;

PROCEDURE RegfileTypename(r : RegRegfile.T; gsP : RegGenState.T) : TEXT =
  VAR
    gs : RegModula3GenState.T := gsP;
  BEGIN
    RETURN r.path & r.nm & RegModula3.CompTypeSuffix[gs.th]
  END RegfileTypename;

PROCEDURE MapIntfName(a : RegAddrmap.T; gsP : RegGenState.T) : TEXT =
  VAR
    gs : RegModula3GenState.T := gsP;
  BEGIN
    RETURN MapIntfNameRW(a, gs.rw)
  END MapIntfName;

PROCEDURE MapTypename(a : RegAddrmap.T; state : RegGenState.T) : TEXT =
  BEGIN
    RETURN MapIntfName(a, state) & ".T"
  END MapTypename;

PROCEDURE FieldName(f : RegField.T; debug : BOOLEAN) : TEXT =
  BEGIN RETURN IdiomName(f.nm,debug) END FieldName;
  
BEGIN END RegModula3Naming.
