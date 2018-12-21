INTERFACE RegCGenState;
IMPORT RegGenState;
IMPORT Wx;
IMPORT RegAddrmap, TextRefTbl, TextTopoSort;

TYPE
  Section = { Import, Components, Maintype, Trailer };

  Wxs = ARRAY Section OF Wx.T;

TYPE Phase = [0..1];

TYPE
  T <: Public;

  Public = RegGenState.T OBJECT
    wx       : Wxs;
    map : RegAddrmap.T; (* do we really need this? could refer to T instead *)
    curSym : TEXT := NIL;
    curWx : Wx.T := NIL;
    wxTbl : TextRefTbl.T;
    topo : TextTopoSort.T;
    phase : Phase;
  METHODS
    put(sec : Section; txt : TEXT);

    p(sec : Section; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL);
    main(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL);
    noteDep(to : TEXT);
  END;

CONST Brand = "RegCGenState";

END RegCGenState.
 

