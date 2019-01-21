INTERFACE RegCGenState;
IMPORT RegGenState;
IMPORT Wx;
IMPORT RegAddrmap, TextRefTbl, TextTopoSort;

TYPE
  Section = { Maintype, Scheme };

  Wxs = ARRAY Section OF Wx.T;

TYPE Phase = [0..1];

(* Here in the C version, we do things a little differently.
   We can't statically know the required order of decls, since we have
   to topo-sort the decls when emitting them.

   so all the C code goes into a single wx.

   the Scheme wx is used for auxiliary Scheme code. *)
     
TYPE
  T <: Public;

  Public = RegGenState.T OBJECT
    wx       : Wxs;
    curSym   : TEXT := NIL;
    curWx    : Wx.T := NIL;
    wxTbl    : TextRefTbl.T;
    topo     : TextTopoSort.T;
    phase    : Phase;

    map      : RegAddrmap.T;
    (* do we really need this? could refer to T instead *)
  METHODS
    put(sec : Section; txt : TEXT);

    p(sec : Section; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL);
    main(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL);
    scm (fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL);
    noteDep(to : TEXT);
  END;

CONST Brand = "RegCGenState";

END RegCGenState.
 

