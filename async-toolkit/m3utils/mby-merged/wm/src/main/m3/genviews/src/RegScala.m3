MODULE RegScala EXPORTS RegScala, RegScalaGenerators;
IMPORT GenViewsScala;
IMPORT RegReg, RegGenState, RegRegfile, RegAddrmap;
IMPORT OSError, Thread, Wr;
IMPORT Pathname, RegScalaGenState;
FROM RegScalaGenState IMPORT Section;
IMPORT Wx;

(* stuff inherited from m3 *)
FROM RegModula3Utils IMPORT CopyWx;

REVEAL
  T = GenViewsScala.Compiler BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE GenReg(r : RegReg.T; genState : RegGenState.T) =
  BEGIN
  END GenReg;
  
PROCEDURE GenAddrmap(map     : RegAddrmap.T; gsF : RegGenState.T) 
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
  END GenAddrmap;
  
TYPE
  GenState = RegScalaGenState.T OBJECT
    map : RegAddrmap.T; (* do we really need this? could refer to T instead *)
  OVERRIDES
    put := PutGS;
  END;
  
PROCEDURE PutGS(gs : GenState; sec : Section; txt : TEXT) =
  BEGIN
    Wx.PutText(gs.wx[sec], txt)
  END PutGS;

PROCEDURE GenRegfile(rf       : RegRegfile.T;
                     genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  BEGIN
  END GenRegfile;

PROCEDURE Write(t : T; dirPath : Pathname.T; phase : Phase)
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs := NEW(GenState,
              dumpSyms   := NIL, (* same as M3 *)
              dirPath    := dirPath,
              map        := t.map);
    intfNm := t.map.intfName(gs);
    path := dirPath & "/" & intfNm & ".i3";
  BEGIN
    FOR i := FIRST(gs.wx) TO LAST(gs.wx) DO
      gs.wx[i] := Wx.New()
    END;

    t.map.generate(gs);

    (* do the actual output *)
    CopyWx(gs.wx, path)
  END Write;
  
BEGIN END RegScala.
