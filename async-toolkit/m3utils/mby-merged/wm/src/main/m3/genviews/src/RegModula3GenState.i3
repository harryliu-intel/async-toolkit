INTERFACE RegModula3GenState;
IMPORT RegGenState;
IMPORT Wx, TextSet, Pathname;
FROM RegModula3 IMPORT RW, TypeHier;

TYPE
  Section = { IImport, IComponents, IMaintype, ITrailer,
              MImport, MDecl, MCode, MTrailer };

  Wxs = ARRAY Section OF Wx.T;

TYPE
  T = RegGenState.T OBJECT
    wx       : Wxs;
    dumpSyms : TextSet.T;
    rw       : RW;          (* which pass/file are we working on *)
    dirPath  : Pathname.T;
    th       : TypeHier;    (* which type hierarchy are we working on *)

    i3imports : TextSet.T;
    m3imports : TextSet.T;
  METHODS
    put(sec : Section; txt : TEXT);
  END;

CONST Brand = "RegModula3GenState";

END RegModula3GenState.
 

