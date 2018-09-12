INTERFACE RegScalaGenState;
IMPORT RegGenState;
IMPORT Wx;

TYPE
  Section = { Import, Components, Maintype, Trailer };

  Wxs = ARRAY Section OF Wx.T;

TYPE
  T = RegGenState.T OBJECT
    wx       : Wxs;
  METHODS
    put(sec : Section; txt : TEXT);
  END;

CONST Brand = "RegScalaGenState";

END RegScalaGenState.
 

