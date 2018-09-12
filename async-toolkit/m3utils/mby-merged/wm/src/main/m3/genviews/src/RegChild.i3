INTERFACE RegChild;
IMPORT RegComponent, RdlArray, RdlNum;

TYPE
  T = OBJECT
    comp   : RegComponent.T;
    nm     : TEXT; (* instance name *)
    array  : RdlArray.Single; (* Range not allowed *)
    at     : RdlNum.T := Unspecified;
    stride : RdlNum.T := Unspecified;
    mod    : RdlNum.T := Unspecified;
  END;

CONST Unspecified : RdlNum.T = NIL;
      
CONST Brand = "RegChild";

END RegChild.
