MODULE Nodes;
IMPORT Dims;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init := DefComplete;
  END;

PROCEDURE DefComplete(nodes         : T; 
                      dutName       : TEXT;
                      sNm            : TEXT; 
                      READONLY dims : Dims.T; 
                      intf          : Intf) : T =
  VAR
    dd := NEW(REF Dims.T, NUMBER(dims));
  BEGIN
    dd^        := dims;
    nodes.nm   := dutName & "." & sNm; 
    nodes.sNm  := sNm;
    nodes.dims := dd;
    nodes.intf := intf;
    RETURN nodes
  END DefComplete;

REVEAL
  Intf = PublicIntf BRANDED OBJECT
  OVERRIDES
    complete := DefCompleteIntf;
  END;

PROCEDURE DefCompleteIntf(intf : Intf; nodes : T) =
  BEGIN intf.nodes := nodes END DefCompleteIntf;

BEGIN END Nodes.
