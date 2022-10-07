INTERFACE VnfModule;
IMPORT Vnf;
IMPORT VnfInstanceList;
IMPORT VnfBundleSeq;
IMPORT Refany;

TYPE
  T = Vnf.Module;
  
REVEAL
  Vnf.Module = BRANDED OBJECT
    nm        : TEXT;
    params    : VnfBundleSeq.T;
    wires     : VnfBundleSeq.T;
    instances : VnfInstanceList.T;
  END;

CONST Equal = Refany.Equal;

CONST Brand = "VnfModule";

END VnfModule.
