INTERFACE VnfInstance;
IMPORT Vnf;
IMPORT Refany;

TYPE
  T = Vnf.Instance;
  
REVEAL
  Vnf.Instance = BRANDED OBJECT
    type     : Vnf.Module;
    name     : TEXT;
    bindings : REF ARRAY OF TEXT;
    parent   : Vnf.Module;
  END;

CONST Brand = "VnfInstance";

CONST Equal = Refany.Equal;

END VnfInstance.
