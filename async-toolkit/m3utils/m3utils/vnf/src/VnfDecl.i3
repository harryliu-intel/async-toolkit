INTERFACE VnfDecl;
IMPORT VnfBundle;
IMPORT VnfInstance;

TYPE
  T = ROOT;

  Bundle = T OBJECT
    b : VnfBundle.T;
  END;

  Instance = T OBJECT
    i : VnfInstance.T;
  END;

CONST Brand = "VnfDecl";

END VnfDecl.

     
