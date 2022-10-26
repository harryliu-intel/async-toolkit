INTERFACE VnfBundle;
IMPORT VnfBundleType;

TYPE
  T = RECORD
    nm    : TEXT;
    type  : VnfBundleType.T;
    usage : Usage;
  END;

  Usage = { Wire, Input, Output };

CONST Brand = "VnfBundle";

END VnfBundle.
