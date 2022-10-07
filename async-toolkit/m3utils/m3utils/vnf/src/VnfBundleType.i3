INTERFACE VnfBundleType;

TYPE
  T = RECORD
    lo := LAST(INTEGER);
    hi := FIRST(INTEGER);
  END;

  (* default values means the type is denoting a scalar *)

CONST Brand = "VnfBundleType";

CONST Empty = T { LAST(INTEGER), FIRST(INTEGER) };

END VnfBundleType.
