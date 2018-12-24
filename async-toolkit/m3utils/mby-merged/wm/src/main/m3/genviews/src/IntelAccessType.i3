INTERFACE IntelAccessType;
(* spec of Nebulon extensions of Intel regfield access *)
IMPORT RegFieldAccess;
FROM RegFieldAccess IMPORT NA, R, W, RW;

TYPE
  T = RECORD
    accessType : TEXT;
    rdlAccess  : RegFieldAccess.T;
    reserved   : BOOLEAN;
    (* add more interesting attributes here *)
  END;

  Access = RegFieldAccess.T;

CONST
  Brand = "RegFieldAccessIntel";

CONST
  NebulonAccessTypes = ARRAY OF T {
  T { "R"      , Access { R , R  }, FALSE },
  T { "RW/P"   , Access { RW, RW }, FALSE },
  T { "RO/C"   , Access { R , RW }, FALSE },
  T { "RW/1S/V", Access { RW, RW }, FALSE },
  T { "RW/V"   , Access { RW, RW }, FALSE },
  T { "RSV"    , Access { R , NA }, TRUE  },
  T { "RO/V"   , Access { R , RW }, FALSE },
  T { "RW/1C/V", Access { RW, RW }, FALSE },
  T { "RO"     , Access { R , R  }, FALSE },
  T { "RW"     , Access { RW, R  }, FALSE }
  };

END IntelAccessType.
