INTERFACE IntelAccessType;
(* spec of Nebulon extensions of Intel regfield access *)
IMPORT RegFieldAccess;
FROM RegFieldAccess IMPORT NA, R, W, RW;

TYPE
  T = RECORD
    nm              : TEXT;
    rdlAccess       : RegFieldAccess.T;
    reserved        : BOOLEAN;
    clearOnRead     : BOOLEAN;
    (* add more interesting attributes here *)
  END;

  Access = RegFieldAccess.T;

CONST
  Brand   = "IntelAccessType";
  UDPName = "AccessType";

CONST
  No = FALSE; Yes = TRUE;

CONST
  Map = ARRAY OF T {
                       (* SW HW *)
  T { "R"      , Access { R , R  }, No , No  }, (* mistake -- to be removed *)
  T { "RW/P"   , Access { RW, RW }, No , No  }, (* should this be { R, RW }? *)
  T { "RO/C"   , Access { R , RW }, No , Yes },
  T { "RO/C/V" , Access { R , RW }, No , Yes }, (* HLP only *)
  T { "RW/1S/V", Access { RW, RW }, No , No  },
  T { "RW/V"   , Access { RW, RW }, No , No  },
  T { "RSV"    , Access { R , NA }, Yes, No  },
  T { "RO/V"   , Access { R , RW }, No , No  },
  T { "RW/1C/V", Access { RW, RW }, No , No  },
  T { "RO"     , Access { R , R  }, No , No  },
  T { "WO"     , Access {  W, R  }, No , No  }, (* HLP only *)
  T { "RW"     , Access { RW, R  }, No , No  }
  };

END IntelAccessType.
