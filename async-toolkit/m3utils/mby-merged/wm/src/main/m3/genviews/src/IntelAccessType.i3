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
    deprecated      : BOOLEAN;
  END;

  Access = RegFieldAccess.T;

CONST
  Brand   = "IntelAccessType";
  UDPName = "AccessType";

CONST
  N = FALSE; Y = TRUE;

CONST
  Map = ARRAY OF T {
                       (* SW HW *)
  T { "R"      , Access { R , R  }, N , N , Y  }, (* mistake -- to be removed *)
  T { "RW/P"   , Access { RW, RW }, N , N , N  }, (* should this be { R, RW }? *)
  T { "RO/C"   , Access { R , RW }, N , Y,  N  },
  T { "RO/C/V" , Access { R , RW }, N , Y,  Y  }, (* HLP only *)
  T { "RW/1S/V", Access { RW, RW }, N , N , N  },
  T { "RW/V"   , Access { RW, RW }, N , N , N  },
  T { "RSV"    , Access { R , NA }, Y,  N , N  },
  T { "RO/V"   , Access { R , RW }, N , N , N  },
  T { "RW/1C/V", Access { RW, RW }, N , N , N  },
  T { "RO"     , Access { R , R  }, N , N , N  },
  T { "WO"     , Access {  W, R  }, N , N , N  }, (* HLP only *)
  T { "RW"     , Access { RW, R  }, N , N , N  }
  };

END IntelAccessType.
