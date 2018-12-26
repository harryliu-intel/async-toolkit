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
  f = FALSE; t = TRUE;
  
CONST
  Map = ARRAY OF T {
                       (* SW HW *)
  T { "R"      , Access { R , R  }, f, f },
  T { "RW/P"   , Access { RW, RW }, f, f },
  T { "RO/C"   , Access { R , RW }, f, t },
  T { "RO/C/V" , Access { R , RW }, f, t }, (* HLP only *)
  T { "RW/1S/V", Access { RW, RW }, f, f },
  T { "RW/V"   , Access { RW, RW }, f, f },
  T { "RSV"    , Access { R , NA }, t, f },
  T { "RO/V"   , Access { R , RW }, f, f },
  T { "RW/1C/V", Access { RW, RW }, f, f },
  T { "RO"     , Access { R , R  }, f, f },
  T { "WO"     , Access { W , R  }, f, f }, (* HLP only *)
  T { "RW"     , Access { RW, R  }, f, f }
  };

END IntelAccessType.
