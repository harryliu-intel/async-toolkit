(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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

  Acc = RegFieldAccess.T;

CONST
  Brand   = "IntelAccessType";
  UDPName = "AccessType";

CONST
  N = FALSE; Y = TRUE;

CONST
  Map = ARRAY OF T {
                       (* SW HW *)
  T { "R"      , Acc { R , R  }, N, N, Y  }, (* mistake, remove *)
  T { "RW/P"   , Acc { RW, RW }, N, N, N  }, 
  T { "RO/C"   , Acc { R , RW }, N, Y, Y  },
  T { "RO/C/V" , Acc { R , RW }, N, Y, Y  }, (* HLP only *)
  T { "RW/1S/V", Acc { RW, RW }, N, N, N  },
  T { "RW/V"   , Acc { RW, RW }, N, N, N  },
  T { "RSV"    , Acc { R , NA }, Y, N, N  },
  T { "RO/V"   , Acc { R , RW }, N, N, N  },
  T { "RW/1C/V", Acc { RW, RW }, N, N, N  },
  T { "RO"     , Acc { R , R  }, N, N, N  },
  T { "WO"     , Acc {  W, R  }, N, N, Y  }, (* HLP only *)
  T { "RW"     , Acc { RW, R  }, N, N, N  }
  };

END IntelAccessType.
