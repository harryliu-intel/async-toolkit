(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MbyParserInfo;
FROM MbyParserTypes IMPORT L2Len, L3Len, MplsLen, TunLen;

TYPE
  T = RECORD
    otrL      : L;
    otrMpls   : Mpls;
    otrTunLen : TunLen;
    inrL      : L;
    inrMpls   : Mpls;
  END;

  Mpls = RECORD
    len : MplsLen;
  END;

  L = RECORD
    len : L2Len;
    vlan1, vlan2, v2first : BOOLEAN;
    l3Len : L3Len;
    l3V6 : BOOLEAN;
    l4Udp, l4Tcp : BOOLEAN;
  END;
  
CONST Brand = "MbyParserInfo";

END MbyParserInfo.
