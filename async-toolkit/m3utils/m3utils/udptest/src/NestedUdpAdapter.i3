(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NestedUdpAdapter;
IMPORT UdpAdapter;
IMPORT IP;

TYPE
  T = UdpAdapter.T OBJECT METHODS
        init(myPort: IP.Port;
         myAddr := IP.NullAddress;
         underlying : UdpAdapter.T := NIL) : T
      RAISES {IP.Error};
  END;

CONST Brand = "NestedUdpAdapter";

END NestedUdpAdapter.
