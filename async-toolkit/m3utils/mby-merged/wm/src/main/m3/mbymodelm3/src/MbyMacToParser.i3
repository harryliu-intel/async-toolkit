(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MbyMacToParser;
IMPORT MbyTypes;

TYPE
  T = RECORD
    rxLength : MbyTypes.PacketLen;
    rxPort   : MbyTypes.Port;
  END;

CONST Brand = "MbyMacToParser";

END MbyMacToParser.
