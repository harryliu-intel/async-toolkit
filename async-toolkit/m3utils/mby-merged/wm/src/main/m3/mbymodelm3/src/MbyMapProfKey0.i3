(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MbyMapProfKey0;
IMPORT Byte;
FROM NetTypes IMPORT U64; (* U8? *)

TYPE
  T = RECORD
    ptrsErr    : BOOLEAN;
    ex         : Byte.T; (* check type *)
    csum       : Byte.T; (* check type *)
    ipFits     : Byte.T; (* check type *)
    ihlOk      : BOOLEAN;
    ihlFits    : BOOLEAN;
    flags      : U64;
  END;

CONST Brand = "MbyMapProfKey0";

END MbyMapProfKey0.
