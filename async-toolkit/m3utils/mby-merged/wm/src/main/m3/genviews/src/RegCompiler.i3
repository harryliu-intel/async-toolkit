(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegCompiler;
IMPORT RegAddrmap, BigInt;

TYPE
  T <: Public;

  Public = OBJECT
    map  : RegAddrmap.T;
    addr : BigInt.T;
  METHODS
    init(map : RegAddrmap.T) : T;
  END;

CONST Brand = "RegCompiler";

END RegCompiler.
