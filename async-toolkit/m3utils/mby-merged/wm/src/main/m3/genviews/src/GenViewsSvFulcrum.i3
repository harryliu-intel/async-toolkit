(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GenViewsSvFulcrum;
IMPORT GenViewsPass2;
IMPORT Word;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = GenViewsPass2.T OBJECT
    addrBits         : AddrBits;
    baseAddressBytes : Word.T;
    packageName      : TEXT;
    outFileName      : Pathname.T;
  END;

  AddrBits = [1..64];

CONST Brand = "GenViewsSvFulcrum";

END GenViewsSvFulcrum.
