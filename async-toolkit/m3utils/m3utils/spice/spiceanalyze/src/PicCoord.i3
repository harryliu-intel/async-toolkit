(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicCoord;
IMPORT LongReal AS Parent;
IMPORT Fmt;

TYPE T = Parent.T;

     NonNeg = T; (* no distinction for floating point *)
     
CONST Brand = "PicCoord(" & Parent.Brand & ")";

CONST Hash    = Parent.Hash;
      Equal   = Parent.Equal;
      Compare = Parent.Compare;

CONST Zero = 0.0d0;

CONST Format = Fmt.LongReal;

END PicCoord.
     
