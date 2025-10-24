(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE VnfBundleType;

TYPE
  T = RECORD
    lo := LAST(INTEGER);
    hi := FIRST(INTEGER);
  END;

  (* default values means the type is denoting a scalar *)

CONST Brand = "VnfBundleType";

CONST Empty = T { LAST(INTEGER), FIRST(INTEGER) };

END VnfBundleType.
