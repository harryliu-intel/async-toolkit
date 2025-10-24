(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE VnfBundle;
IMPORT VnfBundleType;

TYPE
  T = RECORD
    nm    : TEXT;
    type  : VnfBundleType.T;
    usage : Usage;
  END;

  Usage = { Wire, Input, Output };

CONST Brand = "VnfBundle";

END VnfBundle.
