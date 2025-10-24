(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CheckMode;

TYPE T = { Setu, Hold, Puls };

CONST Brand = "CheckMode";

CONST
  Names = ARRAY T OF TEXT { "setu", "hold", "puls" };

END CheckMode.
