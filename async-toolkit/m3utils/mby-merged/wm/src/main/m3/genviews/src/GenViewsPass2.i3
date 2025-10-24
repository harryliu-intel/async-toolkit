(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GenViewsPass2;
IMPORT GenViews, Rd;

TYPE
  T = GenViews.T OBJECT
    fieldAddrRd      : Rd.T;
  END;

CONST Brand = "GenViewsPass2";

END GenViewsPass2.
