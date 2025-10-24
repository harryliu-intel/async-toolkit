(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GenViewsScheme;
IMPORT GenViewsPass2;
IMPORT GenViews;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = GenViewsPass2.T OBJECT
    scmFiles : REF ARRAY OF Pathname.T;
  END;

END GenViewsScheme.
