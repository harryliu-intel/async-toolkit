(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegRegfile;
IMPORT RegContainer;

TYPE
  T = RegContainer.T BRANDED Brand OBJECT END;

CONST
  Brand = "RegRegfile";

END RegRegfile.
