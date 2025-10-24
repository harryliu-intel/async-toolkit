(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PgField;

TYPE T = { Name, Base, Length, Group };

CONST Names = ARRAY T OF TEXT { "name", "base", "length", "policy_group" };
      
CONST Brand = "PgField";

END PgField.
