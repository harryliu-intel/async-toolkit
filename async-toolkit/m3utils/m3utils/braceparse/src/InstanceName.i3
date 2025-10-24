(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE InstanceName;

TYPE
  T = ARRAY [0..7] OF CHAR;

CONST Null = T { FIRST(CHAR), .. };

CONST Brand = "InstanceName";

END InstanceName.
