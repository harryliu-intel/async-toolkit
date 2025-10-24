(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ProbeMode;

TYPE T = {  Assertions ,  Outputs ,  IO ,  All  };
     

CONST Names = ARRAY T OF TEXT 
         { "assertions", "outputs", "io", "all" };

CONST Brand = "ProbeMode";

END ProbeMode.
