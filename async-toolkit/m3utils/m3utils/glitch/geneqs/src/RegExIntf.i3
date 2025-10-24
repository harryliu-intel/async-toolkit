(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegExIntf;
IMPORT RegEx;
IMPORT Refany;

TYPE T = RegEx.Pattern;

CONST Brand = "RegExIntf";

CONST Equal = Refany.Equal;
      
END RegExIntf.
