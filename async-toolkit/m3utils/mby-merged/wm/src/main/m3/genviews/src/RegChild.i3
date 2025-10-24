(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegChild;
IMPORT RegComponent, RdlArray, RdlNum;

TYPE
  T = OBJECT
    comp   : RegComponent.T;
    nm     : TEXT; (* instance name *)
    array  : RdlArray.Single; (* Range not allowed *)
    at     : RdlNum.T := Unspecified;
    stride : RdlNum.T := Unspecified;
    mod    : RdlNum.T := Unspecified;
  END;

CONST Unspecified : RdlNum.T = NIL;
      
CONST Brand = "RegChild";

END RegChild.
