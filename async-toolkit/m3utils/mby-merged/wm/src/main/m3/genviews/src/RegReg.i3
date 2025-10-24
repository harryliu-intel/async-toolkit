(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegReg;
IMPORT RegFieldSeq;
IMPORT RegComponent;

CONST
  Unspecified = LAST(CARDINAL);

TYPE
  T = RegComponent.T OBJECT
    fields : RegFieldSeq.T;
  END;

CONST
  Brand = "RegReg";

END RegReg.
