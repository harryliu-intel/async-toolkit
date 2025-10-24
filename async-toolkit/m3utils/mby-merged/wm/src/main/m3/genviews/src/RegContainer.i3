(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegContainer;
IMPORT RegComponent;
IMPORT RegChildSeq;

CONST
  Unspecified = LAST(CARDINAL);

TYPE
  T <: Public;

  Public = RegComponent.T OBJECT
    children : RegChildSeq.T;
  METHODS
    skipArc() : BOOLEAN; (* skip my name, because I'm a nested array *)
  END;

CONST Brand = "RegContainer";

END RegContainer.
