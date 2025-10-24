(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegC;
IMPORT RegCompiler;
IMPORT GenViews;
IMPORT RegComponent, RegGenState;
IMPORT GenCUtils;

CONST Brand = "RegC";

TYPE Public = RegCompiler.T;
     T      <: Public;
     Gen    = GenViews.T;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };

CONST ComponentTypeName = GenCUtils.ComponentTypeName;
  
END RegC.
