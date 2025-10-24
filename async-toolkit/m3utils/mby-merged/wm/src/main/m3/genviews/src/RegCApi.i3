(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegCApi;
IMPORT RegCompiler;
IMPORT GenViews;
IMPORT GenCUtils;
IMPORT RegC;

CONST Brand = "RegCApi";

TYPE Public = RegC.Public;
     T      <: Public;
     Gen    = RegC.Gen;

TYPE Phase = { P };

CONST
  PhaseNames = RegC.PhaseNames;
  
CONST ComponentTypeName = RegC.ComponentTypeName;

END RegCApi.
