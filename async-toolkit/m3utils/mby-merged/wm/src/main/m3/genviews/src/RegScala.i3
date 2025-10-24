(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegScala;
IMPORT RegCompiler;
IMPORT GenViews;

CONST Brand = "RegScala";

TYPE Public = RegCompiler.T;
     T      <: Public;
     Gen    = GenViews.T;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };
  
END RegScala.
