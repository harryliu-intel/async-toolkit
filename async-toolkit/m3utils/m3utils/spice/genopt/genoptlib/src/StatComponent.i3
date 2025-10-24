(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StatComponent;

TYPE
  T = { Nom, Mu, Sigma };

CONST
  Names = ARRAY T OF TEXT { "Nom", "Mu", "Sigma" };
  
CONST Brand = "StatComponent";

END StatComponent.
