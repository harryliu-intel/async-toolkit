(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TechSimulate;

IMPORT TechConfig;

TYPE Config = TechConfig.T;

PROCEDURE DoSimulate(READONLY c : Config);

VAR ProcDeadline : LONGREAL;
    
END TechSimulate.
