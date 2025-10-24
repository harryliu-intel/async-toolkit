(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TechMeasure;
IMPORT Pathname;
IMPORT TechConfig;

PROCEDURE DoMeasure(READONLY c : TechConfig.T;
                    traceRoot, outName, workDir : Pathname.T;
                    exitOnError := TRUE) : BOOLEAN;
  (* returns TRUE iff we measure a cycle time *)

END TechMeasure.
