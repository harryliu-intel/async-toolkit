(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SimDumperData;
IMPORT Wr;
IMPORT NodeRecSeq;
IMPORT Sim;
IMPORT SimParams;

PROCEDURE DumpData(wr         : Wr.T; 
                   srcs       : NodeRecSeq.T; 
                   READONLY d : ARRAY OF ARRAY OF LONGREAL;
                   sim        : Sim.T;
                   sp         : SimParams.T);

END SimDumperData.
