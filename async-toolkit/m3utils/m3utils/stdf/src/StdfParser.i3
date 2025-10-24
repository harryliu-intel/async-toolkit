(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfParser;
IMPORT Rd;
IMPORT StdfRecordObjectSeq;
IMPORT Thread;
IMPORT StdfE;

PROCEDURE Parse(rd : Rd.T) : StdfRecordObjectSeq.T
  RAISES { StdfE.E, Rd.Failure, Thread.Alerted };

CONST Brand = "StdfParser";
      
END StdfParser.
