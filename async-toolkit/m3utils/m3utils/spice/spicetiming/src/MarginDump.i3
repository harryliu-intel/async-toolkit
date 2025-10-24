(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MarginDump;

(* take a database of margins and dump it to disk in human-readable forms *)

IMPORT MarginMeasurementSeq;

PROCEDURE Do(seq     : MarginMeasurementSeq.T;
             Nworst  : CARDINAL;
             traceRt : TEXT) : MarginMeasurementSeq.T;
  (* the sequence that is returned is the Nworst of each type *)

END MarginDump.
