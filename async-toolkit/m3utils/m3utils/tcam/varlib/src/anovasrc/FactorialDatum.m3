(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: FactorialDatum.m3,v 1.2 2005/04/05 09:09:52 mika Exp $ *)

MODULE FactorialDatum;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a.vi^ = b.vi^ AND a.r^ = b.r^ END Equal;

BEGIN END FactorialDatum.
