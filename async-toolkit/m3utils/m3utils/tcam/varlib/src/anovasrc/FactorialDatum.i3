(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: FactorialDatum.i3,v 1.3 2006/03/06 02:29:03 mika Exp $ *)

INTERFACE FactorialDatum;

TYPE
  T = RECORD
    vi : REF ARRAY OF CARDINAL;
    r :  REF ARRAY OF LONGREAL;
    src : TEXT
  END;

CONST Brand = "FactorialDatum";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END FactorialDatum.
