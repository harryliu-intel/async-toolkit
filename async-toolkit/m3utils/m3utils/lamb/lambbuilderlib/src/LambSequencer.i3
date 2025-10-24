(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LambSequencer;
IMPORT LambCommandSeq AS CommandSeq;
IMPORT BitInteger;
IMPORT LambVerb AS Verb;

PROCEDURE Compile(prog          : CommandSeq.T;
                  VAR      seq  : ARRAY Verb.T OF REF ARRAY OF BitInteger.T);

END LambSequencer.
