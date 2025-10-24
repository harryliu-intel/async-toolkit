(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE INTERFACE TransferC;

<*EXTERNAL Transfer__d2c*>
PROCEDURE d2c(d : LONGREAL; c : ADDRESS);

<*EXTERNAL Transfer__c2d*>
PROCEDURE c2d(c : ADDRESS) : LONGREAL;

END TransferC.
