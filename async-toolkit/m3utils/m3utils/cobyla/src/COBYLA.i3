(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE INTERFACE COBYLA;

<*EXTERNAL cobyla_*>
PROCEDURE Call(N, M           : ADDRESS;
               X              : ADDRESS;
               RHOBEG, RHOEND : ADDRESS;
               IPRINT         : ADDRESS;
               MAXFUN         : ADDRESS;
               W              : ADDRESS;
               IACT           : ADDRESS;
               CALCFC         : Func;
               ITAG           : ADDRESS);

TYPE
  Func = PROCEDURE(N : ADDRESS; M : ADDRESS; X : ADDRESS; CON : ADDRESS; ITAG : ADDRESS) : LONGREAL;

END COBYLA.
