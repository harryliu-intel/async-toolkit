(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Cloudbreak;
FROM SvsTypes IMPORT CornerData;
IMPORT Power;

PROCEDURE SetProgram(VAR params                  : Power.Params;
                     VAR Trunc                   : LONGREAL);

CONST Brand = "Cloudbreak";

END Cloudbreak.
