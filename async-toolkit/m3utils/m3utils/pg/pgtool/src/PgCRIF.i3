(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PgCRIF;
IMPORT PgField;
IMPORT Pathname;

TYPE Processor = PROCEDURE (b : ARRAY PgField.T OF TEXT; lenPerByte : CARDINAL);
     
PROCEDURE Parse(path : Pathname.T; processBuf : Processor);

END PgCRIF.
