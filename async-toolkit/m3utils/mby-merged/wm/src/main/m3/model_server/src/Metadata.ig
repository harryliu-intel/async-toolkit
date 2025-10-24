(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE Metadata(Record);
IMPORT Metadata;

TYPE
  T = Metadata.T OBJECT
    m : Record.T;
  END;

CONST Brand = "Metadata(" & Record.Brand & ")";

END Metadata.
  
