(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfTypeName;

CONST NotAFixedLength = -1;
      
PROCEDURE GetByteLength(nm : TEXT) : [ NotAFixedLength..LAST(CARDINAL) ];

CONST Brand = "StdfTypeName";

END StdfTypeName.
