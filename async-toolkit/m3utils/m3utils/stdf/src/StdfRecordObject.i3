(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfRecordObject;
IMPORT StdfRecordHeader;

TYPE
  Public = BRANDED Brand OBJECT
    hdr : StdfRecordHeader.T;
  END;

  T <: Public;
  
CONST Brand = "StdfRecordObject";

END StdfRecordObject.
