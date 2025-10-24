(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfRecordObjectClass;
IMPORT StdfRecordObject;
IMPORT StdfRecordTypes;

TYPE
  Class = StdfRecordObject.Public OBJECT
    tag : StdfRecordTypes.Enum;
  END;

REVEAL
  StdfRecordObject.T = Class BRANDED OBJECT END;
  
CONST Brand = "StdfRecordObjectClass";

END StdfRecordObjectClass.
