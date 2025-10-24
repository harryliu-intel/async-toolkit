(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LongNames;
IMPORT InstanceName;

TYPE T <: ROOT; (* totally opaque *)
     
PROCEDURE New() : T;
  
PROCEDURE Encode(longNames     : T;
                 READONLY name : ARRAY OF CHAR;
                 VAR      tgt  : InstanceName.T);
  
PROCEDURE Decode(longNames     : T;
                 READONLY inst : InstanceName.T;
                 VAR buffer : ARRAY OF CHAR  (* zero terminated *)
  );

PROCEDURE DecodeToText(longNames     : T;
                       READONLY inst : InstanceName.T) : TEXT;

CONST Brand = "LongNames";
      
END LongNames.
