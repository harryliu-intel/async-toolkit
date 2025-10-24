(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE VnfDecl;
IMPORT VnfBundle;
IMPORT VnfInstance;

TYPE
  T = ROOT;

  Bundle = T OBJECT
    b : VnfBundle.T;
  END;

  Instance = T OBJECT
    i : VnfInstance.T;
  END;

CONST Brand = "VnfDecl";

END VnfDecl.

     
