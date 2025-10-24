(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE VnfInstance;
IMPORT Vnf;
IMPORT Refany;

TYPE
  T = Vnf.Instance;
  
REVEAL
  Vnf.Instance = BRANDED OBJECT
    type     : Vnf.Module;
    name     : TEXT;
    bindings : REF ARRAY OF TEXT;
    parent   : Vnf.Module;
  END;

CONST Brand = "VnfInstance";

CONST Equal = Refany.Equal;

END VnfInstance.
