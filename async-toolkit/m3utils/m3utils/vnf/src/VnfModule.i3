(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE VnfModule;
IMPORT Vnf;
IMPORT VnfInstanceList;
IMPORT VnfBundleSeq;
IMPORT Refany;

TYPE
  T = Vnf.Module;
  
REVEAL
  Vnf.Module = BRANDED OBJECT
    nm        : TEXT;
    params    : VnfBundleSeq.T;
    wires     : VnfBundleSeq.T;
    instances : VnfInstanceList.T;
  END;

CONST Equal = Refany.Equal;

CONST Brand = "VnfModule";

END VnfModule.
