(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Nodes;
IMPORT Dims;
IMPORT Refany;

(* data structure for the abstract name *)

TYPE
  T <: Public;

  Public = OBJECT   
    nm   : TEXT;        (* array name *)
    sNm  : TEXT;        (* nm without the DUT prefix *)
    dims : REF Dims.T;  (* MAX dimensions *)
    intf : Intf := NIL;
  METHODS
    init(dutName, sNm : TEXT; READONLY dims : Dims.T; intf : Intf) : T;
  END;

  Intf <: PublicIntf;

  PublicIntf = OBJECT
    nodes : T;
  METHODS
    complete(nodes : T);
  END;

CONST Equal = Refany.Equal;

CONST Brand = "Nodes";

END Nodes.
