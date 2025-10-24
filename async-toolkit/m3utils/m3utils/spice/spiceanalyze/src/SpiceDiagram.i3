(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpiceDiagram;
IMPORT Canvas, SpiceGate;
TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    addGate(gate : SpiceGate.T);
    render(to : Canvas.T);
  END;

CONST Brand = "SpiceDiagram";

END SpiceDiagram.
