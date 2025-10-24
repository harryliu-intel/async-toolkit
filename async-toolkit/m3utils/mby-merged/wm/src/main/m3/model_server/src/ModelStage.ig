(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE ModelStage(TopAddr, Model);
IMPORT BaseModelStage;
IMPORT ServerPacket AS Pkt;

TYPE
  Super = BaseModelStage.T;
  
  T <: Public;

  Public = Super OBJECT
    h        : TopAddr.H;
    in, out  : Pkt.T;
    complete : BOOLEAN;
  METHODS
    (* called from outside the coroutine *)
    init (h : TopAddr.H; indices : Model.Indices; prev : Super) : T;
  END;

CONST Brand = "ModelStage(" & TopAddr.Brand & ")";

END ModelStage.
