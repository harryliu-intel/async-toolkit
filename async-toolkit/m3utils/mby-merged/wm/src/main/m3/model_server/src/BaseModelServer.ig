(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE BaseModelServer(ModelServer, Map, MapAddr);

TYPE
  T <: Public;

  Super = ModelServer.T;
  
  Public = Super OBJECT
    h        : MapAddr.H;
  METHODS
    setup(READONLY read : Map.T; READONLY update : MapAddr.Update);
  END;

CONST Brand = Map.Brand & "/ " & ModelServer.Brand;

END BaseModelServer.
