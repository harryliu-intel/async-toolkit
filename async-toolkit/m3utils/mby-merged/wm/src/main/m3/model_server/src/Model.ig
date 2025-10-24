(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE Model(Map, MapAddr, ModelServer);
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT ModelServerSuper;

PROCEDURE HandlePacket(server :ModelServer.T;
                       READONLY read : Map.T;
                       READONLY update : MapAddr.Update;
                       READONLY hdr : FmModelMessageHdr.T;
                       pkt : Pkt.T);

PROCEDURE Setup(server : ModelServerSuper.T;
                READONLY read : Map.T;
                READONLY update : MapAddr.Update);

END Model.
