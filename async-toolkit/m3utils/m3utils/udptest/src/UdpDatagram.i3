(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE UdpDatagram;
IMPORT UDP;

TYPE T = UDP.Datagram;

CONST Brand = "UdpDatagram";

PROCEDURE Copy(READONLY a : T) : T;
  (* perform a "deep copy" of a *)

END UdpDatagram.
