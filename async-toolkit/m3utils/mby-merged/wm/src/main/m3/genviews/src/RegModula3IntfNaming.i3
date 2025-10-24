(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegModula3IntfNaming;
IMPORT RegAddrmap;
FROM RegModula3 IMPORT RW;

PROCEDURE MapIntfNameRW(a : RegAddrmap.T; rw : RW) : TEXT;

END RegModula3IntfNaming.
