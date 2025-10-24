(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RegModula3IntfNaming;
FROM RegModula3 IMPORT RW, RWsuffixes;
IMPORT RegAddrmap;

PROCEDURE MapIntfNameRW(a : RegAddrmap.T; rw : RW) : TEXT =
  BEGIN
    RETURN a.nm & RWsuffixes[rw]
  END MapIntfNameRW;

BEGIN END RegModula3IntfNaming.
