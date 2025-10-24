(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE UdpDatagram;

PROCEDURE Copy(READONLY a : T) : T =
  VAR
    res := a;
  BEGIN
    WITH arr = NEW(REF ARRAY OF CHAR, a.len) DO
      arr^ := SUBARRAY(a.bytes^, 0, a.len);
      res.bytes := arr
    END;
    RETURN res
  END Copy;

BEGIN END UdpDatagram.
