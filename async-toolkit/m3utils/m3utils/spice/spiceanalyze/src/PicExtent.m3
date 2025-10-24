(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE PicExtent;
IMPORT PicPoint;
FROM Fmt IMPORT F;

PROCEDURE Merge(READONLY a, b : T) : T =
  VAR
    ll := PicPoint.T { MIN(a.ll.x, b.ll.x), MIN(a.ll.y, b.ll.y) };
    ur := PicPoint.T { MAX(a.ur.x, b.ur.x), MAX(a.ur.y, b.ur.y) };

  BEGIN
    RETURN T { ll, ur }
  END Merge;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("%s { ll=%s ur=%s }",
             Brand,
             PicPoint.Format(a.ll),
             PicPoint.Format(a.ur))
  END Format;
  
BEGIN END PicExtent.
