(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE HTMLList;
IMPORT HTML;

TYPE
  T <: Public;

  Public = HTML.T OBJECT
  METHODS
    add(stuff : HTML.Stuff);
  END;

(* make a list out of Stuff ... *)
PROCEDURE Hack(
    a1, a2, a3, a4, a5, a6, a7, a8, a9, a10 : HTML.Stuff := NIL) : T;


END HTMLList.
