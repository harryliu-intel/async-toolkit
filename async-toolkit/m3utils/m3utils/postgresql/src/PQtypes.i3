(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE PQtypes;

(* workaround for m3tk bugs see bug 134 *)

TYPE
   signed_char        = [-16_7f-1 .. 16_7f];
   short_int          = [-16_7fff-1 .. 16_7fff];
   int                = [-16_7fffffff-1 .. 16_7fffffff];
   char  = signed_char;
  int_star                   = UNTRACED REF int;
  short                      = short_int;
  char_star                  = UNTRACED REF char;


END PQtypes.

