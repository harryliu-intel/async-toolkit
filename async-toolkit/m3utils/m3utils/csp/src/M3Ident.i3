(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE M3Ident;

EXCEPTION Error;

  (* escape _ *)
PROCEDURE Escape(str : TEXT) : TEXT;
PROCEDURE Unescape(str : TEXT) : TEXT RAISES { Error } ;
  
  (* don't escape _ *)
PROCEDURE EscapeU(str : TEXT) : TEXT;
(*PROCEDURE UnescapeU(str : TEXT) : TEXT RAISES { Error } ;
(* this is broken now *)
*)

END M3Ident.
