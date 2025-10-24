(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE HTMLOutput;
IMPORT HTMLPage, HTML;

PROCEDURE SetFooter(footer : HTML.Stuff);
PROCEDURE Ship(page : HTMLPage.T);

END HTMLOutput.
