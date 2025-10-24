(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE PGRES;

TYPE T = {
  EMPTY_QUERY,  (* a query command that doesn't return *)
  COMMAND_OK,   (* anything was executed properly by the backend *)
  TUPLES_OK,    (* a query command that returns tuples *)
                (* was executed properly by the backend, PGresult *)
                (* contains the result tuples *)
  COPY_OUT,
  COPY_IN,
  BAD_RESPONSE, (* an unexpected response was recv'd from the backend *)
  NONFATAL_ERROR,
  FATAL_ERROR};

TYPE PGRES_T = T;

END PGRES.
