(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE xmlParser;

BEGIN
  XML_STATUS_ERROR := xmlStatusError();
  XML_STATUS_OK := xmlStatusOK();
  XML_STATUS_SUSPENDED := xmlStatusSuspended();
END xmlParser.
