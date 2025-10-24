(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspSimUtils;
IMPORT IP;

PROCEDURE ScanIp(str : TEXT) : IP.Address4;

PROCEDURE FmtIp(READONLY addr : IP.Address4) : TEXT;

END CspSimUtils.
