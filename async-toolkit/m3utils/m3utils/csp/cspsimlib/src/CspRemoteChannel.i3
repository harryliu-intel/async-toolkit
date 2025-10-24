(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspRemoteChannel;

(* identify a remote channel assocation *)

TYPE
  T = OBJECT
    nm   : TEXT;       (* globally valid name of channel *)
    id   : CARDINAL;   (* globally valid id of channel *)
    wrnm : TEXT;       (* globally valid name of writer of channel *)
    wrid : CARDINAL;   (* globally valid pid of writer of channel *)
    wrs  : CARDINAL;   (* global scheduler id of writer of channel *)
    rdnm : TEXT;       (* globally valid name of reader of channel *)
    rdid : CARDINAL;   (* globally valid pid of reader of channel *)
    rds  : CARDINAL;   (* global scheduler id of reader of channel *)
  END;

CONST Brand = "CspRemoteChannel";

END CspRemoteChannel.

