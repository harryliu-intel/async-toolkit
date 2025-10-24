(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RDP ;
IMPORT Node ;
IMPORT Rd ;

PROCEDURE Parse( text_stream : Rd.T ) : BOOLEAN (* Node.T *) ;

END RDP .
