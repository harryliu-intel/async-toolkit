(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TriData;
IMPORT Rd, DataPointSeq, Json;
IMPORT TriConfigSeq;

EXCEPTION SyntaxError;
          
PROCEDURE LoadJson(rd : Rd.T) : DataPointSeq.T RAISES { SyntaxError, Json.E };

PROCEDURE TraverseData(seq : DataPointSeq.T; corner : TEXT) : TriConfigSeq.T;



END TriData.
