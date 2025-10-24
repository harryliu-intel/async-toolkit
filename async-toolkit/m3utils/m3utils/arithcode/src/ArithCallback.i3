(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ArithCallback;
IMPORT Wr;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    setErrorCb(ecb : T);
    newError(msg : TEXT);

    newByte(byte : CHAR); (* abstract *)
    newEof();             (* abstract *) 
  END;

  Writer <: PubWriter;

  PubWriter = T OBJECT METHODS
    init(wr : Wr.T) : Writer;
  END;

  
  
CONST Brand = "ArithCallback";

END ArithCallback.
