(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE AddrVisitor;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    internal := DefaultInternal;
  END;

  Internal = BRANDED Brand & " Internal" OBJECT END;

PROCEDURE DefaultInternal(<*UNUSED*>t : T;
                      name, typeName : TEXT;
                      type : Type;
                      array : Array;
                      parent : Internal) : Internal =
  BEGIN
    RETURN NEW(DefInternal,
               name := name, typeName := typeName,
               type := type,
               array := array,
               parent := parent
               )
  END DefaultInternal;

BEGIN END AddrVisitor.
