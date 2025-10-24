(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TreeType;
IMPORT RegComponent;
IMPORT Word;
IMPORT RefSeq;

TYPE
  T = OBJECT
    tag      : TEXT;
    sz       : CARDINAL;
    comp     : RegComponent.T;
    offset   : CARDINAL; (* offset from parent -- only valid for instance *)
    address  : CARDINAL; (* global field address of first instance *)
    addrBits : Word.T;   (* global bit address of first instance *)
    up       : T;
   END;

  Array = T OBJECT
    n          : CARDINAL;
    elem       : T;
    stride     : CARDINAL;
    strideBits : Word.T;
  END;

  Struct <: T; (* see TreeTypeClass for more details *)

  Field = T BRANDED OBJECT END;

PROCEDURE Format(type : T) : TEXT;

PROCEDURE ComputeAddresses(tree : T; base : CARDINAL; ac : AddressConverter);
  
CONST Brand = "TreeType";

TYPE
  AddressConverter = OBJECT METHODS
    field2bit(field : CARDINAL) : Word.T;
  END;

PROCEDURE To(c : RegComponent.T) : T;
  (* initial builder *)

END TreeType.
