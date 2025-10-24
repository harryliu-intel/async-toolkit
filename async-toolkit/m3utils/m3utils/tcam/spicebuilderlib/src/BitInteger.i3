(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BitInteger;
IMPORT Bit;
IMPORT BigInt;

TYPE
  T    = BRANDED OBJECT END;

  SmallPromise <: PubSmallPromise;

  PubSmallPromise = T BRANDED OBJECT 
    v : INTEGER ;
  METHODS
    force(bits : CARDINAL) : Concrete;
  END;

  Concrete = T BRANDED OBJECT bits : REF ARRAY OF Bit.T END;

CONST Brand = "Integer";

PROCEDURE Small(z : INTEGER) : T;

PROCEDURE Big(z : BigInt.T; width : CARDINAL) : Concrete;

PROCEDURE Format(t : T; base : CARDINAL := 10) : TEXT;

PROCEDURE ToBigInt(t : T) : BigInt.T;

END BitInteger.
