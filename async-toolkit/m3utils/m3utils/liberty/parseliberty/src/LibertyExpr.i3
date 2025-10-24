(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyExpr;
IMPORT LibertyComponent;
IMPORT LibertyNumber;

TYPE
  T <: LibertyComponent.T;

  Op = { Plus, Minus, Times, Div, Uminus, Uplus, Uinverse, Ucompl };
  
  Binary = T OBJECT
    op : Op;
    a, b : T;
  END;

  Unary = T OBJECT
    op : Op;
    a : T;
  END;

  IntLiteral = T OBJECT
    val : INTEGER;
  END;

  FloatLiteral = T OBJECT
    val : LONGREAL;
  END;

  Const = T OBJECT
    val : TEXT;
  END;

CONST OpSym = ARRAY Op OF CHAR { '+', '-', '*', '/', '-', '+', '!', '~' };

PROCEDURE Plus(a, b : T) : T;
PROCEDURE Minus(a, b : T) : T;
PROCEDURE Times(a, b : T) : T;
PROCEDURE Div(a, b : T) : T;
PROCEDURE Uminus(a : T) : T;
PROCEDURE Uplus(a : T) : T;
PROCEDURE Uinverse(a : T) : T;
PROCEDURE Ucompl(a : T) : T;
PROCEDURE Num(n : LibertyNumber.T) : T;
PROCEDURE Ident(n : TEXT) : T;

CONST Brand = "LibertyExpr";

END LibertyExpr.
