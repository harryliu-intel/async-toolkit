(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SyntaxTypeSystem;
IMPORT SyntaxType;

(*
   The types here match the types present in a BNF grammar using the foll.
   notation:

   {X}    List(X)
   X,{X}  List(X, emptyOk := FALSE)
   X v Y  X or Y
   [X]    X or NIL
   "s"    string "s"
   ID     identifier ID
   X,Y    sequence

   The parse grammar will result in a type representing a parse tree.

   Disjunctions will be, generally, at the top.

   Therefore, disjuctions are not permitted in [] or ,

   Disjunctions ARE permitted in List.

   v is associative and commutative
   , is associative but not commutative
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    
    makeTerminal(term : TEXT) : SyntaxType.T;
    makeString(string : TEXT) : SyntaxType.T;
    makeOptional(of : SyntaxType.T) : SyntaxType.T;
    makeList(of : SyntaxType.T; emptyOk : BOOLEAN) : SyntaxType.T;
    makeDisjunction(a, b : SyntaxType.T) : SyntaxType.T;
    makeSequence(a, b : SyntaxType.T) : SyntaxType.T;

    emptyDisjunction() : SyntaxType.T;
    emptySequence() : SyntaxType.T;
  END;
  
CONST Brand = "SyntaxTypeSystem";

END SyntaxTypeSystem.

