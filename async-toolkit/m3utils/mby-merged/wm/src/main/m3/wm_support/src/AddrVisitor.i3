(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE AddrVisitor;
IMPORT CompRange;

(* we expect a generated _addr file to include code to launch a visitor 

   the visitor is expected to perform pre-order traversal of the data
   struct 

*)

TYPE
  T <: Public;

  Type = { Reg, Addrmap, Regfile };

  Array = OBJECT idx, sz : CARDINAL END;
  (* rep exposure warning:
     for performance reasons, Array is not re-allocated for each
     array index.  It is shared for each array that appears textually
     in the RDL.  So if the index needs to be held persistently somewhere,
     it must be reallocated in the callee method. *)

  Public = OBJECT METHODS
    internal(name, typeName : TEXT;
             type           : Type;
             array          : Array;
             parent         : Internal) : Internal;
    (* if not overridden, will return a default result;
       if overridden, result will be used recursively *)

    field(name : TEXT;
          at : CompRange.T;
          lsb, width : CARDINAL;
          parent : Internal);
    (* must override *)
  END;

  Internal <: ROOT;

  DefInternal = Internal OBJECT
    name, typeName : TEXT;
    type : Type;
    array : Array;
    parent : Internal;
  END;

CONST Brand = "AddrVisitor";
      
END AddrVisitor.
    
