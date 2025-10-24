(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MemoryMap;
IMPORT CompMemory;
IMPORT AddrVisitor;
IMPORT UpdaterFactory, CompAddr;

TYPE
  T <: Public;

  Public = CompMemory.T OBJECT METHODS
    visit(visitor : AddrVisitor.T);
    init(base : CompAddr.T; factory : UpdaterFactory.T := NIL) : T;
  END;
     
  (* this would normally be implemented by a type H in a _map_addr.i3 *)

CONST Brand = "MemoryMap";

END MemoryMap.
