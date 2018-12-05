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
