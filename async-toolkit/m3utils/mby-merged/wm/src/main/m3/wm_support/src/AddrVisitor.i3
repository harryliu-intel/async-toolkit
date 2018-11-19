INTERFACE AddrVisitor;

(* we expect a generated _addr file to include code to launch a visitor *)

TYPE
  T <: Public;

  Type = { Reg, Addrmap, Regfile };

  Array = OBJECT sz : CARDINAL END;

  Public = OBJECT METHODS
    internal(name, typeName : TEXT;
             type : Type;
             array : Array;
             parent : Internal) : Internal;
    (* if not overridden, will return a default result;
       if overridden, result will be used recursively *)

    field(name : TEXT; width, lsb : CARDINAL; parent : Internal);
  END;

  Internal <: ROOT;

CONST Brand = "AddrVisitor";
      
END AddrVisitor.
    
