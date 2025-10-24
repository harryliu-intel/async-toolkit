GENERIC INTERFACE Symtab(Key, Value, KeyValueTbl);

TYPE
  Super = KeyValueTbl.Default;

  T <: Public;

  Public = Super OBJECT
    up : T := NIL;
    arc : TEXT; (* if a context name is needed *)
  METHODS
    define(k : Key.T; v : Value.T);

    init(overwriteOK := FALSE) : T;

    lookup(prop : Key.T) : Value.T;

    update(withKey : Key.T; new : Value.T);

    dump(debugLevel : CARDINAL := 10);

    getPath(prop : Key.T; sep : TEXT) : TEXT;
    (* get path to a given key, not including the key itself,
       through the arcs *)
    
  END;

CONST Brand = "Symtab(" & Key.Brand & "," & Value.Brand & "," & KeyValueTbl.Brand & ")";

END Symtab.
