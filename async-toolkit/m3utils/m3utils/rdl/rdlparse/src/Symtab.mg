GENERIC MODULE Symtab(Key, Value);
IMPORT Debug;
FROM Fmt IMPORT F;

REVEAL
  T = Public BRANDED Brand OBJECT
    overwriteOK : BOOLEAN;
  OVERRIDES
    define  := Define;
    init    := Init;
    lookup  := Lookup;
    update  := Update;
    dump    := Dump;
    getPath := GetPath;
  END;

PROCEDURE Define(t : T; k : Key.T; v : Value.T) =
  BEGIN
    VAR hadIt := t.put(k,v); BEGIN

      IF hadIt THEN
        IF t.overwriteOK THEN
          
          Debug.Warning(F("Key multiply defined: %s", Key.Format(k)))
        ELSE
                    
          Debug.Error(F("Key multiply defined: %s -- dumping symbol table",
                        Key.Format(k)),
                      exit := FALSE);
          VAR iter := t.iterate();
              kk : Key.T;
              vv : Value.T;
          BEGIN
            WHILE iter.next(kk,vv) DO
              Debug.Out(Key.Format(kk),0)
            END
          END;
            
          Debug.Error("Key multiply defined == quitting")
        END
      END
    END
  END Define;

PROCEDURE Init(t : T; overwriteOK : BOOLEAN) : T =
  BEGIN t := Super.init(t); t.overwriteOK := overwriteOK; RETURN t  END Init;

PROCEDURE Lookup(t : T; p : Key.T) : Value.T =
  VAR
    x : Value.T;
  BEGIN
    WHILE t # NIL DO
      IF t.get(p, x) THEN RETURN x END;
      t := t.up
    END;
    RETURN NIL
  END Lookup;

PROCEDURE Update(t : T; p : Key.T; new : Value.T) =
  VAR
    x : Value.T;
  BEGIN
    WHILE t # NIL DO
      IF t.get(p, x) THEN
        WITH hadIt = t.put(p, new) DO
          <*ASSERT hadIt*>
          RETURN
        END
      END;
      t := t.up
    END;
    <*ASSERT FALSE*> 
  END Update;

PROCEDURE Dump(t : T; debugLevel : CARDINAL) =
  VAR
    margin := "";
  BEGIN
    WHILE t # NIL DO
      VAR
        iter := t.iterate();
        k : Key.T;
        v : Value.T;
      BEGIN
        WHILE iter.next(k,v) DO
          Debug.Out(margin & Key.Format(k), debugLevel)
        END
      END;
      t := t.up;
      margin := margin & "  "
    END
  END Dump;
  
PROCEDURE GetPath(t : T; p : Key.T; sep : TEXT) : TEXT =
  VAR
    x : Value.T;
    res : TEXT := "";
  BEGIN
    WHILE t # NIL DO
      IF t.get(p, x) THEN
        WHILE t.up # NIL DO
          res := t.arc & sep & res;
          t := t.up
        END;
        RETURN res
      END;
      t := t.up
    END;
    <*ASSERT FALSE*>
  END GetPath;

BEGIN END Symtab.
