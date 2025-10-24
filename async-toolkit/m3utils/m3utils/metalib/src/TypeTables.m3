MODULE TypeTables;
FROM Dsim IMPORT Define, Decl;
IMPORT NameRefTbl, NameRefListTbl, NameNameTbl, NameNameListTbl;
IMPORT Name, NameList, RefList, NameSetDef;
<*NOWARN*>IMPORT IO, Fmt;

TYPE 
  Meta = OBJECT
    root : Define;
    types : NameRefTbl.T;
    it : InstanceTypes;
    ti : TypeInstances;

    up : NameRefListTbl.T;
  END;

  InstanceTypes = NameNameTbl.Default OBJECT
    meta : Meta;
  OVERRIDES
    get := GetIT;
  END;

  TypeInstances = NameNameListTbl.Default OBJECT
    meta : Meta;
  OVERRIDES
    get := GetTI;
  END;

PROCEDURE PopulateUpTable(d : Define; tbl : NameRefListTbl.T) =
  VAR p := d.decls;
  BEGIN
    WHILE p # NIL DO
      VAR lst : RefList.T := NIL;
      BEGIN
        WITH tn = NARROW(p.head,Decl).type DO
          <*ASSERT d.typeName # tn*>
          EVAL tbl.get(tn,lst);
          lst := RefList.Cons(d,lst);
          EVAL tbl.put(tn,lst)
        END
      END;
      p := p.tail
    END
  END PopulateUpTable;

PROCEDURE Make(root : Define; types : NameRefTbl.T) : Tables =
  BEGIN
    WITH meta = NEW(Meta, 
                    root := root, 
                    types := types,
                    up := NEW(NameRefListTbl.Default).init() ),
         it = NEW(InstanceTypes, meta := meta).init(),
         ti = NEW(TypeInstances, meta := meta).init() DO

      meta.it := it; meta.ti := ti;

      PopulateUpTable(root,meta.up);
      WITH iter = types.iterate() DO
        VAR n : Name.T; r : REFANY; BEGIN
          WHILE iter.next(n, r) DO PopulateUpTable(r, meta.up) END
        END
      END;

      RETURN Tables { it, ti }
    END
  END Make;
  
PROCEDURE GetIT(tbl          : InstanceTypes; 
                READONLY key : Name.T; 
                VAR      val : Name.T) : BOOLEAN =
  BEGIN
    IF NameNameTbl.Default.get(tbl,key,val) THEN
      RETURN TRUE
    ELSE
      WITH res = SearchInstanceType(tbl.meta, tbl.meta.root, key) DO
        IF res = NIL THEN 
          RETURN FALSE
        ELSE
          val := res.type;
          EVAL NameNameTbl.Default.put(tbl,key,val);
          RETURN TRUE
        END
      END
    END
  END GetIT;

VAR EmptyName := Name.Empty();

PROCEDURE SearchInstanceType(meta : Meta;
                             define : Define; 
                             rest   : Name.T) : Decl =
  
  PROCEDURE SearchDecl(named : Name.T; VAR decl : Decl) : BOOLEAN =
    VAR p := define.decls;
    BEGIN
      WHILE p # NIL DO
        WITH d = NARROW(p.head,Decl) DO
          IF d.id = named THEN decl := d; RETURN TRUE END
        END;
        p := p.tail
      END;
      RETURN FALSE
    END SearchDecl;

  VAR
    try := rest;
    rem := EmptyName;
    decl : Decl;
    down : REFANY;

  BEGIN
    WHILE try # EmptyName DO
      IF SearchDecl(try, decl) THEN
        IF rem = EmptyName THEN 
          (* found actual declaration *)
          RETURN decl 
        ELSE
          (* found a path towards declaration but have to continue pushing *)
          EVAL meta.types.get(decl.type,down);
          RETURN SearchInstanceType(meta, down, rem)
        END
      ELSE
        rem := Name.Append(Name.Tail(try),rem);
        try := Name.Parent(try)
      END
    END;

    RETURN NIL
  END SearchInstanceType;

(**********************************************************************)

PROCEDURE GetTI(tbl          : TypeInstances;
                READONLY key : Name.T;
                VAR      val : NameList.T) : BOOLEAN =
  BEGIN
    IF NameNameListTbl.Default.get(tbl,key,val) THEN
      RETURN TRUE
    ELSE
      WITH lst = SearchInstances(tbl.meta, key) DO
        IF lst # NIL THEN 
          val := lst;
          EVAL NameNameListTbl.Default.put(tbl,key,val);
          RETURN TRUE
        ELSE
          RETURN FALSE
        END
      END
    END
  END GetTI;

PROCEDURE SearchInstances(meta : Meta; this : Name.T) : NameList.T =

  PROCEDURE SearchParent( d : Define ) =
    VAR q := d.decls;
    BEGIN

(*
      IO.Put("Searching for " & Name.Format(this) & " in type " & 
        Name.Format(d.typeName) & "\n");
*)

      WHILE q # NIL DO
        WITH decl = NARROW(q.head, Decl) DO
          IF decl.type = this THEN (* found it *)

(*
            IO.Put("Found " & Name.Format(this) & " as instance "&
              Name.Format(decl.id) & " in supercell of type " & 
              Name.Format(d.typeName) & " root=" & Fmt.Bool(d=meta.root) & 
              "\n");
*)
            
            IF d = meta.root THEN 
              EVAL set.insert(decl.id) 
            ELSE
              VAR
                lst : NameList.T := NIL;
              BEGIN
                EVAL meta.ti.get(d.typeName,lst);
                WHILE lst # NIL DO
                  EVAL set.insert(Name.Append(lst.head,decl.id));
                  lst := lst.tail
                END
              END
            END
          END
        END;
        q := q.tail
      END
    END SearchParent;

  VAR p : RefList.T := NIL;
      set := NEW(NameSetDef.T).init();
  BEGIN
    EVAL meta.up.get(this,p);

    WHILE p # NIL DO SearchParent(p.head); p := p.tail END;

    VAR
      iter := set.iterate();
      res : NameList.T := NIL;
      n : Name.T;
    BEGIN
      WHILE iter.next(n) DO res := NameList.Cons(n,res) END;
      RETURN res
    END
  END SearchInstances;
  
BEGIN END TypeTables.
