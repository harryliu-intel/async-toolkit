INTERFACE Dsim;
IMPORT Rd;
IMPORT NameRefTbl, RefList, NameSeq, Name, NameSet;
IMPORT NameNameTbl, NameNameListTbl;

TYPE
  Decl     = OBJECT 
    type, id : Name.T; 
    args : NameSeq.T 
  END;
  DsimBody = OBJECT 
    rules    : RefList.T := NIL;
  END;
  Define   = OBJECT
    typeName : Name.T;
    args     : NameSeq.T;
    dsimBody : DsimBody;
    decls    : RefList.T := NIL;
    wires    : NameRefTbl.T;
  END;
  Wire     = OBJECT
    nodes    : NameSet.T;
  END;
  Conjunct = OBJECT
    input    : Name.T;
    sense    : Sense;
  END;
  Rule     = OBJECT
    conjuncts : RefList.T := NIL;
    target    : Name.T;
    sense     : Sense;
    attrs     := SET OF Keyword {};
    delay     := LAST(CARDINAL);
  END;

  Sense = { Up, Down };

  Keyword = { After, Define, Dsim, Env, Exclcc, Exclhi, Excllo, Isochronic, 
              Metastab, Unstab, Wire };

CONST KeywordTexts = ARRAY Keyword OF TEXT { "after", "define", "dsim", "env", "exclcc", "exclhi", "excllo", "isochronic", "metastab", "unstab", "wire" };

PROCEDURE Parse(rd    : Rd.T; 
                types : NameRefTbl.T; (* type definitions typeName -> Define *)
                decls : NameRefTbl.T; (* inst definitions id       -> Decl   *)
                VAR topLevelWires : NameRefTbl.T
                ) : Define;
  (* Parse returns its results in two different ways: one, as a Define with
     most fields NIL; two, in the tables and sets passed to it *)

PROCEDURE Flatten(define          : Define; 
                  types           : NameRefTbl.T; (* from Parse *)
                  instanceTypeTbl : NameNameTbl.T;
                  typeInstanceTbl : NameNameListTbl.T;
                  root            : Name.T := NIL );
  (* this is an intermediate solution: avoid reading the "build" file.
     really we shouldn't do either but just traverse the data structure
     on demand (would require a pointer from type up to parent type)  

     Flatten IS NO LONGER USED.  We now search only the part of the
     data structure we're interested in.
  *)

PROCEDURE FormatRule(rule : Rule) : TEXT;

END Dsim.
