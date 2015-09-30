MODULE DirectivesTable EXPORTS Directives;
IMPORT NodePair, NameSet, NameSetDef, NodePairRefTbl;

REVEAL
  Table =  NodePairRefTbl.Default OBJECT METHODS
    allNodes() : NameSet.T;
  END BRANDED OBJECT 
    nodes : NameSet.T := NIL;
  OVERRIDES
    put      := Put;
    allNodes := AllNodes;
  END;

PROCEDURE AllNodes(t : Table) : NameSet.T = 
  BEGIN 
    IF t.nodes = NIL THEN 
      t.nodes := NEW(NameSetDef.T).init()
    END;
    RETURN t.nodes
  END AllNodes;

PROCEDURE Put(t : Table; 
              READONLY pair : NodePair.T; 
              READONLY ref : REFANY) : BOOLEAN =
  BEGIN
    IF t.nodes = NIL THEN t.nodes := NEW(NameSetDef.T).init() END;
    EVAL t.nodes.insert(pair.fanin);
    EVAL t.nodes.insert(pair.fanout);
    RETURN NodePairRefTbl.Default.put(t,pair,ref)
  END Put;

BEGIN END DirectivesTable.
