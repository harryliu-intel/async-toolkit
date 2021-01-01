MODULE SpiceAnalyze;
IMPORT Debug;
IMPORT SpiceCircuit;
IMPORT TextSpiceObjectTbl;
FROM Fmt IMPORT F, Int;
IMPORT CktGraph;
IMPORT CktCell;
IMPORT TextCktCellTbl;
IMPORT SpiceObject;
IMPORT TextSpiceCircuitTbl;
IMPORT TextTextSetTbl;
IMPORT SpiceInstance;
IMPORT TextTextTbl;
IMPORT TextSpiceInstanceSetTbl;
FROM SpiceFlat IMPORT VisitCktNodes, Canonicalize, BuildCanonTbl;
IMPORT TextSet;
IMPORT TextNodeTbl;
FROM CktGraph IMPORT Node, Element;
IMPORT TextSetDef;
IMPORT TextElementTbl;
IMPORT CktNodeSeq;
IMPORT CktElementList;
IMPORT CktNodeSet, CktNodeSetDef;
IMPORT NodeProperty;

VAR doDebug := Debug.DebugThis("SpiceAnalyze");
    
REVEAL
  CktCell.T = BRANDED OBJECT
    typeNm   : TEXT;
    subcells : TextCktCellTbl.T;
  END;

PROCEDURE Cell(nm      : TEXT;
               ckt     : SpiceCircuit.T;
               power   : PowerSets;
               hierTbl : TextCktCellTbl.T;
               subCkts : TextSpiceCircuitTbl.T) =
  CONST
    DefaultTopName = "CIRCUIT";
  VAR
    symtab := NEW(TextSpiceObjectTbl.Default).init();
    canonTbl := NEW(TextTextTbl.Default).init();

  BEGIN
    Debug.Out("Working on " & nm);
    Debug.Out(F("name %s", Debug.UnNil(ckt.name)));
    IF ckt.name = NIL THEN
      ckt.name := DefaultTopName;
      Debug.Out(F("updated name %s", Debug.UnNil(ckt.name)));
    END;
    IF doDebug THEN
      FOR i := 0 TO ckt.params.size() - 1 DO
        Debug.Out(F("param[%s] %s", Int(i), ckt.params.get(i)))
      END
    END;
    FOR i := 0 TO ckt.elements.size() - 1 DO
      WITH obj = ckt.elements.get(i) DO
        <*ASSERT obj.name # NIL*>
        IF doDebug THEN
          Debug.Out(F("elem[%s] %s", Int(i), obj.name))
        END;
        EVAL symtab.put(obj.name, obj)
      END
    END;

    VAR
      hier := BuildCellStructure(ckt, symtab, hierTbl);
    BEGIN
      Debug.Out(F("hier.name %s subcells %s",
                  Debug.UnNil(hier.typeNm),
                  Int(hier.subcells.size())));
      EVAL hierTbl.put(ckt.name, hier);
    END;
    
    VAR
      nodeTab := NEW(TextNodeTbl.Default).init();
      (* this will become the mapping of all aliases to the nodes *)

      elemTab := NEW(TextElementTbl.Default).init();

      gateOutputs : CktNodeSet.T;
    BEGIN
      BuildNodeTab(canonTbl, ckt, subCkts, nodeTab);
      
      BuildElementTab(ckt, subCkts, nodeTab, elemTab, ckt.name);

      Debug.Out(F("circuit %s : %s nodes, %s elements",
                  ckt.name, Int(nodeTab.size()), Int(elemTab.size())));

      MarkNodeProperties(nodeTab, elemTab);
      
      gateOutputs := FindGateOutputs(nodeTab)
    END;

  END Cell;

PROCEDURE FindGateOutputs(nodeTab : TextNodeTbl.T) : CktNodeSet.T =
  TYPE
    NdProp = NodeProperty.T;
  VAR
    res := NEW(CktNodeSetDef.T).init();
    iter := nodeTab.iterate();
    nn : TEXT;
    n : Node;
  BEGIN
    WHILE iter.next(nn, n) DO
      IF NdProp.IsNfetTerminal IN n.props AND
         NdProp.IsPfetTerminal IN n.props THEN
        EVAL res.insert(n)
      END
    END;
    RETURN res
  END FindGateOutputs;

PROCEDURE MarkNodeProperties(nodeTab : TextNodeTbl.T;
                             elemTab : TextElementTbl.T) =
  VAR
    iter := nodeTab.iterate();
  BEGIN
  END MarkNodeProperties;
  
PROCEDURE BuildElementTab(ckt     : SpiceCircuit.T;
                          subCkts : TextSpiceCircuitTbl.T;
                          nodeTab : TextNodeTbl.T;
                          elemTab : TextElementTbl.T;
                          path    : TEXT) =
  VAR
    n : Node;
    id := 0;
  BEGIN
    FOR i := 0 TO ckt.elements.size() - 1 DO
      WITH e   = ckt.elements.get(i),
           new = NEW(Element, id := id, src := e, terminals := NEW(CktNodeSeq.T).init()),
           fqn = path & "." & e.name DO
        INC(id);
        FOR i := 0 TO e.terminals.size() - 1 DO
          WITH tn = e.terminals.get(i),
               ffqn = path & "." & tn DO
            IF doDebug THEN
              Debug.Out(F("Seeking name %s", ffqn))
            END;
            WITH hadIt = nodeTab.get(ffqn, n) DO
              IF NOT hadIt THEN
                Debug.Error("Can't find node " & ffqn)
              END
            END;
            (* record the source of the def'n *)
            
            (* cross-link the Node and Element *)
            n.elements := CktElementList.Cons(new, n.elements);
            new.terminals.addhi(n);
          END
        END(*ROF*);

        WITH hadIt = elemTab.put(fqn, new) DO
          <*ASSERT NOT hadIt*>
        END
      END
    END
  END BuildElementTab;

PROCEDURE BuildNodeTab(canonTbl : TextTextTbl.T;
                       ckt     : SpiceCircuit.T;
                       subCkts : TextSpiceCircuitTbl.T;
                       nodeTab : TextNodeTbl.T) =
    VAR
      aliasTab := FindAllAliases(ckt, subCkts, canonTbl);
      iter := aliasTab.iterate();
      t, u : TEXT;
      ts : TextSet.T;

      (* nodeTab will map every alias to the unique Node data structure
         we wish to use *)
      
      canons, namesSoFar := NEW(TextSetDef.T).init();
      canon : TEXT;
      id := 0;
    BEGIN
      WHILE iter.next(t, ts) DO
        IF NOT namesSoFar.member(t) THEN
          (* 
             this is a little sketchy in case we get the .vss special
             case, (see implementation of Canonicalize)

             I think this will break the aliases (aliases are wrong
             in this special case, since we shouldn't be merging at this
             late point in the game, and we will crash instead if we hit
             that case here) 
          *)
          EVAL Canonicalize(t, canon, canonTbl);
          IF doDebug THEN
            Debug.Out(F("canon %s, aliases %s", canon, Int(ts.size())))
          END;
          WITH node = NEW(Node, id := id, aliases := ts, elements := NIL),
               jter = ts.iterate() DO
            INC(id);
            WHILE jter.next(u) DO
              IF doDebug THEN
                Debug.Out(F("alias : \"%s\"", u))
              END;
              WITH hadIt = nodeTab.put(u, node) DO
                <*ASSERT NOT hadIt*>
              END
            END
          END;
          namesSoFar := namesSoFar.union(ts);
          WITH hadIt = canons.insert(canon) DO
            <*ASSERT NOT hadIt*>
          END
        END
      END
    END BuildNodeTab;

PROCEDURE FindAllAliases(ckt      : SpiceCircuit.T;
                         subCkts  : TextSpiceCircuitTbl.T;
                         canonTbl : TextTextTbl.T) : TextTextSetTbl.T =
  VAR
    symTab := NEW(TextTextSetTbl.Default).init();
    topInstance : SpiceInstance.T;
    assocs := NEW(TextSpiceInstanceSetTbl.Default).init();
  BEGIN
    Debug.Out("FindAllAliases: top is called " & Debug.UnNil(ckt.name));

    topInstance := NEW(SpiceInstance.T).init(ckt.name,
                                             NIL (* not right *),
                                             NIL);
    (* what does topInstance do? *)
    
    VisitCktNodes(ckt.name,
                  symTab,
                  ckt,
                  NIL,
                  assocs, (* this associates all the other nodes *)
                  topInstance,
                  subCkts);

    BuildCanonTbl(symTab, canonTbl);
    
    RETURN symTab
  END FindAllAliases;
 
PROCEDURE BuildCellStructure(ckt    : SpiceCircuit.T;
                             symtab : TextSpiceObjectTbl.T;
                             typeTbl : TextCktCellTbl.T) : CktCell.T  =
  VAR
    res := NEW(CktCell.T,
               typeNm := ckt.name,
               subcells := NEW(TextCktCellTbl.Default).init());
    
    subc : CktCell.T;
  BEGIN
    FOR i := 0 TO ckt.elements.size() - 1 DO
      TYPECASE ckt.elements.get(i) OF
        SpiceObject.X(e) =>
        WITH hadIt = typeTbl.get(e.type, subc) DO
          IF NOT hadIt THEN
            Debug.Error(F("Working on %s : subcell type %s not found",
                          ckt.name, e.type))
          END;
          WITH hadIt2 = res.subcells.put(e.name, subc) DO
            IF hadIt2 THEN
              Debug.Error(F("Working on %s : subcell instance %s multiply defined",
                            ckt.name, e.name))
            END
          END
        END
      ELSE
      END
    END;
    RETURN res
  END BuildCellStructure;

BEGIN END SpiceAnalyze.
