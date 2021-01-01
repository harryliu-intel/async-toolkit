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
IMPORT CktNodeList AS NodeList;
IMPORT CktNodeSet, CktNodeSetDef;
IMPORT NodeProperty;
IMPORT NodePropertySet AS NdProps;
IMPORT ElementProperty;
IMPORT ElementPropertySet AS EtProps;
IMPORT TextUtils;
IMPORT Text;
IMPORT CktGraphDfs AS Dfs;
IMPORT FetArray;

VAR doDebug := Debug.DebugThis("SpiceAnalyze");
    
REVEAL
  CktCell.T = BRANDED OBJECT
    typeNm   : TEXT;
    subcells : TextCktCellTbl.T;
  END;

TYPE
  EtProp = ElementProperty.T;
  NdProp = NodeProperty.T;

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

      canons : TextSet.T;
    BEGIN
      canons := BuildNodeTab(canonTbl, ckt, subCkts, nodeTab, power);
      
      BuildElementTab(ckt, subCkts, nodeTab, elemTab, ckt.name);

      Debug.Out(F("circuit %s : %s nodes, %s elements",
                  ckt.name, Int(nodeTab.size()), Int(elemTab.size())));

      MarkNodeProperties(nodeTab, elemTab);
      
      gateOutputs := FindGateOutputs(canons, nodeTab);

      CONST
        MatchSets = ARRAY OF EtProps.T {
          EtProps.T { EtProp.IsNfet },
          EtProps.T { EtProp.IsPfet }
        };
      VAR
        n : Node;
        visitor := NEW(GateFindVisitor);
        iter    := gateOutputs.iterate();
      BEGIN
        WHILE iter.next(n) DO
          FOR i := FIRST(MatchSets) TO LAST(MatchSets) DO
            visitor.eMustMatch := MatchSets[i];
            visitor.fetArray := NEW(FetArray.T).init();

            Debug.Out("Finding gate from " & Int(n.id));
            Dfs.Node(n, visitor);
            Debug.Out(F("%s elements", Int(visitor.fetArray.size())))
          END
        END
      END
    END;

  END Cell;

TYPE
  GateFindVisitor = Dfs.NodeVisitor OBJECT
    eMustMatch : EtProps.T;
    fetArray   : FetArray.T;
  OVERRIDES
    visit := GateFindVisit;
  END;

TYPE FetTerminal = { Drain, Gate, Source, Body };

PROCEDURE IsFetTerminal(e : Element; n : Node; term : FetTerminal) : BOOLEAN =
  VAR
    found := -1;
  BEGIN
    <*ASSERT e.props * EtProps.T { EtProp.IsNfet, EtProp.IsPfet } # EtProps.Empty *>
    FOR i := 0 TO e.terminals.size() - 1 DO
      IF e.terminals.get(i) = n THEN found := i END
    END;
    <*ASSERT found # -1 *>
    RETURN found = ORD(term)
  END IsFetTerminal;
  
PROCEDURE GateFindVisit(v    : GateFindVisitor;
                        path : NodeList.T;
                        via  : Element;
                        this : Node) : BOOLEAN =
  VAR
    prev := path.head;
  BEGIN
    Debug.Out(F("GateFindVisit : n %s -> e %s -> n %s",
                Int(prev.id), Int(via.id), Int(this.id)));

    IF NdProp.IsVdd IN this.props OR NdProp.IsGnd IN this.props THEN
      (* end of line, we are at power supply *)
      Debug.Out(F("GateFindVisit n %s at power supply", Int(this.id)));
      RETURN FALSE
    ELSIF v.eMustMatch * via.props # v.eMustMatch THEN
      Debug.Out(F("GateFindVisit non-transistor e %s", Int(via.id)));
      RETURN FALSE
    ELSIF NOT (IsFetTerminal(via, this, FetTerminal.Source) OR
               IsFetTerminal(via, this, FetTerminal.Source)) THEN
      Debug.Out(F("GateFindVisit not source or drain of %s", Int(via.id)));
      RETURN FALSE
    ELSE
      v.fetArray.addToRow(via, NodeList.Length(path) - 1);
      RETURN TRUE
    END
  END GateFindVisit;

PROCEDURE FindGateOutputs(canons  : TextSet.T;
                          nodeTab : TextNodeTbl.T) : CktNodeSet.T =
  VAR
    res := NEW(CktNodeSetDef.T).init();
    iter := canons.iterate();
    nn : TEXT;
    n : Node;
  BEGIN
    WHILE iter.next(nn) DO
      WITH hadIt = nodeTab.get(nn, n) DO
        <*ASSERT hadIt*>
      END;
      IF NdProp.IsNfetSourceDrain IN n.props AND
         NdProp.IsPfetSourceDrain IN n.props THEN
        Debug.Out("Found a putative gate output : " & nn);
        EVAL res.insert(n)
      END
    END;
    RETURN res
  END FindGateOutputs;

TYPE TransistorType = { N, P, Unknown };
     
PROCEDURE DecodeTransistorTypeName(tn : TEXT) : TransistorType =
  BEGIN
    WITH low   = TextUtils.ToLower(tn),
         first = Text.GetChar(tn, 0) DO
      CASE first OF
        'p' => RETURN TransistorType.P
      |
        'n' => RETURN TransistorType.N
      ELSE
        RETURN TransistorType.Unknown
      END
    END
  END DecodeTransistorTypeName;

PROCEDURE MarkNodeProperties(nodeTab : TextNodeTbl.T;
                             elemTab : TextElementTbl.T) =

  PROCEDURE Mark(n : Node; prop : NdProp) =
    BEGIN
      n.props := n.props + NdProps.T { prop }
    END Mark;
    
  VAR
    iter := elemTab.iterate();
    e : Element;
    t : TEXT;
  BEGIN
    WHILE iter.next(t, e) DO
      TYPECASE e.src OF
        SpiceObject.M(m) =>
        WITH tt   = DecodeTransistorTypeName(m.type),
             drnT = e.terminals.get(0),
             gatT = e.terminals.get(1),
             srcT = e.terminals.get(2),
             bodT = e.terminals.get(3) DO
          CASE tt OF
            TransistorType.N => 
            Mark(drnT, NdProp.IsNfetSourceDrain);
            Mark(gatT, NdProp.IsNfetGate);
            Mark(srcT, NdProp.IsNfetSourceDrain);
            Mark(bodT, NdProp.IsNfetBody);
          |
            TransistorType.P =>
            Mark(drnT, NdProp.IsPfetSourceDrain);
            Mark(gatT, NdProp.IsPfetGate);
            Mark(srcT, NdProp.IsPfetSourceDrain);
            Mark(bodT, NdProp.IsPfetBody);
          |
            TransistorType.Unknown =>
          END
        END
      ELSE
      END
    END
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
           new = NEW(Element,
                     id := id,
                     src := e,
                     terminals := NEW(CktNodeSeq.T).init()),
           fqn = path & "." & e.name DO
        INC(id);
        TYPECASE e OF
          SpiceObject.M(m) =>
          WITH tt = DecodeTransistorTypeName(m.type) DO
            CASE tt OF
              TransistorType.P => new.props := EtProps.T { EtProp.IsPfet }
            |
              TransistorType.N => new.props := EtProps.T { EtProp.IsNfet }
            ELSE
            END
          END
        ELSE
        END;
        
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

PROCEDURE FinalArc(nm : TEXT) : TEXT =
  BEGIN
    FOR i := Text.Length(nm) - 1 TO 0 BY -1 DO
      IF Text.GetChar(nm, i) = '.' THEN
        RETURN Text.Sub(nm, i + 1, LAST(CARDINAL))
      END
    END;
    RETURN nm
  END FinalArc;

PROCEDURE BuildNodeTab(canonTbl : TextTextTbl.T;
                       ckt     : SpiceCircuit.T;
                       subCkts : TextSpiceCircuitTbl.T;
                       nodeTab : TextNodeTbl.T;
                       powerSets : PowerSets) : TextSet.T =
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
            WITH suffix = FinalArc(u) DO
              IF    powerSets[Power.GND].member(suffix) THEN
                Debug.Out("Found ground node " & u);
                node.props := node.props + NdProps.T { NdProp.IsGnd }
              ELSIF powerSets[Power.Vdd].member(suffix) THEN
                Debug.Out("Found power node " & u);
                node.props := node.props + NdProps.T { NdProp.IsVdd }
              END
            END;
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
    END;
    RETURN canons
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
