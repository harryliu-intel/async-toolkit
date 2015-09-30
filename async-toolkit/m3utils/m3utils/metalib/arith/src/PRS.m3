MODULE PRS;
IMPORT Name, NameRefTbl;
IMPORT TimingModel;
IMPORT PRSClass;

REVEAL
  T = PRSClass.Private BRANDED OBJECT
  OVERRIDES
    init := Init;
    node := MakeNode;
  END;

PROCEDURE Init(t : T; tm : TimingModel.T) : T = 
  BEGIN 
    t.tm := tm; t.nodes := NEW(NameRefTbl.Default).init(); RETURN t 
  END Init;

PROCEDURE MakeNode(t : T; named : Name.T) : Node =
  BEGIN
    WITH n = NEW(Node, name := named, transitions := NIL) DO
      EVAL t.nodes.put(named, n);
      RETURN n
    END
  END MakeNode;

BEGIN END PRS.
