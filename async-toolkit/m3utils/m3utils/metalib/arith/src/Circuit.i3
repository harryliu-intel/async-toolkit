INTERFACE Circuit;
IMPORT Name, Dsim, PRS, TimingModel;
IMPORT NameNameSetTbl;
IMPORT NameSet;
IMPORT NameRefTbl;
IMPORT RefList;
IMPORT Directives;
IMPORT Arith, Atom;
IMPORT NodePair;

PROCEDURE ScanForNames(rules : RefList.T) : NameSet.T;

PROCEDURE Build(types        : NameRefTbl.T; (* from Dsim.Parse *)
                define       : Dsim.Define;
                instanceName : Name.T;
                tm           : TimingModel.T;
                globalNames  : NameSet.T;
                skipTargets  : NameSet.T;
                canonicalizer: Canonicalizer) : PRS.T;

TYPE Canonicalizer = OBJECT METHODS canon(node : Name.T) : Name.T END;

TYPE
  CktTimingModel <: PubTimingModel;

  PubTimingModel = TimingModel.T OBJECT METHODS
    init(tbl : Directives.Table; aliasTbl : NameNameSetTbl.T) : CktTimingModel;
    addArbitraryDelay(fanin, fanout : Name.T);

    getVar(named : Atom.T) : Arith.T;
  END;

PROCEDURE AddOverride(p                                : NodePair.T; 
                      overrideDelayLo, overrideDelayHi : LONGREAL);
  (* override (from_fanout,from_fanin) with specified delay *)


END Circuit.
