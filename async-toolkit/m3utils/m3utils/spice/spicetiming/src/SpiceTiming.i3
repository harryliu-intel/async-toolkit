INTERFACE SpiceTiming;

(* 
   check timing constraints from a spice deck
   -- setup
   -- hold
   -- pulsewidth

 *)

IMPORT Pathname;
IMPORT CheckMode;
IMPORT Transition;
IMPORT CheckDir;
IMPORT Trace;
IMPORT SpiceCircuit;
IMPORT TextSet;
IMPORT TransitionFinder;
IMPORT TextTextTbl;

PROCEDURE MeasureFromSpice(spiceFn        : Pathname.T;
                           quick          : BOOLEAN;
                           nMargins       : CARDINAL;
                           trace          : Trace.T;
                           translate      : BOOLEAN;
                           rootType       : TEXT;
                           rootCkt        : SpiceCircuit.T;
                           mapper         : Mapper;
                           traceRt        : Pathname.T;
                           graphNs        : LONGREAL;
                           Dot            : TEXT;
                           allNames       : TextSet.T;
                           dutPfx         : TEXT;
                           tranFinder     : TransitionFinder.T;
                           resetTime      : LONGREAL;
                           mappedNames    : TextTextTbl.T);
  (* measure timing, using a spice deck as the spec *)
  
PROCEDURE MeasureByName(truncValues : CARDINAL;
                        trace       : Trace.T;
                        traceRt     : TEXT;
                        vdd         : LONGREAL;
                        valueTag    : TEXT;
                        graphNs     : LONGREAL;
                        nMargins    : CARDINAL;
                        tranFinder  : TransitionFinder.T;
                        resetTime   : LONGREAL;
                        mappedNames : TextTextTbl.T);
  (* measure timing, from a graybox extracted circuit, assuming 
     certain mappings for the clock and Q inputs of the circuits

     The logic to find Q, D, and CLK is in the Latches interface
     (specifically, Latches.IsQNode).

     A simple future extension would be to pass in an object that
     has the same interface as IsQNode and allow the client to
     provide said object.
  *)

TYPE
  Node = RECORD
    nm       : TEXT;
    internal : BOOLEAN;
  END;

  N = Node;
  
  Arc = RECORD
    fr, to   : Node;
    mode     : CheckMode.T;
    clkDir   : Transition.Dir;
    dataDir  : CheckDir.T;
  END;

  ArcArr = ARRAY [ 0 .. MaxArcs - 1 ] OF Arc;
  
  LatchSpec = RECORD
    typeNamePattern : TEXT;
    arcs            : ArcArr;
  END;

CONST
  MaxArcs      = 6;
  QuickMargins = 1000;
  NoNode       = N { NIL, FALSE };
  NoArc        = Arc { NoNode, NoNode, FIRST(CheckMode.T), 0, Dn };

CONST
  Up = CheckDir.T { 1 };
  Dn = CheckDir.T { -1 };
  UD = CheckDir.T { -1, 1 };

TYPE Mapper = PROCEDURE(t : TEXT) : TEXT;


END SpiceTiming.
