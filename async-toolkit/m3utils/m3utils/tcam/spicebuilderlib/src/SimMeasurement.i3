INTERFACE SimMeasurement;
IMPORT Rd;
IMPORT NodeMeasurementSet;
IMPORT ProbeType AS ProbeTypeIntf;

TYPE
  Type = { Max, Min, Ave, Last };

  ProbeType = ProbeTypeIntf.T;
  
CONST TypeNames = ARRAY Type OF TEXT { "Max", "Min", "Ave", "Last" };

      ProbeTypeNames = ARRAY ProbeType OF TEXT { "Voltage", "Current" };

      OpNames = ARRAY Op OF TEXT { "LE", "GE", "UP", "DN" };
      
CONST Brand = "SimMeasurement";

EXCEPTION ParseError(TEXT);
      
TYPE
  T  <: Public;

  Public = OBJECT METHODS
    format(dutName : TEXT (* must include . *);
           convertClockToTime : ClockConverter) : TEXT;
    visitNodeMeasurements(into : NodeMeasurementSet.T);
  END;

  Default <: PublicDef;

  PublicDef = T OBJECT
    nodeNm   : TEXT;
    quantity : ProbeType := ProbeType.Voltage;
    type     : Type;
    from, to : Trigger;
  END;

  Clock <: T;

  Time  <: T;

  Trigger = OBJECT
    spec : T;
    op   : Op;
    val  : LONGREAL;
  END;

  Op = { Le, Ge, Up, Dn };

  ClockConverter = OBJECT METHODS
    timeOfCycle(clock : LONGREAL) : LONGREAL;
  END;

PROCEDURE Parse(rd : Rd.T) : T RAISES { ParseError, Rd.Failure, Rd.EndOfFile };

  
END SimMeasurement.
