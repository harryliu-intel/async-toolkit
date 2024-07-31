INTERFACE LibertyParse;
IMPORT Rd;
IMPORT LibertyComponent;

PROCEDURE Parse(rd : Rd.T) : LibertyComponent.T RAISES { Rd.Failure };

END LibertyParse.
