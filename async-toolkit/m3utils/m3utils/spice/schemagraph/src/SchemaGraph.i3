INTERFACE SchemaGraph;
IMPORT Pathname;
IMPORT Schema;
IMPORT TextSeq;
IMPORT RefSeq;
IMPORT Scheme;
IMPORT OSError, Rd;

PROCEDURE ReadSchema(spn : Pathname.T) : Schema.T
  RAISES { OSError.E, Rd.Failure };
  
PROCEDURE ReadData(schema : Schema.T; files : TextSeq.T) : RefSeq.T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile };

PROCEDURE EvalFormulas(scm : Scheme.T; schema : Schema.T; data : RefSeq.T)
  RAISES { Scheme.E };

PROCEDURE DoSweeps(targDir : Pathname.T; schema : Schema.T; data : RefSeq.T; doLabels : BOOLEAN);

CONST Brand = "SchemaGraph";

END SchemaGraph.
