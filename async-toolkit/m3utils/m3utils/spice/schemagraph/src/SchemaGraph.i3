INTERFACE SchemaGraph;
IMPORT Pathname;
IMPORT Schema;
IMPORT TextSeq;
IMPORT RefSeq;
IMPORT Scheme;

PROCEDURE ReadSchema(spn : Pathname.T) : Schema.T;

PROCEDURE ReadData(schema : Schema.T; files : TextSeq.T) : RefSeq.T;

PROCEDURE EvalFormulas(scm : Scheme.T; schema : Schema.T; data : RefSeq.T);

PROCEDURE DoSweeps(targDir : Pathname.T; schema : Schema.T; data : RefSeq.T; doLabels : BOOLEAN);

CONST Brand = "SchemaGraph";

END SchemaGraph.
