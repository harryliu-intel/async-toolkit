INTERFACE PgCRIF;
IMPORT PgField;
IMPORT Pathname;

TYPE Processor = PROCEDURE (b : ARRAY PgField.T OF TEXT; lenPerByte : CARDINAL);
     
PROCEDURE Parse(path : Pathname.T; processBuf : Processor);

END PgCRIF.
