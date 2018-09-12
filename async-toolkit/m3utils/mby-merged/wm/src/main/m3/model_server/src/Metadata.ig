GENERIC INTERFACE Metadata(Record);
IMPORT Metadata;

TYPE
  T = Metadata.T OBJECT
    m : Record.T;
  END;

CONST Brand = "Metadata(" & Record.Brand & ")";

END Metadata.
  
