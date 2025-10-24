INTERFACE TypeTables;
IMPORT NameNameTbl, NameNameListTbl, Dsim, NameRefTbl;

TYPE 
  Tables = RECORD 
    instanceTypes : NameNameTbl.T;
    typeInstances : NameNameListTbl.T;
  END;

PROCEDURE Make(root : Dsim.Define; types : NameRefTbl.T) : Tables;

END TypeTables.
       
