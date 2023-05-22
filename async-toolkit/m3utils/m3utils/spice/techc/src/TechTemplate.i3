INTERFACE TechTemplate;
IMPORT Pathname, OSError, TextSeq, TextTextTbl;
IMPORT Rd, Wr;

PROCEDURE LoadTemplate(path : Pathname.T) : TextSeq.T
  RAISES { OSError.E, Rd.Failure };
  
PROCEDURE ModifyTemplate(template : TextSeq.T; map : TextTextTbl.T);
  
PROCEDURE WriteTemplate(template : TextSeq.T; path : Pathname.T)
  RAISES { OSError.E, Wr.Failure };

END TechTemplate.
