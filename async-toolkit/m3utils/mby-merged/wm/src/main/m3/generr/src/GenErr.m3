MODULE GenErr EXPORTS Main;

IMPORT CSVParse;
IMPORT FileRd;
IMPORT Params;
IMPORT Rd;
IMPORT FmError, FmErrorSeq;
IMPORT Scan;

VAR
  rd := FileRd.Open(Params.Get(1));
  csv := NEW(CSVParse.T).init(rd);
  seq := NEW(FmErrorSeq.T).init();
  e : FmError.T;
  
BEGIN
  TRY
    LOOP
      csv.startLine();
      e := FmError.T { csv.cell(), Scan.Int(csv.cell()), csv.cell() };
      seq.addhi(e)
    END
  EXCEPT
    Rd.EndOfFile => (* done *)
  END
END GenErr.



