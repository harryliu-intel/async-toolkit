INTERFACE Gox;
IMPORT TextList;
IMPORT BraceParse;

PROCEDURE ProduceReports(roots : TextList.T;
                         parsed : BraceParse.T;
                         levels : CARDINAL;
                         transistorReportSfx : TEXT);

CONST Brand = "Gox";

END Gox.
