INTERFACE SpiceValues;
IMPORT CheckDir;
IMPORT CardSeq;
IMPORT TransitionFinder;

PROCEDURE GetValues(clkDirs           : CheckDir.T;
                    clkTrIdx          : CARDINAL;
                    clkNm             : TEXT;
                    thresh            : LONGREAL;
                    READONLY ta, da   : ARRAY OF LONGREAL;
                    tranFinder        : TransitionFinder.T;
                    resetTime         : LONGREAL) : CardSeq.T;


END SpiceValues.
