INTERFACE LexParse;
IMPORT NFA;
IMPORT Rd;
IMPORT TokSpec;
IMPORT TextList;
TYPE
  T <: Public;
  Public = OBJECT
    n: NFA.T;
    names: TextList.T;
  END;
PROCEDURE New(from: Rd.T; tok: TokSpec.T): T;
END LexParse.
