INTERFACE LexFmt;
IMPORT Wr;
IMPORT Rd, TokSpec;
TYPE
  T <: Public;
  Public = OBJECT METHODS
    writeInterface(to: Wr.T);
    writeModule(to: Wr.T);
    test();
  END;
PROCEDURE New(from: Rd.T; tok: TokSpec.T;
              outMN, tokMN: TEXT): T;
END LexFmt.
