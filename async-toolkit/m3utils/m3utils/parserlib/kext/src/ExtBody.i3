INTERFACE ExtBody;
IMPORT LoadSpec;
IMPORT TextSubs;
IMPORT Rd;
TYPE
  T = TextSubs.T;
PROCEDURE Parse(from: Rd.T; READONLY spec: LoadSpec.Info): T;
END ExtBody. 
