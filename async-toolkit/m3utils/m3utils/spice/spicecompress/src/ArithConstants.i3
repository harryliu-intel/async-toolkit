INTERFACE ArithConstants;

(* constants for arithmetic coding *)

IMPORT FreqTable;

TYPE
  T = FreqTable.T;

CONST Brand = "ArithConstants";

(* codebook 0 is always "no arithmetic coding" *)

CONST ZeroCode = 0;
      MinCode  = ZeroCode;
      MaxCode  = 1;
      
TYPE CodeIdx = [ MinCode .. MaxCode ];

CONST EqualCode = T { 1, .. }; (* very basic arith code *)
     
CONST CodeBook = ARRAY [ 1 .. LAST(CodeIdx) ] OF T {
    EqualCode
  };

END ArithConstants.
