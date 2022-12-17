INTERFACE ArithConstants;

(* constants for arithmetic coding *)

IMPORT FreqTable;

TYPE
  T = FreqTable.T;

CONST Brand = "ArithConstants";

(* codebook 0 is always "no arithmetic coding" *)

CONST ZeroCode  = 0;
      MinCode   = ZeroCode;
      MaxCode   = 1;
      DenseCode = 255; (* this is a special code, not used for arithmetic coding.
                          It means that the following data is dense floats,
                          as in the original aspice format.
                          
                          This value is only used in the final trace format,
                          not in the intermediate format in ct.work
                       *)
      
TYPE CodeIdx = [ MinCode .. MaxCode ];

     Encoding = [ ZeroCode .. DenseCode ]; (* one byte *)

CONST EqualCode = T { 1, .. }; (* very basic arith code *)
     
CONST CodeBook = ARRAY [ 1 .. LAST(CodeIdx) ] OF T {
    EqualCode
  };

END ArithConstants.
