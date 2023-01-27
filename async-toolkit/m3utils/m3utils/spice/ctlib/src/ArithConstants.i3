INTERFACE ArithConstants;

(* constants for arithmetic coding *)

IMPORT FreqTable;

TYPE T = FreqTable.T;

CONST Brand = "ArithConstants";

(* codebook 0 is always "no arithmetic coding" *)

CONST ZeroCode : Encoding = 0;
      MinCode   = ZeroCode;
      
      FirstArith  : Encoding = 1;
      MaxCode   = FirstArith;

      Pickle    : Encoding = 252;
                        (* 
                           this is a special code, not used for arithmetic coding

                           It signifies that the data is a Modula-3 Pickle
                        *)
                         
      Automatic : Encoding = 253;
                       (* 
                          this is a special code, not used in the files,
                          only between modules, to denote that the selection
                          of encoding scheme shall be automatic 
                       *)

      LinearCode : Encoding = 254;
                       (* 
                          this is a special code, not used for arithmetic coding
      
                          It means that the data is presented in the format
                          <first value>
                          <last value>

                          where both are raw floats
                          and the other data points are interpolated linearly
                       *)
      
      DenseCode : Encoding = 255;
                       (* 
                          this is a special code, not used for arithmetic coding
                          It means that the following data is dense floats,
                          as in the original aspice format.
                          
                          This value is only used in the final trace format,
                          not in the intermediate format in ct.work

                          Note that with the dense format, the data is also
                          not scaled, so the norm from the directory is not
                          used.
                       *)
      
      
TYPE CodeIdx  = [ MinCode .. MaxCode ];

     Encoding = [ 0 .. 255 ]; (* one byte *)

CONST LastArith = MaxCode; (* do we need two names for this? *)
CONST ArithCodes = SET OF Encoding { FirstArith .. MaxCode };

CONST EqualCode = T { 1, .. }; (* very basic arith code *)
     
CONST CodeBook = ARRAY [ 1 .. LAST(CodeIdx) ] OF T {
    EqualCode
  };

END ArithConstants.
