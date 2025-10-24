INTERFACE ArithConstants;

(* constants for arithmetic coding *)

IMPORT FreqTable;

TYPE T = FreqTable.T;

CONST Brand = "ArithConstants";

(* codebook 0 is always "no arithmetic coding" *)

CONST ZeroCode : Encoding = 0;
                      (* 
                         ZeroCode means that the signal is not arithmetically
                         encoded, but passed through verbatim 
                      *)
      
      MinCode   = ZeroCode;   (* first legal arithmetic code *)
                      
      
      FirstArith  : Encoding = 1;
                       (*
                          One (of potentially many) arithmetic coding schemes
                       *)  

      MaxCode   = FirstArith; (* last legal arithmetic code *)

      Automatic : Encoding = MaxCode + 1;
                       (* 
                          this is a special code, not used in the files,
                          only between modules, to denote that the selection
                          of encoding scheme shall be automatic.

                          Because it is only used within the executable and
                          not in stable storage, it is OK for it to change
                          during program development.  (It changes in value
                          as more coding schemes are added.)
                       *)

      Pickle    : Encoding  = 253;
                        (* 
                           this is a special code, not used for arithmetic coding

                           It signifies that the data is a Modula-3 Pickle
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
      
      
TYPE Encoding = [ 0 .. 255 ];
     (* one byte *)
     
     CodeIdx  = [ MinCode .. MaxCode ];
     (* these are the codes that are _actually_used_ for arithmetic coding *)

     XCodeIdx = [ MinCode .. Automatic ];
     (* these are the codes that can be used for ENcoding *)
     
CONST LastArith = MaxCode; (* do we need two names for this? *)
CONST ArithCodes = SET OF Encoding { FirstArith .. MaxCode };

CONST EqualCode = T { 1, .. }; (* very basic arith code *)
     
CONST CodeBook = ARRAY [ 1 .. MaxCode ] OF T {
    EqualCode
  };

END ArithConstants.
