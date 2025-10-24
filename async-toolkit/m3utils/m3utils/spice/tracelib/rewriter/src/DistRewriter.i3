INTERFACE DistRewriter;
IMPORT Pathname;
IMPORT TraceOp;
IMPORT TraceRewriter;
IMPORT ArithConstants;

TYPE
  T = Master;
  
  Master <: Public;

  Public = OBJECT METHODS

    init(root     : Pathname.T;
         nthreads : CARDINAL;
         cmdPath  : Pathname.T;
         rew      : TraceRewriter.T) : T;
    
    addNamedOp(op      : TraceOp.T;
               nm      : TEXT;
               relPrec : LONGREAL;
               code    : ArithConstants.Encoding := ArithConstants.Automatic);
    
    flush();
    
  END;

CONST Brand = "DistRewriter";

PROCEDURE RunSlave(root : Pathname.T);
  (* never returns -- run my process as a DistRewriter slave *)

END DistRewriter.
