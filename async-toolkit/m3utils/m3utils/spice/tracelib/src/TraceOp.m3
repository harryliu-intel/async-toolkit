MODULE TraceOp;
IMPORT Trace;
IMPORT Rd;
IMPORT LRFunction;
IMPORT MapError;

REVEAL
  T = BRANDED Brand OBJECT
    trace  : Trace.T;
  METHODS
  END;
  
  Array = PublicArray BRANDED Brand & " Array" OBJECT
    result : REF ARRAY OF LONGREAL;
  METHODS
    getWorkingSpace() : REF ARRAY OF LONGREAL := GetWorkingSpace;
    eval() RAISES { Rd.EndOfFile, Rd.Failure };
  OVERRIDES
    exec := Exec;
  END;

  (* the way we structure the code is that exec() is the external interface
     eval() is used internally.

     eval() does the following:

     1. if child nodes have non-NIL results, they are done and we can use
        the results
 
        if child node is NIL:
     2. ensure that child nodes have the trace set correctly
     3. call child eval()

     then do our own evaluation, setting our own result

     To make this work, exec must ensure root node sets trace before calling
     its eval.
  *)
  
  Scale = Scalar BRANDED Brand & " Scale" OBJECT
  OVERRIDES
    eval := EvalScale;
  END;

  Pickle = PublicPickle BRANDED Brand & " Pickle" OBJECT
  END;

PROCEDURE GetWorkingSpace(t : Array) : REF ARRAY OF LONGREAL =
  BEGIN
    RETURN NEW(REF ARRAY OF LONGREAL, t.trace.getSteps())
  END GetWorkingSpace;

PROCEDURE Exec(t : Array; trace : Trace.T; VAR result : ARRAY OF LONGREAL)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    t.trace := trace;
    t.eval();
    result := t.result^
  END Exec;

(**********************************************************************)
  
REVEAL
  GetNode = (Array OBJECT nodeid : NodeId; END)
                BRANDED Brand & " GetNode" OBJECT
  OVERRIDES
    eval := EvalGetNode;
  END;

PROCEDURE EvalGetNode(self : GetNode)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    self.result := self.getWorkingSpace();
    self.trace.getNodeData(self.nodeid, self.result^)
  END EvalGetNode;
    
(**********************************************************************)

REVEAL
  Unary = (Array OBJECT a : Array; END) BRANDED Brand & " Unary" OBJECT
  OVERRIDES
    eval := EvalUnary;
  END;

  Func = (Unary OBJECT f : PROCEDURE(x : LONGREAL) : LONGREAL; END)
            BRANDED Brand & " Func" OBJECT
  OVERRIDES
    eval := EvalFunc;
  END;

  LrFunc = (Unary OBJECT f : LRFunction.T; END)
            BRANDED Brand & " LrFunc" OBJECT
  OVERRIDES
    eval := EvalLrFunc;
  END;

  Integrate = (Unary OBJECT f : PROCEDURE(x : LONGREAL) : LONGREAL; END)
            BRANDED Brand & " Integrate" OBJECT
  OVERRIDES
    eval := EvalIntegrate;
  END;

PROCEDURE EvalUnary(self : Unary)  
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    self.result := self.getWorkingSpace();
    IF self.a.result = NIL THEN
      self.a.trace := self.trace;
      self.a.eval()
    END
  END EvalUnary;

PROCEDURE EvalFunc(self       : Func) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    Unary.eval(self);
    FOR i := FIRST(self.result^) TO LAST(self.result^) DO
      self.result[i] := self.f(self.a.result[i])
    END
  END EvalFunc;
  
PROCEDURE EvalLrFunc(self       : LrFunc) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  <*FATAL MapError.E*>
  BEGIN
    Unary.eval(self);
    FOR i := FIRST(self.result^) TO LAST(self.result^) DO
      self.result[i] := self.f.eval(self.a.result[i])
    END
  END EvalLrFunc;
  
PROCEDURE EvalIntegrate(self       : Integrate) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    Unary.eval(self);

    self.result[0] := 0.0d0;
    
    WITH time = self.trace.sharedTime() DO
      FOR i := FIRST(time^) + 1 TO LAST(time^) DO
        WITH prev  = self.result[i - 1],
             prevA = self.a.result[i - 1],
             currA = self.a.result[i],
             
             midA  = 0.5d0 * (prevA + currA), (* midpoint rule *)
             ts    = time[i] - time[i - 1],

             dI    = midA * ts DO
          self.result[i] := prev + dI
        END
      END
    END
  END EvalIntegrate;
  
(**********************************************************************)

REVEAL
  Binary = (Array OBJECT a, b : Array; END) BRANDED Brand & " Binary" OBJECT
  OVERRIDES
    eval := EvalBinary;
  END;
  
  Plus = Binary BRANDED Brand & " Plus" OBJECT
  OVERRIDES
    eval := EvalPlus;
  END;

  Times = Binary BRANDED Brand & " Times" OBJECT
  OVERRIDES
    eval := EvalTimes;
  END;

  Divide = Binary BRANDED Brand & " Divide" OBJECT
  OVERRIDES
    eval := EvalDivide;
  END;

PROCEDURE EvalBinary(self : Binary) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    self.result := self.getWorkingSpace();
    IF self.a.result = NIL THEN
      self.a.trace := self.trace;
      self.a.eval()
    END;
    IF self.b.result = NIL THEN
      self.b.trace := self.trace;
      self.b.eval()
    END
  END EvalBinary;
  
PROCEDURE EvalPlus(self       : Plus)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    Binary.eval(self);
    FOR i := FIRST(self.result^) TO LAST(self.result^) DO
      self.result[i] := self.a.result[i] + self.b.result[i]
    END
  END EvalPlus;

PROCEDURE EvalTimes(self       : Times) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    Binary.eval(self);
    FOR i := FIRST(self.result^) TO LAST(self.result^) DO
      self.result[i] := self.a.result[i] * self.b.result[i]
    END
  END EvalTimes;
  
PROCEDURE EvalDivide(self       : Divide) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    Binary.eval(self);
    FOR i := FIRST(self.result^) TO LAST(self.result^) DO
      self.result[i] := self.a.result[i] / self.b.result[i]
    END
  END EvalDivide;

  (**********************************************************************)

PROCEDURE EvalScale(self       : Scale) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    Unary.eval(self);
    FOR i := FIRST(self.result^) TO LAST(self.result^) DO
      self.result[i] := self.scalar * self.a.result[i]
    END
  END EvalScale;

  (**********************************************************************)

PROCEDURE MakeGetNode(nodeid : NodeId) : GetNode =
  BEGIN
    RETURN NEW(GetNode, nodeid := nodeid)
  END MakeGetNode;

PROCEDURE MakeFunc(f : LRFunction.T) : LrFunc =
  BEGIN
    RETURN NEW(LrFunc, f := f)
  END MakeFunc;

PROCEDURE MakePlus(a, b : Array) : Plus =
  BEGIN
    RETURN NEW(Plus, a := a, b := b)
  END MakePlus;

PROCEDURE MakeTimes(a, b : Array) : Times =
  BEGIN
    RETURN NEW(Times, a := a, b := b)
  END MakeTimes;

PROCEDURE MakeDivide(a, b : Array) : Divide =
  BEGIN
    RETURN NEW(Divide, a := a, b := b)
  END MakeDivide;

PROCEDURE MakeScale(a : Array; scalar : LONGREAL) : Scale =
  BEGIN
    RETURN NEW(Scale, a := a, scalar := scalar)
  END MakeScale;

BEGIN END TraceOp.
