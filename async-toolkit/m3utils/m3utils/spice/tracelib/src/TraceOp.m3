MODULE TraceOp;
IMPORT Trace;
IMPORT Rd;

REVEAL
  T = Public BRANDED Brand OBJECT
    trace : Trace.T;
  METHODS
    getWorkingSpace() : REF ARRAY OF LONGREAL := GetWorkingSpace;
  END;
  
  Plus = Binary BRANDED Brand & " Plus" OBJECT
  OVERRIDES
    exec := ExecPlus;
  END;

  Times = Binary BRANDED Brand & " Times" OBJECT
  OVERRIDES
    exec := ExecTimes;
  END;

  Divide = Binary BRANDED Brand & " Divide" OBJECT
  OVERRIDES
    exec := ExecDivide;
  END;

  Scale = Scalar BRANDED Brand & " Scale" OBJECT
  OVERRIDES
    exec := ExecScale;
  END;

  Func = (Unary OBJECT f : PROCEDURE(x : LONGREAL) : LONGREAL; END)
            BRANDED Brand & " Func" OBJECT
  OVERRIDES
    exec := ExecFunc;
  END;

PROCEDURE GetWorkingSpace(t : T) : REF ARRAY OF LONGREAL =
  BEGIN
    RETURN NEW(REF ARRAY OF LONGREAL, t.trace.getSteps())
  END GetWorkingSpace;

PROCEDURE ExecPlus(self       : Plus;
                   trace      : Trace.T;
                   VAR result : ARRAY OF LONGREAL)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    self.trace := trace;
    WITH a = self.getWorkingSpace(),
         b = self.getWorkingSpace() DO
      trace.getNodeData(self.a, a^);
      trace.getNodeData(self.b, b^);
      FOR i := FIRST(result) TO LAST(result) DO
        result[i] := a[i] + b[i]
      END
    END
  END ExecPlus;

PROCEDURE ExecTimes(self       : Times;
                    trace      : Trace.T;
                    VAR result : ARRAY OF LONGREAL) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    self.trace := trace;
    WITH a = self.getWorkingSpace(),
         b = self.getWorkingSpace() DO
      trace.getNodeData(self.a, a^);
      trace.getNodeData(self.b, b^);
      FOR i := FIRST(result) TO LAST(result) DO
        result[i] := a[i] * b[i]
      END
    END
  END ExecTimes;
  
PROCEDURE ExecDivide(self       : Divide;
                    trace      : Trace.T;
                    VAR result : ARRAY OF LONGREAL) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    self.trace := trace;
    WITH a = self.getWorkingSpace(),
         b = self.getWorkingSpace() DO
      trace.getNodeData(self.a, a^);
      trace.getNodeData(self.b, b^);
      FOR i := FIRST(result) TO LAST(result) DO
        result[i] := a[i] / b[i]
      END
    END
  END ExecDivide;
  
PROCEDURE ExecScale(self       : Scale;
                    trace      : Trace.T;
                    VAR result : ARRAY OF LONGREAL) =
  RAISES { Rd.EndOfFile, Rd.Failure } 
  BEGIN
    self.trace := trace;
    WITH a = self.getWorkingSpace()DO
      trace.getNodeData(self.a, a^);
      FOR i := FIRST(result) TO LAST(result) DO
        result[i] := self.scalar * a[i]
      END
    END
  END ExecScale;

PROCEDURE ExecFunc(self       : Func;
                    trace      : Trace.T;
                    VAR result : ARRAY OF LONGREAL) 
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    self.trace := trace;
    WITH a = self.getWorkingSpace() DO
      trace.getNodeData(self.a, a^);
      FOR i := FIRST(result) TO LAST(result) DO
        result[i] := self.f(a[i])
      END
    END
  END ExecFunc;
  
BEGIN END TraceOp.
