INTERFACE CspGuardedCommand;
IMPORT CspExpression;
IMPORT CspStatement;

TYPE
  T = OBJECT
    guard   : CspExpression.T;
    command : CspStatement.T;
  END;

CONST Brand = "CspGuardedCommand";

END CspGuardedCommand.
  
