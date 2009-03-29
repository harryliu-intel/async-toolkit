(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE AstToType;

IMPORT Atom, Wr;
IMPORT AST, ASTWalk, M3Context;
IMPORT Type;
IMPORT SchemePair;

TYPE
  Handle <: Public;
  Public = ASTWalk.Closure OBJECT
    wr: Wr.T;
    intf: Atom.T;
    context: M3Context.T;
  END;

PROCEDURE NewHandle(wr: Wr.T; intf: TEXT; c: M3Context.T): Handle;
(* Creates a new handle suitable for passing to "ASTWalk.VisitNodes",
   or directly to "Node", from another walk. The "callback" method
   is set to "Node". 
   If NetObj.T is not defined in context c, returns NIL *)
 
PROCEDURE OneStubScm(c: M3Context.T; qid: Type.Qid; wr: Wr.T): INTEGER;


VAR list : SchemePair.T := NIL;

END AstToType.
