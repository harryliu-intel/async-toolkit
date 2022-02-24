%source liberty.t liberty.y
%import libertyLexStd libertyParse

%module {
IMPORT LibertyAttrVal;
IMPORT LibertyAttrValExpr;
IMPORT LibertyBoolean;
IMPORT LibertyExpr;
IMPORT LibertySorI;
IMPORT LibertyDefine;
IMPORT LibertyDefineGroup;
IMPORT LibertyParamList;
IMPORT LibertyHead;
IMPORT LibertyGroup;
IMPORT LibertyStatementSeq;
IMPORT LibertySimpleAttr;
IMPORT LibertyComplexAttr;
}

%interface {
IMPORT LibertyAttrVal;
IMPORT LibertyAttrValExpr;
IMPORT LibertyExpr;
IMPORT LibertySorI;
IMPORT LibertyDefine;
IMPORT LibertyDefineGroup;
IMPORT LibertyParamList;
IMPORT LibertyHead;
IMPORT LibertyGroup;
IMPORT LibertyStatementSeq;
IMPORT LibertyStatement;
IMPORT LibertySimpleAttr;
IMPORT LibertyComplexAttr;
}

%public {
  val : LibertyGroup.T
}

file: { val : LibertyGroup.T }
  group { $$ := $1; self.val := $1 }

group: { val : LibertyGroup.T }
  nonempty { $$ := NEW(LibertyGroup.T,
                       head := $1,
                       statements := $2) }
                       
  empty    { $$ := NEW(LibertyGroup.T,
                       head := $1,
                       statements := NEW(LibertyStatementSeq.T).init()) }

statements: { val : LibertyStatementSeq.T }
  x        { WITH res = NEW(LibertyStatementSeq.T).init() DO
               <*ASSERT $1 # NIL*>
               res.addhi($1);
               $$ := res
             END
           }
  cons     {
              <*ASSERT $2 # NIL*>
              $1.val.addhi($2);
              $$ := $1
           }

statement: { val : LibertyStatement.T }
  simple_attr  { <*ASSERT $1 # NIL*> $$ := $1 }
  complex_attr { <*ASSERT $1 # NIL*> $$ := $1 }
  define       { <*ASSERT $1 # NIL*> $$ := $1 }
  define_group { <*ASSERT $1 # NIL*> $$ := $1 }
  group        { <*ASSERT $1 # NIL*> $$ := $1 }

simple_attr: { val : LibertySimpleAttr.T }
  colonsemi { $$ := NEW(LibertySimpleAttr.T,
                        ident := $1,
                        attrValExpr := $2,
                        syntax := LibertySimpleAttr.Syntax.ColonSemi)
            }
  colon     { $$ := NEW(LibertySimpleAttr.T,
                        ident := $1,
                        attrValExpr := $2,
                        syntax := LibertySimpleAttr.Syntax.Colon)
            }
  eq        { $$ := NEW(LibertySimpleAttr.T,
                        ident := $1,
                        attrValExpr := $2,
                        syntax := LibertySimpleAttr.Syntax.Eq)
            }

complex_attr: { val : LibertyComplexAttr.T }
  headsemi  { $$ := NEW(LibertyComplexAttr.T, head := $1, semi := TRUE) }
  head      { $$ := NEW(LibertyComplexAttr.T, head := $1, semi := FALSE) }

head: { val : LibertyHead.T }
  x     { $$ := LibertyHead.New($1, $2) }
  empty { $$ := LibertyHead.New($1, LibertyParamList.New()) }

param_list: { val : LibertyParamList.T }
  x { WITH res = LibertyParamList.New() DO
        res.params.addhi($1);
        $$ := res
      END
    }
  commacons { $1.val.params.addhi($2); $1.val.sep := ","; $$ := $1 }
  cons      { $1.val.params.addhi($2); $1.val.sep := ""; $$ := $1 }

define: { val : LibertyDefine.T }
  x     { $$ := NEW(LibertyDefine.T,
                    s := ARRAY [0..2] OF LibertySorI.T { $1, $2, $3 }) }

define_group: { val : LibertyDefineGroup.T }
  x     { $$ := NEW(LibertyDefineGroup.T,
                    s := ARRAY [0..1] OF LibertySorI.T { $1, $2 }) }

s_or_i: { val : LibertySorI.T }
  string { $$ := NEW(LibertySorI.String, val := $1) }
  ident  { $$ := NEW(LibertySorI.Ident,  val := $1) }

attr_val: { val : LibertyAttrVal.T }
  num { $$ := NEW(LibertyAttrVal.Num, val := $1) }
  s_or_i { $$ := NEW(LibertyAttrVal.SorI, val := $1) }
  colon { $$ := NEW(LibertyAttrVal.Colon, x := $1, y := $2) }
  true { $$ := NEW(LibertyAttrVal.Boolean,
                   val := NEW(LibertyBoolean.T, val := TRUE)) }
  false { $$ := NEW(LibertyAttrVal.Boolean,
                   val := NEW(LibertyBoolean.T, val := FALSE)) }

attr_val_expr: {val : LibertyAttrValExpr.T }
  string { $$ := NEW(LibertyAttrValExpr.String, val := $1) }
  true { $$ := NEW(LibertyAttrValExpr.Boolean,
                   val := NEW(LibertyBoolean.T, val := TRUE)) }
  false { $$ := NEW(LibertyAttrValExpr.Boolean,
                   val := NEW(LibertyBoolean.T, val := FALSE)) }
  expr { $$ := NEW(LibertyAttrValExpr.Expr, val := $1) }

expr: { val : LibertyExpr.T }
  plus    { $$ := LibertyExpr.Plus($1, $2) }
  minus   { $$ := LibertyExpr.Minus($1, $2) }
  times   { $$ := LibertyExpr.Times($1, $2) }
  div     { $$ := LibertyExpr.Div($1, $2) }
  paren   { $$ := $1 }
  uminus  { $$ := LibertyExpr.Uminus($1) }
  uplus   { $$ := LibertyExpr.Uplus($1) }
  num     { $$ := LibertyExpr.Num($1) }
  ident   { $$ := LibertyExpr.Ident($1) }