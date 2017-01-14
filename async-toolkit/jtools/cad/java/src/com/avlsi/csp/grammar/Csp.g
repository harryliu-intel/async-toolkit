//
// Copyright 2001 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

// authors: Jesse Rosenstock, Tim Crowder
// version: $Revision$ $Date$

header {
    package com.avlsi.csp.grammar;

    import java.math.BigInteger;

    import com.avlsi.util.container.Pair;
    import com.avlsi.cast2.impl.CastTwoUtil;
    import com.avlsi.csp.ast.*;
}

class CspParser extends Parser;
options {
    buildAST = false;
    k = 3;
    defaultErrorHandler = false;
}
{
    private antlr.TokenStreamSelector selector = null;

    public void setSelector(final antlr.TokenStreamSelector selector) {
        this.selector = selector;
    }

    private IntegerExpression decodeInt(final String s) {
        // XXX: catch exceptions, mapping to something or other
        if (s.startsWith("0x"))
            return new IntegerExpression(s.substring(2), 16);
        else if (s.startsWith("0b"))
            return new IntegerExpression(s.substring(2), 2);
        else {
            final int underIdx = s.indexOf('_');
            if (underIdx == -1)
                return new IntegerExpression(s, 10);
            else {
                final String radixString = s.substring(0, underIdx);
                final int radix = Integer.parseInt(radixString);
                final String valString = s.substring(underIdx + 1);

                return new IntegerExpression(valString, radix);
            }
        }
    }
    private IntegerExpression trueExpression() {
        return new BooleanExpression(true);
    }
    private IntegerExpression falseExpression() {
        return new BooleanExpression(false);
    }
    private StatementInterface waitStatement(final ExpressionInterface e) {
        final DeterministicSelectionStatement stmt =
            new DeterministicSelectionStatement();
        final SkipStatement skip =
            new SkipStatement();
        stmt.epr(e);
        skip.epr(e);
        stmt.addGuardedCommand(new GuardedCommand(e, skip));
        return stmt;
    }
    private StatementInterface infiniteLoop(final StatementInterface s) {
        final DeterministicRepetitionStatement stmt =
            new DeterministicRepetitionStatement();
        final IntegerExpression true_exp = trueExpression();
        stmt.epr(s);
        true_exp.epr(s);
        stmt.addGuardedCommand(new GuardedCommand(true_exp, s));
        return stmt;
    }
}
// ANTLR only expects EOFs after "start rules", which are rules
// that are not referred to by any in the grammar.  If we don't
// have this rule, EOF will break us!
program returns [Pair pair = null]
    { FunctionDeclaration funcDecl; StatementInterface s;
      StructureDeclaration structDecl;
      CSPProgram prog = new CSPProgram(); }
    : ( funcDecl=function_decl {prog.addFunctionDeclaration(funcDecl);
                                prog.epr(funcDecl);}
      | structDecl=structure_decl {prog.addStructureDeclaration(structDecl);
                                   prog.epr(structDecl);}
      )*
      ( s=sequential_statement {prog.setStatement(s);
                                prog.epr(s);} )?
      ( DIRECTIVES l:LCURLY {l.setText(CastTwoUtil.getDumbBlockString(selector,
                                                                      this));})?
      EOF
    { pair = new Pair(prog, l); }
    ;
function_decl returns [FunctionDeclaration funcDecl = null]
    { DeclarationList decList = null; Type t = null; StatementInterface s; }
    : func:FUNCTION id:IDENT paren:LPAREN
      ( decList=declaration_list[true]
      | /*empty*/ {decList = new DeclarationList(); 
                   decList.epr(paren);}
      ) RPAREN ( COLON t=type )? ASSIGN s=statement semi:SEMI
    { funcDecl = new FunctionDeclaration(id.getText(), decList, t, s); 
      funcDecl.epr(func, semi); }
    ;
var_statement returns [VarStatement varStmt = null]
    { StatementInterface stmt = null; 
      DeclarationList decList = null; 
      Declaration dec = null; }
    : dec=declaration[false]
      { varStmt = new VarStatement (dec);
        varStmt.epr(dec); }
    ;
loop_statement returns [StatementInterface stmt = null]
    { Range r; StatementInterface s; int sep = -1; Token t=null; }
/* XXX: not sure this will work --frederik */
    : (la0:LANGL {sep = LoopStatement.SEQUENTIAL; t=la0; }
      |la1:SLOOP {sep = LoopStatement.SEQUENTIAL; t=la1; }
      |la2:BLOOP {sep = LoopStatement.PARALLEL; t=la2; }
      |la3:CLOOP {sep = LoopStatement.PARALLEL; t=la3; })
      id:IDENT COLON r=range COLON LPAREN s=sequential_statement RPAREN ra:RANGL
    { stmt = new LoopStatement(id.getText(), r, sep, s);
      stmt.epr(t,ra); }
    ;
sequential_part returns [StatementInterface stmt]
    : ( var_statement ) => stmt=var_statement
    | stmt=parallel_statement
    ;
sequential_statement returns [SequentialStatement seqStmt]
    { seqStmt = new SequentialStatement();
      StatementInterface stmt; }
    // Interspersed var statements and parallel statements.  We used to require
    // that every var statement must be followed by a parallel statement, and
    // at least one parallel statement must occur.
    : stmt=sequential_part {seqStmt.addStatement(stmt); seqStmt.epr(stmt);}
      ( options { warnWhenFollowAmbig = false; } :
        SEMI stmt=sequential_part {seqStmt.addStatement(stmt);
                                   seqStmt.epr(stmt);}
      )*
      ( SEMI )?
    ;
parallel_statement returns [StatementInterface stmt]
    { ParallelStatement parStmt = null; }
    : stmt=statement
      (COMMA
       { if (parStmt == null) {
           parStmt = new ParallelStatement();
           parStmt.addStatement(stmt);
           parStmt.epr(stmt);
         }
       }
       stmt=statement { parStmt.addStatement(stmt);
                        parStmt.epr(stmt); }
      )*
      { if (parStmt != null)
          stmt = parStmt;
      }
    ;
statement returns [StatementInterface stmt = null]
    { ExpressionInterface e; }
    : lp:LPAREN stmt=sequential_statement rp:RPAREN 
        { stmt.epr(lp,rp); }
    | (LBRACK | ( HASH LBRACK ) ) => stmt=selection_statement
    | stmt=repetition_statement
    | (lvalue
        (  ASSIGN | PASSIGN | MASSIGN | TASSIGN |  XASSIGN |
          DASSIGN | RASSIGN | AASSIGN | OASSIGN | LSASSIGN | RSASSIGN )
      ) => stmt=assignment_statement
    | (lvalue ( INC | DEC ) ) => stmt=incdec_statement
    | (HASH | ( lvalue ( QUEST | BANG ) ) ) => stmt=communication_statement
    | (lvalue ( PLUS | MINUS ) ) => stmt=bool_assignment_statement
    | e=lvalue { stmt=new ExpressionStatement(e); stmt.epr(e); }
    | stmt=loop_statement
    | err:ERROR {stmt = new ErrorStatement(getFilename(), err.getLine(),
                                           err.getColumn());
                 stmt.epr(err); }
    | sk:SKIP {stmt = new SkipStatement();
               stmt.epr(sk); }
    ;
selection_statement returns [StatementInterface stmt = null]
    { ExpressionInterface e; AbstractGuardedStatement g = null; }
    : lb:LBRACK ( ( (expression (AT | ARROW)) | CLLOOP | BOXLOOP )
                 => stmt=guard_commands[false]
             | e=expression {stmt = waitStatement(e);}
             ) rb:RBRACK {stmt.epr(lb,rb);}
    | ha:HASH LBRACK {g=new DeterministicSelectionStatement();}
      det_guard_noelse_commands[g] rb1:RBRACK
      {g.addElseStatement((StatementInterface) new SkipStatement().epr(ha));
       stmt = g; stmt.epr(ha,rb1);}
    ;
repetition_statement returns [StatementInterface stmt = null]
    { StatementInterface s; }
    : lo:LOOP
      ( ( (expression ARROW) | CLLOOP | BOXLOOP ) => stmt=guard_commands[true]
      | s=sequential_statement {stmt = infiniteLoop(s);})
      rb:RBRACK {stmt.epr(lo,rb);}
    ;
assignment_statement returns [StatementInterface stmt = null]
    { ExpressionInterface e1, e2; char kind = AssignmentStatement.EQUAL; }
    : e1=lvalue
      ( ASSIGN   { kind = AssignmentStatement.EQUAL;      }
      | PASSIGN  { kind = AssignmentStatement.ADD;        }
      | MASSIGN  { kind = AssignmentStatement.SUBTRACT;   }
      | TASSIGN  { kind = AssignmentStatement.MULTIPLY;   }
      | DASSIGN  { kind = AssignmentStatement.DIVIDE;     }
      | RASSIGN  { kind = AssignmentStatement.REMAINDER;  }
      | AASSIGN  { kind = AssignmentStatement.AND;        }
      | OASSIGN  { kind = AssignmentStatement.OR;         }
      | XASSIGN  { kind = AssignmentStatement.XOR;        }
      | LSASSIGN { kind = AssignmentStatement.LEFTSHIFT;  }
      | RSASSIGN { kind = AssignmentStatement.RIGHTSHIFT; }
      )
      e2=expression
      {stmt = new AssignmentStatement(e1, e2, kind); stmt.epr(e1,e2);}
    ;
incdec_statement returns [StatementInterface stmt = null]
    { ExpressionInterface e1; boolean inc = false; }
    : e1=lvalue ( i:INC { inc = true;  e1.epr(i); }
                | d:DEC { inc = false; e1.epr(d); } )
      {stmt = new IncDecStatement(e1, inc); stmt.epr(e1); }
    ;
bool_assignment_statement returns [StatementInterface stmt = null]
    { ExpressionInterface e1, e2 = null; }
    : e1=lvalue ( p:PLUS { e2 = trueExpression(); e2.epr(p); }
                | m:MINUS { e2 = falseExpression(); e2.epr(m); })
    {stmt = new AssignmentStatement(e1, e2); stmt.epr(e1, e2);}
    ;
communication_statement returns [StatementInterface stmt = null]
    { ExpressionInterface e1, e2=null;
      boolean peekP = false;}
    : ha:HASH e1=lvalue QUEST e2=lvalue
        {stmt = new AssignmentStatement(e2, 
                (PeekExpression)new PeekExpression(e1).epr(e1));
         stmt.epr(ha,e2);}
    | e1=lvalue
                               // note that e2 may be null for recv
      ( q:QUEST (e2=lvalue)? {stmt = new ReceiveStatement(e1, e2); 
                              stmt.epr(e1,q); stmt.epr(e2);}
      | b:BANG (e2=expression)? {
                stmt = new SendStatement(e1,
                    e2 != null ? e2
                : (IntegerExpression)falseExpression().epr(b));
                stmt.epr(e1,b); stmt.epr(e2); }
      )
    ;
guard_commands[boolean repetitionP] returns [StatementInterface stmt = null]
    : ( CLLOOP
      | expression ( AT | ARROW sequential_statement COLON ) ) =>
        stmt=non_det_guard_commands[repetitionP]
    | stmt=det_guard_commands[repetitionP]
    ;
det_guard_commands[boolean repetitionP]
returns [AbstractGuardedStatement stmt]
    { StatementInterface s;
      stmt = repetitionP
          ? (AbstractGuardedStatement)
              new DeterministicRepetitionStatement()
          : (AbstractGuardedStatement)
              new DeterministicSelectionStatement(); }
    : det_guard_noelse_commands[stmt]
      (BOX s=guard_else_command {stmt.addElseStatement(s); 
                                 stmt.epr(s);})?
    ;
det_guard_noelse_commands[AbstractGuardedStatement stmt]
    { GuardedCommandInterface g; }
    : g=det_guard_command {stmt.addGuardedCommand(g); stmt.epr(g);}
      (BOX g=det_guard_command {stmt.addGuardedCommand(g); stmt.epr(g);})*
    ;
non_det_guard_linked[boolean repetitionP, AbstractGuardedStatement stmt]
    { GuardedCommandInterface g; LinkageTerms neutralTerms = null; }
    : g=linked_guard_command {stmt.addGuardedCommand(g);
                              stmt.epr(g);}
      (COLON g=linked_guard_command {stmt.addGuardedCommand(g);
                                     stmt.epr(g);})*
      COLON neutralTerms=linkage_specifier
      { if (repetitionP)
            ((NonDeterministicRepetitionStatement) stmt)
                .setNeutralState(neutralTerms);
        else
            ((NonDeterministicSelectionStatement) stmt)
                .setNeutralState(neutralTerms);
        stmt.epr(neutralTerms);
      }
    ;
non_det_guard_simple[boolean repetitionP, AbstractGuardedStatement stmt]
    { GuardedCommandInterface g; }
    : g=non_det_guard_command {stmt.addGuardedCommand(g); stmt.epr(g);}
      (COLON g=non_det_guard_command {stmt.addGuardedCommand(g); stmt.epr(g);})*
    ;
non_det_guard_commands[boolean repetitionP]
returns [AbstractGuardedStatement stmt]
    { stmt = repetitionP
          ? (AbstractGuardedStatement)
              new NonDeterministicRepetitionStatement()
          : (AbstractGuardedStatement)
              new NonDeterministicSelectionStatement(); }
    : ( non_det_guard_simple[repetitionP, stmt] ) =>
        non_det_guard_simple[repetitionP, stmt]
    | non_det_guard_linked[repetitionP, stmt]
    ;
guard_command_simple returns [GuardedCommandInterface guardedCommand = null]
    { ExpressionInterface expr;
      SequentialStatement seqStmt; }
    : expr=expression ARROW seqStmt=sequential_statement
    { guardedCommand = new GuardedCommand(expr, seqStmt);
      guardedCommand.epr(expr);
      guardedCommand.epr(seqStmt);}
    ;
det_guard_command returns [GuardedCommandInterface guardedCommand = null]
    { GuardedCommandInterface g;
      LoopGuard loop = null;
      Range r; }
    : guardedCommand=guard_command_simple
    | la:BOXLOOP bid:IDENT COLON r=range COLON LPAREN
      {loop = new LoopGuard(bid.getText(), r, LoopGuard.BOX);}
      g=det_guard_command {loop.addGuard(g);}
      (BOX g=det_guard_command {loop.addGuard(g);})*
      RPAREN ra:RANGL
    { guardedCommand = loop; guardedCommand.epr(la,ra); }
    ;
non_det_guard_command returns [GuardedCommandInterface guardedCommand = null]
    { GuardedCommandInterface g;
      LoopGuard loop = null;
      Range r; }
    : guardedCommand=guard_command_simple
    | lb:CLLOOP cid:IDENT COLON r=range COLON LPAREN
      {loop = new LoopGuard(cid.getText(), r, LoopGuard.COLON);}
      g=non_det_guard_command {loop.addGuard(g);}
      (COLON g=non_det_guard_command {loop.addGuard(g);})*
      RPAREN rb:RANGL
    { guardedCommand = loop; guardedCommand.epr(lb,rb); }
    ;
linked_guard_command returns [GuardedCommandInterface guardedCommand = null]
    { ExpressionInterface guardExpr;
      LinkageTerms linkTerms;
      SequentialStatement seqStmt;
      GuardedCommandInterface g;
      LoopGuard loop = null;
      Range r; }
    : guardExpr=expression linkTerms=linkage_specifier
      ARROW seqStmt=sequential_statement
    { guardedCommand = new GuardedCommand(guardExpr, linkTerms, seqStmt); 
      guardedCommand.epr(guardExpr,seqStmt); }
    | lb:CLLOOP cid:IDENT COLON r=range COLON LPAREN
      {loop = new LoopGuard(cid.getText(), r, LoopGuard.COLON);}
      g=linked_guard_command {loop.addGuard(g);}
      (COLON g=linked_guard_command {loop.addGuard(g);})*
      RPAREN rb:RANGL
    { guardedCommand = loop; guardedCommand.epr(lb,rb); }
    ;
linkage_specifier returns [LinkageTerms terms]
    : at:AT LPAREN terms=linkage_terms rp:RPAREN
      { terms.epr(at,rp); }
    ;
guard_else_command returns [SequentialStatement stmt]
    : el:ELSE ARROW stmt=sequential_statement
      { stmt.epr(el,stmt); }
    ;
linkage_terms returns [LinkageTerms terms = new LinkageTerms()]
    { LinkageTermInterface term; }
    : term=linkage_term {terms.addTerm(term); 
                         terms.epr(term); }
      ( COMMA term=linkage_term {terms.addTerm(term); 
                                 terms.epr(term); } )*
    ;
linkage_term returns [LinkageTermInterface term = null]
    { boolean isInverted = false;
      LinkageExpressionInterface expr = null;
      ExpressionInterface e;
      Range r; }
    : ( ti:TILDE { isInverted = true; } )?
      id1:IDENT { expr = new LinkageIdentifierExpression(id1.getText());
                  expr.epr(ti,id1); }
      ( DOT id2:IDENT
        { expr = (LinkageExpressionInterface)
                (new LinkageStructureAccessExpression(expr, id2.getText()))
                .epr(expr,id2); }
      | DOT id4:INTEGER
        { expr = (LinkageExpressionInterface)
                (new LinkageStructureAccessExpression(expr, id4.getText()))
                .epr(expr,id4); }
      | LBRACK e=expression
        { expr = (LinkageExpressionInterface)
                (new LinkageArrayAccessExpression(expr, e))
                .epr(expr,e); }
        ( COMMA e=expression
          { expr = (LinkageExpressionInterface)
                    (new LinkageArrayAccessExpression(expr, e))
                    .epr(expr,e); } )*
        rb:RBRACK {expr.epr(rb);}
      )*
      { term = new LinkageExpressionTerm(expr, isInverted);
        term.epr(expr); }
    | cl:CLOOP id3:IDENT COLON
      r=range COLON
      ( term=linkage_term | LPAREN term=linkage_term RPAREN ) ra:RANGL
      { term = new LinkageLoopTerm(id3.getText(), r, term);
        term.epr(cl,ra); }
    ;
expression returns [ExpressionInterface expr]
    : expr=conditional_or_expression
    ;
loop_expression returns [ExpressionInterface expr = null]
    { Range r; ExpressionInterface e; int sep = -1; Token t=null; }
    : (lo0:PLOOP {sep = LoopExpression.PLUS; t=lo0; }
      |lo1:MLOOP {sep = LoopExpression.TIMES; t=lo1; }
      |lo2:ALOOP {sep = LoopExpression.AND; t=lo2; }
      |lo3:OLOOP {sep = LoopExpression.OR; t=lo3; }
      |lo4:XLOOP {sep = LoopExpression.XOR; t=lo4; })
      id:IDENT COLON r=range COLON LPAREN e=expression RPAREN ra:RANGL
    { expr = new LoopExpression(id.getText(), r, sep, e);
      expr.epr(t,ra); }
    ;
conditional_or_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=conditional_and_expression
      (PIPE2 e2=conditional_and_expression
            {expr = (ExpressionInterface) 
                new ConditionalOrExpression(expr, e2).epr(expr,e2);})*
    ;
conditional_and_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=or_expression
      (AMP2 e2=or_expression
            {expr = (ExpressionInterface)
                new ConditionalAndExpression(expr, e2).epr(expr,e2);})*
    ;
or_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=xor_expression
      (PIPE e2=xor_expression 
            {expr = (ExpressionInterface) 
                new OrExpression(expr, e2).epr(expr,e2);})*
    ;
xor_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=and_expression
      (CARET e2=and_expression 
            {expr = (ExpressionInterface) 
                new XorExpression(expr, e2).epr(expr,e2);})*
    ;
and_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=equal_expression
      (AMP e2=equal_expression 
            {expr = (ExpressionInterface)
                new AndExpression(expr, e2).epr(expr,e2);})*
    ;
equal_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=rel_expression
      (NEQ e2=rel_expression
            {expr = (ExpressionInterface)
                new InequalityExpression(expr, e2).epr(expr,e2); }
      |EQUAL e2=rel_expression
            {expr = (ExpressionInterface)
                new EqualityExpression(expr, e2).epr(expr,e2); }
      )*
    ;
rel_expression returns [ExpressionInterface expr]
    // the RANGL causes nondeterminism, which way is it resolved?
    { ExpressionInterface e2; }
    : expr=shift_expression
      (LEQ e2=shift_expression
            {expr = (ExpressionInterface)
                new LessEqualExpression(expr, e2).epr(expr,e2);}
      |GEQ e2=shift_expression
            {expr = (ExpressionInterface)
                new GreaterEqualExpression(expr, e2).epr(expr,e2);}
      |LANGL e2=shift_expression 
            {expr = (ExpressionInterface)
                new LessThanExpression(expr, e2).epr(expr,e2);}
      |RANGL e2=shift_expression 
            {expr = (ExpressionInterface)
                new GreaterThanExpression(expr, e2).epr(expr,e2);}
      )*
    ;
shift_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=add_expression
      (LSHIFT e2=add_expression 
            {expr = (ExpressionInterface)
                new LeftShiftExpression(expr, e2).epr(expr,e2); }
      |RSHIFT e2=add_expression 
            {expr = (ExpressionInterface)
                new RightShiftExpression(expr, e2).epr(expr,e2); }
      )*
    ;
add_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=mult_expression
      (PLUS e2=mult_expression
            {expr = (ExpressionInterface)
                new AddExpression(expr, e2).epr(expr,e2);}
      |MINUS e2=mult_expression
            {expr = (ExpressionInterface)
                new SubtractExpression(expr, e2).epr(expr,e2);}
      )*
    ;
mult_expression returns [ExpressionInterface expr]
    { ExpressionInterface e2; }
    : expr=unary_expression
      (STAR e2=unary_expression  
            {expr = (ExpressionInterface)
                new MultiplyExpression(expr, e2).epr(expr,e2);}
      |SLASH e2=unary_expression
            {expr = (ExpressionInterface)
                new DivideExpression(expr, e2).epr(expr,e2);}
      |CENT e2=unary_expression  
            {expr = (ExpressionInterface)
                new RemainderExpression(expr, e2).epr(expr,e2);}
      )*
    ;
unary_expression returns [ExpressionInterface expr]
    : TILDE expr=unary_expression
        {expr = (ExpressionInterface)
            new NotExpression(expr).epr(expr);}
    | MINUS expr=unary_expression
        {expr = (ExpressionInterface)
            new NegateExpression(expr).epr(expr);}
    | expr=exponential_expression
    ;
exponential_expression returns [ExpressionInterface expr]
    { ExpressionInterface e; }
    : expr=primary_expression
      ( EXP e=unary_expression
        {expr = (ExpressionInterface)
                new ExponentialExpression(expr, e).epr(expr,e);} )?
    ;
primary_expression returns [ExpressionInterface expr = null]
    : i:INTEGER {expr = decodeInt(i.getText()); expr.epr(i);}
    | tr:TRUE {expr = trueExpression(); expr.epr(tr);}
    | fa:FALSE {expr = falseExpression(); expr.epr(fa);}
    | s:STRING_LITERAL {expr = new StringExpression(s.getText()); expr.epr(s);}
    | LPAREN expr=expression RPAREN
    | (lvalue QUEST) => expr=communication_expression
    | expr=lvalue
    | expr=probe_expression
    | expr=loop_expression
    ;
probe_expression returns [ExpressionInterface expr]
    : ha:HASH expr=lvalue
      (q:QUEST {expr = (ExpressionInterface)
                new PeekExpression(expr).epr(ha,q);}
      |/*empty*/ {expr = (ExpressionInterface)
                new ProbeExpression(expr).epr(ha,expr);}
      )
    // | HASH lvalue ((COMMA lvalue)* COLON expression)?
    ;
communication_expression returns [ExpressionInterface expr]
    : expr=lvalue qu:QUEST
        {expr = (ExpressionInterface)
            new ReceiveExpression(expr).epr(expr,qu);}
    ;
range returns [Range r = null]
    { ExpressionInterface minExpr, maxExpr; }
    : minExpr=expression
      ( DOTDOT maxExpr=expression
            {r = new Range(minExpr, maxExpr); r.epr(minExpr,maxExpr);}
      | /*empty*/ { IntegerExpression i0=new IntegerExpression("0", 10);
                    IntegerExpression i1=new IntegerExpression("1", 10);
                    SubtractExpression s0=new SubtractExpression(minExpr,i1);
                    i0.epr(minExpr); i1.epr(i0); s0.epr(i0);
                    r = new Range(i0,s0); r.epr(minExpr);}
      )
    ;
identifier returns [IdentifierExpression ident = null]
    : id:IDENT {ident = new IdentifierExpression(id.getText());
                ident.epr(id);}
    ;
lvalue returns [ExpressionInterface expr = null]
    { ExpressionInterface e1, e2 = null;
      FunctionCallExpression funcCall = null; }
    : ( expr=identifier
      | st:STRING { expr=new IdentifierExpression(st.getText());
                    expr.epr(st);
                  }
      )
      (
                 // Array slicing has been removed until it is demanded.
        ( LBRACK // ((expression DOTDOT) => range
                 // | expression
                 // )
          e2=expression {expr = (ExpressionInterface)
                    new ArrayAccessExpression(expr, e2).epr(expr,e2);}
          ( COMMA e2=expression
            {expr = (ExpressionInterface)
                        new ArrayAccessExpression(expr, e2).epr(expr,e2);}
          )*
          rb:RBRACK {expr.epr(rb);}
        )
      | LCURLY e1=expression ( COLON e2=expression | /*empty*/ {e2=null;} )
        rc:RCURLY {expr = (ExpressionInterface)
                new BitRangeExpression(expr, e2, e1).epr(expr,rc);}
      | LPAREN {funcCall = new FunctionCallExpression(expr);}
          ( e2=expression {funcCall.addActual(e2);}
            ( COMMA e2=expression {funcCall.addActual(e2);} )* )?
        rp:RPAREN {expr = (ExpressionInterface)funcCall.epr(expr,rp);}
      | DOT id:IDENT
        {expr = (ExpressionInterface)
                new StructureAccessExpression(expr, id.getText()).epr(expr,id);}
      | DOT ie:INTEGER
        {expr = (ExpressionInterface)
                new StructureAccessExpression(expr, ie.getText()).epr(expr,ie);}
      | COLON2 mbr:IDENT
        {expr = (ExpressionInterface)
                new MemberAccessExpression(expr, mbr.getText()).epr(expr,mbr);}
      )*
    ;
declaration_list[boolean formalP] returns [DeclarationList declList]
    { declList = new DeclarationList (); 
      Declaration decl = null; }
    : decl=declaration[formalP] {declList.addDeclaration(decl);
                                 declList.epr(decl);}
      ( SEMI decl=declaration[formalP] {declList.addDeclaration(decl);
                                        declList.epr(decl);} )*
      ( SEMI )?
    ;
declaration[boolean formalP] returns [Declaration decl = null]
    { IdentifierList idList;
      DeclaratorList decList;
      Type t; }
//    : idList=identifier_list COLON t=type
//      { decl = new Declaration(idList, t); }
    : t=type decList=declarator_list[formalP]
      { decl = new Declaration(decList, t);
        decl.epr(t,decList); }
    ;
declarator_list[boolean formalP] returns [DeclaratorList decList]
    { decList = new DeclaratorList(); 
      Declarator dec = null; }
    : dec=declarator[formalP] {decList.addDeclarator(dec);
                               decList.epr(dec);}
      ( COMMA dec=declarator[formalP] {decList.addDeclarator(dec);
                                       decList.epr(dec);} )*
    ;

declarator[boolean formalP] returns [Declarator dec]
    { IdentifierExpression ident = null; 
      ExpressionInterface init = null;
      ArrayType s, t = null; 
      Range rng;
      int dir = Declarator.NONE;
      Token pm = null;
      dec = null; }
    : ( {formalP}? ( p:PLUS {dir=Declarator.OUT; pm = p;}
                     ( MINUS {dir=Declarator.INOUT;} )?
                   | m:MINUS {dir=Declarator.IN; pm = m;}
                     ( PLUS  {dir=Declarator.INOUT;} )? ) )?
      ident=identifier
        {dec = new Declarator(ident, null, null, dir); dec.epr(ident);
         if (pm != null) dec.epr(pm);}
      ( lb:LBRACK rng=range rb:RBRACK 
         { s=new ArrayType(rng, null); s.epr(lb,rb);
           if (t == null) {
               dec.setTypeFragment(s);
               dec.epr(s);
           } else {
               t.setElementType(s); 
               t.epr(s);
           } 
           t = s; 
         }
      )*
      ( ASSIGN init=expression {dec.setInitializer(init);
                                dec.epr(init);} )?
    ;
identifier_list returns [IdentifierList idList]
    { idList = new IdentifierList(); 
      IdentifierExpression ident = null; }
    : ident=identifier 
        {idList.addIdentifier(ident); idList.epr(ident);}
      ( COMMA ident=identifier 
            {idList.addIdentifier(ident); idList.epr(ident);} )*
    ;
type returns [Type t]
    { boolean constantP = false; 
      ExpressionInterface bits = null;
      t = null; }
    : (co:CONST {constantP = true;})? 
      ( to0:INT ( LPAREN bits=expression RPAREN )?
          {t = (Type)new IntegerType(constantP, bits).epr(co,to0);}
      | to4:SINT LPAREN bits=expression RPAREN
          {t = (Type)new IntegerType(constantP, true, bits).epr(co,to0);}
      | to1:BOOLEAN {t = (Type)new BooleanType(constantP).epr(co,to1);}
      | to2:BOOL {t = (Type)new BooleanType(constantP).epr(co,to2);}
      | to3:STRING {t = (Type)new StringType(constantP).epr(co,to3);}
      | id:IDENT {t = (Type)new StructureType(constantP, id.getText()).epr(co,id);} )
    ;
structure_decl returns [StructureDeclaration sd]
    { DeclarationList decl = null;
      sd = null; }
    : st:STRUCTURE id:IDENT ASSIGN LPAREN decl=declaration_list[false] RPAREN
      semi:SEMI
      { sd = new StructureDeclaration(id.getText(), decl); sd.epr(st, semi); }
    ;
class CspLexer extends Lexer;
options {
    k = 3;
    charVocabulary = '\3' .. '\377';  // needed for ~ to make sense
    testLiterals = false;
}
tokens {
    TRUE="true";
    FALSE="false";
    ERROR="error";
    SKIP="skip";
    ELSE="else";
    BOOLEAN="boolean";
    BOOL="bool";
    INT="int";
    SINT="sint";
    CONST="const";
    FUNCTION="function";
    DIRECTIVES="directives";
    STRUCTURE="structure";
    STRING="string";
}

{
    public Token makeToken(final int t) {
        final Token tok = super.makeToken(t);
        ((com.avlsi.cast.impl.TokenWithInfo) tok).setFilename(getFilename());
        return tok;
    }
}

WS : (' ' | '\t' | '\n' { newline(); } | '\r') { _ttype = Token.SKIP; } ;
protected ALPHA : (('a'..'z') | ('A'..'Z') | '_') ;
protected DIGIT : ('0'..'9') ;
protected HEX_DIGIT : DIGIT | 'a'..'f' | 'A'..'F' ;
protected BIN_DIGIT : '0' | '1';
IDENT
  options { testLiterals = true; }
  : ALPHA (ALPHA | DIGIT)* ;
INTEGER
    : (DIGIT)+ ( '_' (('a'..'z') | ('A'..'Z') | DIGIT)+ )?
    | "0x" ('_'!)? (HEX_DIGIT)+ ('_'! (HEX_DIGIT)+)*
    | "0b" ('_'!)? (BIN_DIGIT)+ ('_'! (BIN_DIGIT)+)*
    ;
LPAREN: '(' ;    RPAREN: ')' ;
LBRACK: '[' ;    RBRACK: ']' ;
LCURLY: '{' ;    RCURLY: '}' ;
BOX   :"[]" ;
LOOP  :"*[" ;
PLOOP :"<+" ;    MLOOP :"<*" ;    ALOOP :"<&" ;    OLOOP :"<|" ;
SLOOP :"<;" ;    BLOOP :"<||";    CLOOP :"<," ;    XLOOP :"<^" ;
BOXLOOP :"<[]";  CLLOOP: "<:";
LANGL : '<' ;    RANGL : '>' ;
LSHIFT:"<<" ;    RSHIFT:">>" ;
LEQ   :"<=" ;    GEQ   :">=" ;
ARROW : "->";
EQUAL : "==" ;
NEQ   :"!=" ;
ASSIGN: '=' ;    PASSIGN:"+=";    MASSIGN:"-=";    TASSIGN:"*=";
DASSIGN:"/=";    RASSIGN:"%=";    AASSIGN:"&=";    OASSIGN:"|=";
LSASSIGN:"<<=";  RSASSIGN:">>=";  XASSIGN:"^=";
INC   : "++";    DEC   : "--";
TILDE : '~' ;
STAR  : '*' ;    SLASH : '/' ;
CENT  : '%' ;
PLUS  : '+' ;    MINUS : '-' ;
EXP   : "**" ;
DOT   : '.' ;    COMMA : ',' ;
SEMI  : ';' ;    COLON : ':' ;
COLON2: "::";
DOTDOT: "..";
QUEST : '?' ;
BANG  : '!' ;
PIPE  : '|' ;
PIPE2 : "||";
AMP   : '&' ;
AMP2  : "&&";
CARET : "^" ;
HASH  : '#' ;
AT    : '@' ;
//PEEK  : "#?" ;

COMMENT_1
    : "/*"
       ( options { generateAmbigWarnings=false; }
       : { LA(2) != '/' }? '*'
       | '\n' { newline(); }
       | ~('*' | '\n')
       )*
      "*/"
    { $setType(Token.SKIP); }
    ;

COMMENT_2
    : "//" (~ '\n' )* '\n' { $setType(Token.SKIP); newline(); }
    ;

STRING_LITERAL
    : '"'! (ESC|'\n' { newline(); } | ~('"'|'\\'|'\n'))* '"'!
    ;

protected
ESC
    :   '\\'
        (   'n' { $setText("\n"); }
        |   'r' { $setText("\r"); }
        |   't' { $setText("\t"); }
        |   'b' { $setText("\b"); }
        |   'f' { $setText("\f"); }
        |   '"' { $setText("\""); }
        |   '\'' { $setText("'"); }
        |   '\\' { $setText("\\"); }
        |   'x' HEX_DIGIT HEX_DIGIT {
                String esc = $getText;
                char c[] = { (char) Integer.parseInt(esc.substring(2), 16) };
                String s = new String(c);
                $setText(s);
            }
        |   ('0'..'3')
            (
                options {
                    warnWhenFollowAmbig = false;
                }
            :   ('0'..'7')
                (   
                    options {
                        warnWhenFollowAmbig = false;
                    }
                :   '0'..'7'
                )?
            )? {
                String esc = $getText;
                char c[] = { (char) Integer.parseInt(esc.substring(1), 8) };
                String s = new String(c);
                $setText(s);
            }
        )
    ;
