package com.avlsi.csp.util;

import java.util.Iterator;
import java.util.Map;

import com.avlsi.csp.ast.*;

public class VisitorByCategory implements VisitorInterface {
    protected VisitorInterface getVisitor() {
        return this;
    }

    protected void
    processDeclarator(Declarator d) throws VisitorException {
        d.getIdentifier().accept(getVisitor());
        final Type t = d.getTypeFragment();
        if (t != null) t.accept(getVisitor());
        final ExpressionInterface init = d.getInitializer();
        if (init != null) init.accept(getVisitor());
    }

    protected void
    processDeclarationList(DeclarationList l) throws VisitorException {
        for (Iterator i = l.getDeclarations(); i.hasNext(); ) {
            final Declaration decl = (Declaration) i.next();
            final DeclaratorList dclr = decl.getDeclaratorList();
            for (Iterator j = dclr.getDeclarators(); j.hasNext(); ) {
                final Declarator d = (Declarator) j.next();
                processDeclarator(d);
            }
        }
    }
    protected void
    processFunctionDeclaration(FunctionDeclaration decl)
    throws VisitorException {
        processDeclarationList(decl.getFormals());
        final Type rtype = decl.getReturnType();
        if (rtype != null) rtype.accept(getVisitor());
        decl.getBodyStatement().accept(getVisitor());
    }
    protected void
    processStructureDeclaration(StructureDeclaration decl)
    throws VisitorException {
        processDeclarationList(decl.getDeclarations());
    }
    protected void
    processRefinementParent(CSPProgram p) throws VisitorException { }
    public void visitCSPProgram(CSPProgram p) throws VisitorException
    {
        for (Iterator i = p.getRefinementParents().iterator(); i.hasNext(); ) {
            processRefinementParent((CSPProgram) i.next());
        }
        for (Iterator i = p.getFunctionDeclarations(); i.hasNext(); ) {
            processFunctionDeclaration((FunctionDeclaration) i.next());
        }
        for (Iterator i = p.getStructureIterator(); i.hasNext(); ) {
            processStructureDeclaration((StructureDeclaration) i.next());
        }
        p.getInitializerStatement().accept(getVisitor());
        p.getStatement().accept(getVisitor());
    }

    /* AbstractBinaryExpression */
    protected void
    processAbstractBinaryExpression(final AbstractBinaryExpression e)
        throws VisitorException {
        e.getLeft().accept(getVisitor());
        e.getRight().accept(getVisitor());
    }
    protected void
    processArithmeticBinaryExpression(final AbstractBinaryExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitAddExpression(AddExpression e) throws VisitorException {
        processArithmeticBinaryExpression(e);
    }
    public void visitAndExpression(AndExpression e) throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitConditionalAndExpression(ConditionalAndExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitConditionalOrExpression(ConditionalOrExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitDivideExpression(DivideExpression e)
        throws VisitorException {
        processArithmeticBinaryExpression(e);
    }
    public void visitEqualityExpression(EqualityExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitExponentialExpression(ExponentialExpression e)
        throws VisitorException {
        processArithmeticBinaryExpression(e);
    }
    public void visitGreaterEqualExpression(GreaterEqualExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitGreaterThanExpression(GreaterThanExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitInequalityExpression(InequalityExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitLeftShiftExpression(LeftShiftExpression e)
        throws VisitorException {
        processArithmeticBinaryExpression(e);
    }
    public void visitLessEqualExpression(LessEqualExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitLessThanExpression(LessThanExpression e)
        throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitMultiplyExpression(MultiplyExpression e)
        throws VisitorException {
        processArithmeticBinaryExpression(e);
    }
    public void visitOrExpression(OrExpression e) throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitXorExpression(XorExpression e) throws VisitorException {
        processAbstractBinaryExpression(e);
    }
    public void visitRemainderExpression(RemainderExpression e)
        throws VisitorException {
        processArithmeticBinaryExpression(e);
    }
    public void visitRightShiftExpression(RightShiftExpression e)
        throws VisitorException {
        processArithmeticBinaryExpression(e);
    }
    public void visitSubtractExpression(SubtractExpression e)
        throws VisitorException {
        processArithmeticBinaryExpression(e);
    }

    /* AbstractChannelExpression */
    protected void
    processAbstractChannelExpression(final AbstractChannelExpression e)
    throws VisitorException {
        e.getChannelExpression().accept(getVisitor());
    }
    public void visitPeekExpression(PeekExpression e) throws VisitorException {
        processAbstractChannelExpression(e);
    }
    public void visitProbeExpression(ProbeExpression e)
        throws VisitorException {
        processAbstractChannelExpression(e);
    }
    public void visitReceiveExpression(ReceiveExpression e)
        throws VisitorException {
        processAbstractChannelExpression(e);
    }

    /* AbstractChannelStatement */
    protected void
    processAbstractChannelStatement(final AbstractChannelStatement s)
    throws VisitorException {
        s.getChannelExpression().accept(getVisitor());
        final ExpressionInterface rhs = s.getRightHandSide();
        if (rhs != null) rhs.accept(getVisitor());
    }
    public void visitReceiveStatement(ReceiveStatement s)
        throws VisitorException {
        processAbstractChannelStatement(s);
    }
    public void visitSendStatement(SendStatement s) throws VisitorException {
        processAbstractChannelStatement(s);
    }

    /* AbstractCompositeStatement */
    protected void
    processAbstractCompositeStatement(final AbstractCompositeStatement s)
    throws VisitorException {
        for (Iterator i = s.getStatements(); i.hasNext(); ) {
            ((StatementInterface) i.next()).accept(getVisitor());
        }
    }
    public void visitParallelStatement(ParallelStatement s)
        throws VisitorException {
        processAbstractCompositeStatement(s);
    }
    public void visitSequentialStatement(SequentialStatement s)
        throws VisitorException {
        processAbstractCompositeStatement(s);
    }

    /* AbstractUnaryExpression */
    protected void
    processAbstractUnaryExpression(final AbstractUnaryExpression e)
    throws VisitorException {
        e.getExpression().accept(getVisitor());
    }
    public void visitNegateExpression(NegateExpression e)
        throws VisitorException {
        processAbstractUnaryExpression(e);
    }
    public void visitNotExpression(NotExpression e) throws VisitorException {
        processAbstractUnaryExpression(e);
    }

    /* Type */
    protected void processType(final Type t) throws VisitorException { }
    public void visitArrayType(ArrayType t) throws VisitorException {
        processType(t);
    }
    public void visitBooleanType(BooleanType t) throws VisitorException {
        processType(t);
    }
    public void visitChannelType(ChannelType t) throws VisitorException {
        processType(t);
    }
    public void visitChannelStructureType(ChannelStructureType t)
        throws VisitorException {
        processType(t);
    }
    public void visitIntegerType(IntegerType t) throws VisitorException {
        processType(t);
    }
    public void visitNodeType(NodeType t) throws VisitorException {
        processType(t);
    }
    public void visitStringType(StringType t) throws VisitorException {}
    public void visitStructureType(StructureType t) throws VisitorException {
        processType(t);
    }

    /* Primary expressions */
    protected void processPrimaryExpression(ExpressionInterface e)
    throws VisitorException { }
    public void visitArrayAccessExpression(ArrayAccessExpression e)
        throws VisitorException {
        e.getArrayExpression().accept(getVisitor());
        e.getIndexExpression().accept(getVisitor());
    }
    public void visitBitRangeExpression(BitRangeExpression e)
        throws VisitorException {
        e.getBitsExpression().accept(getVisitor());
        if (e.getMinExpression() != null)
            e.getMinExpression().accept(getVisitor());
        e.getMaxExpression().accept(getVisitor());
    }
    public void visitFunctionCallExpression(FunctionCallExpression e)
        throws VisitorException {
        e.getFunctionExpression().accept(getVisitor());
        for (Iterator i = e.getActuals(); i.hasNext(); ) {
            ((ExpressionInterface) i.next()).accept(getVisitor());
        }
    }
    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException {
        processPrimaryExpression(e);
    }
    public void visitIntegerExpression(IntegerExpression e)
        throws VisitorException {
        processPrimaryExpression(e);
    }
    public void visitMemberAccessExpression(MemberAccessExpression e)
        throws VisitorException {
        e.getStructureExpression().accept(getVisitor());
    }
    public void visitStringExpression(StringExpression e)
        throws VisitorException {
        processPrimaryExpression(e);
    }
    public void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException {
        e.getStructureExpression().accept(getVisitor());
    }

    /* Loop */
    protected void processAbstractLoop(final AbstractLoop l)
        throws VisitorException {
        processRange(l.getRange());
    }
    public void visitLinkageLoopTerm(LinkageLoopTerm term)
        throws VisitorException {
        processAbstractLoop(term);
        term.getTerm().accept(getVisitor());
    }
    public void visitLoopExpression(LoopExpression e) throws VisitorException {
        processAbstractLoop(e);
        e.getExpression().accept(getVisitor());
    }
    public void visitLoopStatement(LoopStatement s) throws VisitorException {
        processAbstractLoop(s);
        s.getStatement().accept(getVisitor());
    }

    protected void processGuardedCommandInterface(GuardedCommandInterface gci)
        throws VisitorException {
    }
    public void visitLoopGuard(LoopGuard s) throws VisitorException {
        processAbstractLoop(s);
        for (Iterator i = s.getGuards().iterator(); i.hasNext(); ) {
            processGuardedCommandInterface((GuardedCommandInterface) i.next());
        }
    }

    /* Assignment and variable declaration statements */
    public void visitAssignmentStatement(AssignmentStatement s)
        throws VisitorException {
        s.getLeftHandSide().accept(getVisitor());
        s.getRightHandSide().accept(getVisitor());
    }
    public void visitIncDecStatement(IncDecStatement s)
        throws VisitorException {
        s.getExpression().accept(getVisitor());
    }
    public void visitExpressionStatement(ExpressionStatement s)
        throws VisitorException {
        s.getExpression().accept(getVisitor());
    }
    public void visitVarStatement(VarStatement s) throws VisitorException {
        processDeclarationList(s.getDeclarationList());
        final StatementInterface stmt = s.getStatement();
        if (stmt != null) stmt.accept(getVisitor());
    }

    /* Abstract guarded statement */
    protected void
    processAbstractGuardedStatement(AbstractGuardedStatement s)
    throws VisitorException {
        for (Iterator i = s.getGuardedCommands(); i.hasNext(); ) {
            final GuardedCommandInterface gci =
                (GuardedCommandInterface) i.next();
            processGuardedCommandInterface(gci);
        }
        final StatementInterface els = s.getElseStatement();
        if (els != null) els.accept(getVisitor());
    }

    /* Abstract reptition statement */
    protected void
    processAbstractRepetitionStatement(AbstractRepetitionStatement s)
    throws VisitorException {
        processAbstractGuardedStatement(s);
    }
    /* Deterministic repetition and selection */
    public void
        visitDeterministicRepetitionStatement
        (DeterministicRepetitionStatement s) throws VisitorException
    {
        processAbstractRepetitionStatement(s);
    }
    public void
        visitNonDeterministicRepetitionStatement
        (NonDeterministicRepetitionStatement s) throws VisitorException
    {
        processAbstractRepetitionStatement(s);
        final LinkageTerms terms = s.getNeutralState();
        if (terms != null) processLinkageTerms(terms);
    }

    /* Abstract selection statement */
    protected void
    processAbstractSelectionStatement(AbstractSelectionStatement s)
    throws VisitorException {
        processAbstractGuardedStatement(s);
    }
    public void
        visitDeterministicSelectionStatement
        (DeterministicSelectionStatement s) throws VisitorException
    {
        processAbstractSelectionStatement(s);
    }
    public void
        visitNonDeterministicSelectionStatement
        (NonDeterministicSelectionStatement s) throws VisitorException
    {
        processAbstractSelectionStatement(s);
        final LinkageTerms terms = s.getNeutralState();
        if (terms != null) processLinkageTerms(terms);
    }

    /* Trivial statements */
    public void visitErrorStatement(ErrorStatement s) throws VisitorException {
    }
    public void visitSkipStatement(SkipStatement s) throws VisitorException {
    }

    /* LinkageExpressionInterface */
    protected void
    processLinkageExpressionInterface(LinkageExpressionInterface e)
    throws VisitorException { }
    public void
    visitLinkageArrayAccessExpression(LinkageArrayAccessExpression e)
        throws VisitorException {
        processLinkageExpressionInterface(e);
    }
    public void visitLinkageExpressionTerm(LinkageExpressionTerm term)
        throws VisitorException {
        term.getExpression().accept(getVisitor());
    }
    public void visitLinkageIdentifierExpression(LinkageIdentifierExpression e)
        throws VisitorException {
        processLinkageExpressionInterface(e);
    }
    public void
    visitLinkageStructureAccessExpression(LinkageStructureAccessExpression e)
        throws VisitorException {
        processLinkageExpressionInterface(e);
    }

    /* Range */
    protected void processRange(final Range r) throws VisitorException {
        r.getMinExpression().accept(getVisitor());
        r.getMaxExpression().accept(getVisitor());
    }

    protected void processLinkageTerms(final LinkageTerms terms)
        throws VisitorException {
        for (Iterator i = terms.getTerms(); i.hasNext(); ) {
            ((LinkageTermInterface) i.next()).accept(getVisitor());
        }
    }

    /* Currently unused */
    public void visitIdentifierList(IdentifierList il) throws VisitorException {
    }
}
