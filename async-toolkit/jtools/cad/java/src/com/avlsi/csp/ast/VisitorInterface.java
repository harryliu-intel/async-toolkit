/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

/**
 * Visitor pattern for CSP AST.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface VisitorInterface {
    void visitCSPProgram(CSPProgram p) throws VisitorException;

    void visitAddExpression(AddExpression e) throws VisitorException;
    void visitAndExpression(AndExpression e) throws VisitorException;
    void visitArrayAccessExpression(ArrayAccessExpression e)
        throws VisitorException;
    void visitBitRangeExpression(BitRangeExpression e) throws VisitorException;
    void visitConditionalAndExpression(ConditionalAndExpression e)
        throws VisitorException;
    void visitConditionalOrExpression(ConditionalOrExpression e)
        throws VisitorException;
    void visitDivideExpression(DivideExpression e) throws VisitorException;
    void visitEqualityExpression(EqualityExpression e)
        throws VisitorException;
    void visitExponentialExpression(ExponentialExpression e)
        throws VisitorException;
    void visitFunctionCallExpression(FunctionCallExpression e)
        throws VisitorException;
    void visitGreaterEqualExpression(GreaterEqualExpression e)
        throws VisitorException;
    void visitGreaterThanExpression(GreaterThanExpression e)
        throws VisitorException;
    void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException;
    void visitIncDecStatement(IncDecStatement s) throws VisitorException;
    void visitInequalityExpression(InequalityExpression e)
        throws VisitorException;
    void visitIntegerExpression(IntegerExpression e) throws VisitorException;
    void visitLeftShiftExpression(LeftShiftExpression e)
        throws VisitorException;
    void visitLessEqualExpression(LessEqualExpression e)
        throws VisitorException;
    void visitLessThanExpression(LessThanExpression e)
        throws VisitorException;
    void visitLoopExpression(LoopExpression e) throws VisitorException;
    void visitMultiplyExpression(MultiplyExpression e)
        throws VisitorException;
    void visitNegateExpression(NegateExpression e) throws VisitorException;
    void visitNotExpression(NotExpression e) throws VisitorException;
    void visitOrExpression(OrExpression e) throws VisitorException;
    void visitPeekExpression(PeekExpression e) throws VisitorException;
    void visitProbeExpression(ProbeExpression e) throws VisitorException;
    void visitReceiveExpression(ReceiveExpression e) throws VisitorException;
    void visitRemainderExpression(RemainderExpression e)
        throws VisitorException;
    void visitRightShiftExpression(RightShiftExpression e)
        throws VisitorException;
    void visitStringExpression(StringExpression e)
        throws VisitorException;
    void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException;
    void visitMemberAccessExpression(MemberAccessExpression e)
        throws VisitorException;
    void visitSubtractExpression(SubtractExpression e)
        throws VisitorException;
    void visitXorExpression(XorExpression e) throws VisitorException;

    void visitAssignmentStatement(AssignmentStatement s)
        throws VisitorException;
    void visitDeterministicRepetitionStatement(
            DeterministicRepetitionStatement s)
        throws VisitorException;
    void visitDeterministicSelectionStatement(
            DeterministicSelectionStatement s)
        throws VisitorException;
    void visitErrorStatement(ErrorStatement s) throws VisitorException;
    void visitExpressionStatement(ExpressionStatement s)
        throws VisitorException;
    void visitLoopStatement(LoopStatement s)
        throws VisitorException;
    void visitNonDeterministicRepetitionStatement(
            NonDeterministicRepetitionStatement s)
        throws VisitorException;
    void visitNonDeterministicSelectionStatement(
            NonDeterministicSelectionStatement s)
        throws VisitorException;
    void visitParallelStatement(ParallelStatement s)
        throws VisitorException;
    void visitReceiveStatement(ReceiveStatement s)
        throws VisitorException;
    void visitSendStatement(SendStatement s) throws VisitorException;
    void visitSequentialStatement(SequentialStatement s)
        throws VisitorException;
    void visitSkipStatement(SkipStatement s) throws VisitorException;
    void visitVarStatement(VarStatement s) throws VisitorException;
    void visitArrayType(ArrayType t) throws VisitorException;
    void visitChannelType(ChannelType t) throws VisitorException;
    void visitChannelStructureType(ChannelStructureType t)
        throws VisitorException;
    void visitIntegerType(IntegerType t) throws VisitorException;
    void visitBooleanType(BooleanType t) throws VisitorException;
    void visitNodeType(NodeType t) throws VisitorException;
    void visitStringType(StringType t) throws VisitorException;
    void visitStructureType(StructureType t) throws VisitorException;
    void visitIdentifierList(IdentifierList il) throws VisitorException;

    void visitLinkageLoopTerm(LinkageLoopTerm term) throws VisitorException;
    void visitLinkageExpressionTerm(LinkageExpressionTerm term)
        throws VisitorException;

    void visitLinkageArrayAccessExpression(LinkageArrayAccessExpression e)
        throws VisitorException;
    void visitLinkageIdentifierExpression(LinkageIdentifierExpression e)
        throws VisitorException;
    void visitLinkageStructureAccessExpression(
            LinkageStructureAccessExpression e) throws VisitorException;
    void visitLoopGuard(LoopGuard s) throws VisitorException;
}
