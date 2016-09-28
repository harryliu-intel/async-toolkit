/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.csp.csp2xml;

import com.avlsi.csp.ast.AbstractASTNodeInterface;
import com.avlsi.csp.ast.AbstractBinaryExpression;
import com.avlsi.csp.ast.AbstractChannelStatement;
import com.avlsi.csp.ast.AbstractCompositeStatement;
import com.avlsi.csp.ast.AbstractGuardedStatement;
import com.avlsi.csp.ast.AbstractLoop;
import com.avlsi.csp.ast.AbstractUnaryExpression;
import com.avlsi.csp.ast.AddExpression;
import com.avlsi.csp.ast.AndExpression;
import com.avlsi.csp.ast.ArrayAccessExpression;
import com.avlsi.csp.ast.ArrayType;
import com.avlsi.csp.ast.AssignmentStatement;
import java.math.BigInteger;
import com.avlsi.csp.ast.BitRangeExpression;
import com.avlsi.csp.ast.BooleanType;
import com.avlsi.csp.ast.ChannelType;
import com.avlsi.csp.ast.ChannelStructureType;
import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.csp.ast.ConditionalAndExpression;
import com.avlsi.csp.ast.ConditionalOrExpression;
import com.avlsi.util.debug.Debug;
import com.avlsi.csp.ast.Declaration;
import com.avlsi.csp.ast.DeclarationList;
import com.avlsi.csp.ast.Declarator;
import com.avlsi.csp.ast.DeterministicRepetitionStatement;
import com.avlsi.csp.ast.DeterministicSelectionStatement;
import com.avlsi.csp.ast.DivideExpression;
import com.avlsi.csp.ast.EqualityExpression;
import com.avlsi.csp.ast.ErrorStatement;
import com.avlsi.csp.ast.ExponentialExpression;
import com.avlsi.csp.ast.ExpressionInterface;
import com.avlsi.csp.ast.ExpressionStatement;
import com.avlsi.csp.ast.FunctionCallExpression;
import com.avlsi.csp.ast.FunctionDeclaration;
import com.avlsi.csp.ast.GreaterEqualExpression;
import com.avlsi.csp.ast.GreaterThanExpression;
import com.avlsi.csp.ast.GuardedCommand;
import com.avlsi.csp.ast.GuardedCommandWithStatement;
import com.avlsi.csp.ast.IdentifierExpression;
import com.avlsi.csp.ast.IdentifierList;
import com.avlsi.csp.ast.IncDecStatement;
import com.avlsi.csp.ast.InequalityExpression;
import com.avlsi.csp.ast.IntegerExpression;
import com.avlsi.csp.ast.IntegerType;
import com.avlsi.csp.ast.XorExpression;

import java.util.Iterator;
import com.avlsi.csp.ast.LeftShiftExpression;
import com.avlsi.csp.ast.LessEqualExpression;
import com.avlsi.csp.ast.LessThanExpression;
import com.avlsi.csp.ast.LinkageArrayAccessExpression;
import com.avlsi.csp.ast.LinkageExpressionTerm;
import com.avlsi.csp.ast.LinkageIdentifierExpression;
import com.avlsi.csp.ast.LinkageLoopTerm;
import com.avlsi.csp.ast.LinkageStructureAccessExpression;
import java.util.LinkedList;
import com.avlsi.csp.ast.LoopGuard;
import com.avlsi.csp.ast.LoopExpression;
import com.avlsi.csp.ast.LoopStatement;
import com.avlsi.csp.ast.MemberAccessExpression;
import com.avlsi.csp.ast.MultiplyExpression;
import com.avlsi.csp.ast.NegateExpression;
import com.avlsi.csp.ast.NodeType;
import com.avlsi.csp.ast.NonDeterministicRepetitionStatement;
import com.avlsi.csp.ast.NonDeterministicSelectionStatement;
import com.avlsi.csp.ast.NotExpression;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.csp.ast.OrExpression;
import com.avlsi.csp.ast.ParallelStatement;
import com.avlsi.csp.ast.PeekExpression;
import java.io.PrintWriter;
import com.avlsi.csp.ast.ProbeExpression;
import com.avlsi.csp.ast.ReceiveExpression;
import com.avlsi.csp.ast.ReceiveStatement;
import com.avlsi.csp.ast.RemainderExpression;
import com.avlsi.csp.ast.RightShiftExpression;
import com.avlsi.csp.ast.SendStatement;
import com.avlsi.csp.ast.SequentialStatement;
import com.avlsi.csp.ast.SkipStatement;
import com.avlsi.csp.ast.StatementInterface;
import com.avlsi.csp.ast.StringExpression;
import com.avlsi.csp.ast.StringType;
import com.avlsi.util.text.StringUtil;
import com.avlsi.csp.ast.StructureAccessExpression;
import com.avlsi.csp.ast.StructureType;
import com.avlsi.csp.ast.SubtractExpression;
import com.avlsi.csp.ast.Type;
import com.avlsi.csp.ast.VarStatement;
import com.avlsi.csp.ast.VisitorException;
import com.avlsi.csp.ast.VisitorInterface;

public class XmlTourist implements VisitorInterface {
    private final PrintWriter pw;
    private int indent = 0;

    public XmlTourist(PrintWriter pw) {
        this.pw = pw;
    }

    private String hexify(String val, int base) {
        byte[] b = new BigInteger(val, base).toByteArray();
        StringBuffer s = new StringBuffer();
        for (int i = 0; i < b.length; i++)
            s.append(NumberFormatter.toHexString(0xff & (int)b[i], 2));
        return s.toString();
    }

    public void print(String s) {
        pw.println(StringUtil.repeatString("  ", indent) + s);
    }

    public void beginTag(String s) {
        print("<" + s + ">");
        indent++;
    }

    public void endTag(String s) {
        indent--;
        print("</" + s + ">");
    }

    private String pos(AbstractASTNodeInterface n) {
        String range = n.getParseRange().toString();
        if (range.equals("0:0-0:0"))
            return "";
        return "pos=\"" + range + "\"";
    }

    private void beginTag(AbstractASTNodeInterface n) {
        beginTag(n, "");
    }

    private void beginTag(AbstractASTNodeInterface n, String extra) {
        String s = n.getClass().getName();
        s = s.substring(s.lastIndexOf('.') + 1);
        print("<" + s + " " + extra + pos(n) + ">");
        indent++;
    }

    private void endTag(AbstractASTNodeInterface n) {
        indent--;
        String s = n.getClass().getName();
        s = s.substring(s.lastIndexOf('.') + 1);
        print("</" + s + ">");
    }

    private void binary(AbstractBinaryExpression e) throws VisitorException {
        String s = e.getClass().getName();
        s = s.substring(s.lastIndexOf('.') + 1, s.length() - 10);
        s = s.substring(0,1).toLowerCase() + s.substring(1);
        print("<BinaryExpression op=\"" + s + "\" " + pos(e) + ">");
        indent++;
        beginTag("left");
        e.getLeft().accept(this);
        endTag("left");
        beginTag("right");
        e.getRight().accept(this);
        endTag("right");
        endTag("BinaryExpression");
    }

    private void unary(AbstractUnaryExpression e) throws VisitorException {
        String s = e.getClass().getName();
        s = s.substring(s.lastIndexOf('.') + 1, s.length() - 10);
        s = s.substring(0,1).toLowerCase() + s.substring(1);
        print("<UnaryExpression op=\"" + s + "\" " + pos(e) + ">");
        indent++;
        e.getExpression().accept(this);
        endTag("UnaryExpression");
    }

    private void beginChannel(AbstractASTNodeInterface n)
        throws VisitorException {
        String s = n.getClass().getName();
        s = s.substring(s.lastIndexOf('.') + 1, s.length() - 10);
        s = s.substring(0,1).toLowerCase() + s.substring(1);
        print("<ChannelExpression op=\"" + s + "\" " + pos(n) + ">");
        indent++;
    }

    private void endChannel(AbstractASTNodeInterface n)
        throws VisitorException {
        endTag("ChannelExpression");
    }

    private void guarded(AbstractGuardedStatement s) throws VisitorException {
        String str = s.getClass().getName();
        str = str.substring(str.lastIndexOf('.') + 1, str.length() - 9);
        str = str.substring(0,1).toLowerCase() + str.substring(1);
        print("<GuardedStatement op=\"" + str + "\" " + pos(s) + ">");
        indent++;
        for (Iterator it = s.getGuardedCommands(); it.hasNext(); ) {
            GuardedCommand g = (GuardedCommand) it.next();
            if (g instanceof GuardedCommandWithStatement) {
                beginTag("GuardedCommandWithStatement");
                beginTag("guard");
                beginTag("statement");
                ((GuardedCommandWithStatement) g).getGuardStatement()
                                                 .accept(this);
                endTag("statement");
                beginTag("expression");
                g.getGuard().accept(this);
                endTag("expression");
                endTag("guard");
                beginTag("command");
                g.getCommand().accept(this);
                endTag("command");
                endTag("GuardedCommandWithStatement");
            } else {
                beginTag("GuardedCommand");
                beginTag("guard");
                g.getGuard().accept(this);
                endTag("guard");
                beginTag("command");
                g.getCommand().accept(this);
                endTag("command");
                endTag("GuardedCommand");
            }
        }
        if (s.getElseStatement() != null) {
            beginTag("else");
            s.getElseStatement().accept(this);
            endTag("else");
        }
        endTag("GuardedStatement");
    }

    private void composite(AbstractCompositeStatement s)
        throws VisitorException {
        int len =
            CollectionUtils.addAll(new LinkedList(), s.getStatements()).size();
        switch (len) {
        case 0:
            /* Not sure if this will ever happen, but if it does, an
             * empty composite statement is equivalent to a skip statement */
            print("<SkipStatement " + pos(s) + "/>");
            break;
        case 1:
            /* If there is only one statement, don't wrap it in a
             * composite statement, since that would just be silly. */
            ((StatementInterface)s.getStatements().next()).accept(this);
            break;
        default:
            String str = s.getClass().getName();
            str = str.substring(str.lastIndexOf('.') + 1, str.length() - 9);
            str = str.substring(0,1).toLowerCase() + str.substring(1);
            print("<CompositeStatement op=\"" + str + "\" " + pos(s) + ">");
            indent++;
            for (Iterator it = s.getStatements(); it.hasNext(); ) {
                StatementInterface si = (StatementInterface) it.next();
                si.accept(this);
            }
            endTag("CompositeStatement");
            break;
        }
    }

    private void channel(AbstractChannelStatement s) throws VisitorException {
        String str = s.getClass().getName();
        str = str.substring(str.lastIndexOf('.') + 1, str.length() - 9);
        str = str.substring(0,1).toLowerCase() + str.substring(1);
        print("<ChannelStatement op=\"" + str + "\" " + pos(s) + ">");
        indent++;
        beginTag("channel");
        s.getChannelExpression().accept(this);
        endTag("channel");
        if (s.getRightHandSide() != null) {
            beginTag("rhs");
            s.getRightHandSide().accept(this);
            endTag("rhs");
        }
        endTag("ChannelStatement");
    }

    private void loop(AbstractLoop l, String op, AbstractASTNodeInterface n)
        throws VisitorException {
        beginTag(l, "op=\"" + op + "\" var=\"" + l.getIndexVar() + "\" ");
        beginTag("min");
        l.getRange().getMinExpression().accept(this);
        endTag("min");
        beginTag("max");
        l.getRange().getMaxExpression().accept(this);
        endTag("max");
        beginTag("loopBody");
        if (n instanceof ExpressionInterface) {
            ((ExpressionInterface)n).accept(this);
        } else {
            ((StatementInterface)n).accept(this);
        }
        endTag("loopBody");
        endTag(l);
    }

    public void visitCSPProgram(CSPProgram p) throws VisitorException {
        beginTag(p, "filename=\"" + p.getParseRange().start.filename + "\" ");
        beginTag("initializer");
        if (p.getInitializerStatement() != null)
            p.getInitializerStatement().accept(this);
        endTag("initializer");
        beginTag("functions");
        for (Iterator it = p.getFunctionDeclarations(); it.hasNext(); ) {
            FunctionDeclaration f = (FunctionDeclaration) it.next();
            beginTag("function name=\"" + f.getName() + "\"");
            beginTag("parameters");
            printDeclarationList(f.getFormals());
            endTag("parameters");
            if (f.getReturnType() != null) {
                beginTag("returnType");
                f.getReturnType().accept(this);
                endTag("returnType");
            }
            beginTag("body");
            f.getBodyStatement().accept(this);
            endTag("body");
            endTag("function");
        }
        endTag("functions");
        if (p.getStatement() != null) {
            beginTag("body");
            p.getStatement().accept(this);
            endTag("body");
        }
        endTag(p);
    }
    
    
    public void visitAddExpression(AddExpression e) throws VisitorException {
        binary(e);
    }
    
    public void visitAndExpression(AndExpression e) throws VisitorException {
        binary(e);
    }

    public void visitConditionalAndExpression(ConditionalAndExpression e)
        throws VisitorException {
        binary(e);
    }

    public void visitConditionalOrExpression(ConditionalOrExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitArrayAccessExpression(ArrayAccessExpression e)
        throws VisitorException {
        beginTag(e);
        beginTag("array");
        e.getArrayExpression().accept(this);
        endTag("array");
        beginTag("index");
        e.getIndexExpression().accept(this);
        endTag("index");
        endTag(e);
    }
    
    public void visitBitRangeExpression(BitRangeExpression e)
        throws VisitorException {
        beginTag(e);
        beginTag("bits");
        e.getBitsExpression().accept(this);
        endTag("bits");
        if (e.getMinExpression() != null) {
            beginTag("min");
            e.getMinExpression().accept(this);
            endTag("min");
        }
        beginTag("max");
        e.getMaxExpression().accept(this);
        endTag("max");
        endTag(e);
    }
    
    public void visitDivideExpression(DivideExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitEqualityExpression(EqualityExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitExponentialExpression(ExponentialExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitFunctionCallExpression(FunctionCallExpression e)
        throws VisitorException {
        beginTag(e);
        beginTag("function");
        e.getFunctionExpression().accept(this);
        endTag("function");
        beginTag("parameters");
        for (Iterator it = e.getActuals(); it.hasNext(); ) {
            ExpressionInterface p = (ExpressionInterface) it.next();
            p.accept(this);
        }
        endTag("parameters");
        endTag(e);
    }
    
    public void visitGreaterEqualExpression(GreaterEqualExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitGreaterThanExpression(GreaterThanExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitIncDecStatement(IncDecStatement s)
        throws VisitorException {
        final String type = s.isIncrement() ? "increment" : "decrement";
        beginTag(type);
        beginTag("expr");
        s.getExpression().accept(this);
        endTag("expr");
        endTag(type);
    }

    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException {
	print("<identifier id=\"" + e.getIdentifier() + "\" " + pos(e) + "/>");
    }
    
    public void visitInequalityExpression(InequalityExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitIntegerExpression(IntegerExpression e)
        throws VisitorException {
	print("<integer val=\"" + e.getValue() + "\" base=\"" + e.getRadix() +
              // "\" hex=\"" + hexify(e.getValue(), e.getRadix()) +
              "\" " + pos(e) + "/>");
    }
    
    public void visitLeftShiftExpression(LeftShiftExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitLessEqualExpression(LessEqualExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitLessThanExpression(LessThanExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitLoopExpression(LoopExpression e) throws VisitorException {
        String op = "unknown";
        switch (e.getSeparator()) {
        case LoopExpression.AND:
            op = "and";
            break;
        case LoopExpression.OR:
            op = "or";
            break;
        case LoopExpression.XOR:
            op = "xor";
            break;
        case LoopExpression.TIMES:
            op = "multiply";  // for consistency with MultiplyExpression
            break;
        case LoopExpression.PLUS:
            op = "add";       // for consistency with AddExpression
            break;
        }
        loop(e, op, e.getExpression());
    }
    
    public void visitMultiplyExpression(MultiplyExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitNegateExpression(NegateExpression e)
        throws VisitorException {
        unary(e);
    }
    
    public void visitNotExpression(NotExpression e) throws VisitorException {
        unary(e);
    }
    
    public void visitOrExpression(OrExpression e) throws VisitorException {
        binary(e);
    }

    public void visitXorExpression(XorExpression e) throws VisitorException {
        binary(e);
    }
    
    public void visitPeekExpression(PeekExpression e) throws VisitorException {
        beginChannel(e);
        e.getChannelExpression().accept(this);
	endChannel(e);
    }
    
    public void visitProbeExpression(ProbeExpression e)
        throws VisitorException {
        beginChannel(e);
        e.getChannelExpression().accept(this);
        endChannel(e);
    }
    
    public void visitReceiveExpression(ReceiveExpression e)
        throws VisitorException {
	beginChannel(e);
        e.getChannelExpression().accept(this);
	endChannel(e);
    }
    
    public void visitRemainderExpression(RemainderExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitRightShiftExpression(RightShiftExpression e)
        throws VisitorException {
        binary(e);
    }
    
    public void visitStringExpression(StringExpression e)
        throws VisitorException {
        // XXX: encode special characters
        print("<string val=\"" + e.getValue() + "\" " + pos(e) + "/>");
    }

    public void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException {
        beginTag(e, "field=\"" + e.getFieldName() + "\" ");
        e.getStructureExpression().accept(this);
	endTag(e);
    }
    
    public void visitSubtractExpression(SubtractExpression e)
        throws VisitorException {
        binary(e);
    }
    
    
    public void visitAssignmentStatement(AssignmentStatement s)
        throws VisitorException {
        beginTag(s);
        beginTag("lhs");
        s.getLeftHandSide().accept(this);
        endTag("lhs");
        beginTag("rhs");
        s.getRightHandSide().accept(this);
        endTag("rhs");
        endTag(s);
    }
    
    public void visitDeterministicRepetitionStatement(DeterministicRepetitionStatement s)
        throws VisitorException {
        guarded(s);
    }
    
    public void visitDeterministicSelectionStatement(DeterministicSelectionStatement s)
        throws VisitorException {
        guarded(s);
    }
    
    public void visitErrorStatement(ErrorStatement s) throws VisitorException {
	print("<ErrorStatement " + pos(s) + "/>");
    }
    
    public void visitLoopStatement(LoopStatement s)
        throws VisitorException {
        String op = "unknown";
        switch (s.getSeparator()) {
        case LoopStatement.SEQUENTIAL:
            op = "sequential";
            break;
        case LoopStatement.PARALLEL:
            op = "parallel";
            break;
        }
        loop(s, op, s.getStatement());
    }
    
    public void visitNonDeterministicRepetitionStatement(NonDeterministicRepetitionStatement s)
        throws VisitorException {
        guarded(s);
    }
    
    public void visitNonDeterministicSelectionStatement(NonDeterministicSelectionStatement s)
        throws VisitorException {
        guarded(s);
    }
    
    public void visitParallelStatement(ParallelStatement s)
        throws VisitorException {
        composite(s);
    }
    
    public void visitReceiveStatement(ReceiveStatement s)
        throws VisitorException {
        channel(s);
    }
    
    public void visitSendStatement(SendStatement s) throws VisitorException {
        channel(s);
    }
    
    public void visitSequentialStatement(SequentialStatement s)
        throws VisitorException {
        composite(s);
    }
    
    public void visitSkipStatement(SkipStatement s) throws VisitorException {
	print("<SkipStatement " + pos(s) + "/>");
    }
    
    private void printDeclarationList(DeclarationList l)
        throws VisitorException {
        for (Iterator it = l.getDeclarations(); it.hasNext(); ) {
            Declaration d = (Declaration) it.next();

            for (Iterator it2 = d.getDeclaratorList().getDeclarators() ;
                 it2.hasNext(); ) {
                Declarator d2 = (Declarator) it2.next();

                beginTag("Declarator");
                d2.getIdentifier().accept(this);
                d2.getTypeFragment().accept(this);
                if (d2.getInitializer() != null) {
                    beginTag("initializer");
                    d2.getInitializer().accept(this);
                    endTag("initializer");
                }
                endTag("Declarator");
            }
        }
    }

    public void visitVarStatement(VarStatement s) throws VisitorException {
        beginTag(s);
        printDeclarationList(s.getDeclarationList());

        // ???: When will this not be null?
        if (s.getStatement() != null)
            throw new VisitorException("s.getStatement() is not null!");
        endTag(s);
    }
    
    public void visitChannelType(ChannelType t) throws VisitorException {
        throw new AssertionError("Declarations of channels not supported.");
    }

    public void visitChannelStructureType(ChannelStructureType t)
        throws VisitorException {
        throw new AssertionError("Declarations of structures of " +
                                 "channels not supported.");
    }

    public void visitArrayType(ArrayType t) throws VisitorException {
        beginTag(t);
        beginTag("min");
        t.getRange().getMinExpression().accept(this);
        endTag("min");
        beginTag("max");
        t.getRange().getMaxExpression().accept(this);
        endTag("max");
        beginTag("elementType");
        t.getElementType().accept(this);
        endTag("elementType");
        endTag(t);
    }
    
    public void visitIntegerType(IntegerType t) throws VisitorException {
        beginTag("IntegerType const=\"" + t.isConst() +
                 "\" signed=\"" + t.isSigned() +
                 "\" " + pos(t));
        if (t.getDeclaredWidth() != null) {
            beginTag("width");
            t.getDeclaredWidth().accept(this);
            endTag("width");
        }
        endTag("IntegerType");
    }
    
    public void visitBooleanType(BooleanType t) throws VisitorException {
	print("<BooleanType const=\"" + t.isConst() + "\" " + pos(t) + "/>");
    }
    
    public void visitNodeType(NodeType t) throws VisitorException {
        throw new AssertionError("Declarations of nodes not supported.");
    }
    
    public void visitIdentifierList(IdentifierList il)
        throws VisitorException {
        Debug.unimplemented();
    }
    
    
    public void visitLinkageLoopTerm(LinkageLoopTerm term)
        throws VisitorException {
        Debug.unimplemented();
    }
    
    public void visitLinkageExpressionTerm(LinkageExpressionTerm term)
        throws VisitorException {
        Debug.unimplemented();
    }
    
    
    public void visitLinkageArrayAccessExpression(LinkageArrayAccessExpression e)
        throws VisitorException {
        Debug.unimplemented();
    }
    
    public void visitLinkageIdentifierExpression(LinkageIdentifierExpression e)
        throws VisitorException {
        Debug.unimplemented();
    }
    
    public void visitLinkageStructureAccessExpression(LinkageStructureAccessExpression e) throws VisitorException {
        Debug.unimplemented();
    }

    public void visitLoopGuard(LoopGuard s) throws VisitorException {
        Debug.unimplemented();
    }   

    public void visitExpressionStatement(ExpressionStatement s)
        throws VisitorException {
        beginTag(s);
        s.getExpression().accept(this);
        endTag(s);
    }   

    public void visitStringType(StringType t) throws VisitorException {
        print("<StringType const=\"" + t.isConst() + "\" " + pos(t) + "/>");
    }

    public void visitStructureType(StructureType t) throws VisitorException {
        print("<StructureType const=\"" + t.isConst() + "\" " + pos(t) + "/>");
    }

    public void visitMemberAccessExpression(MemberAccessExpression e)
        throws VisitorException {
        beginTag(e, "field=\"" + e.getMemberName() + "\" ");
        e.getStructureExpression().accept(this);
        endTag(e);
    }   
    public static String print(ExpressionInterface ei) {
        if (ei == null) return "null";
        final java.io.StringWriter sw = new java.io.StringWriter();
        final PrintWriter pw = new PrintWriter(sw);
        final XmlTourist tour = new XmlTourist(pw);
        String result = "visit exception";
        try {
            ei.accept(tour);
            pw.flush();
            result = sw.toString();
        } catch (VisitorException e) { }
        return result;
    }
}
