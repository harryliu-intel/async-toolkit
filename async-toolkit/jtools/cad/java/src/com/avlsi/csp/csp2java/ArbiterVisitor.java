/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.csp2java;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.util.UniqueLabel;
import com.avlsi.util.container.Pair;
import com.avlsi.util.text.StringUtil;

/**
 * Visitor to find all nodes involved in linked arbitration and create
 * <code>Arbiter.Linkage</code>s for them.
 *
 * The sequence of the statements that gets visited must be identical to the
 * sequence used by JavaEmitter, otherwise the linkage objects won't match up
 * properly.  See bug 12054.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
class ArbiterVisitor implements VisitorInterface {

    /**
     * List of <code>ArbiterLinkage</code>s that 
     **/
    private final ArrayList<ArbiterLinkage> arbiters =
        new ArrayList<ArbiterLinkage>();

    /**
     * List of <code>Term[]</code>s for the guards of the arbiter
     * that is currently being built up.
     **/
    private ArrayList guards = null;

    /**
     * List of <code>Term</code>s for the nodes in an arbitration condition
     * that is currently being built up.
     **/
    private ArrayList terms = null;

    /**
     * The linked node that is currently being built up.
     **/
    private TermInterface term = null;

    /**
     * The JavaEmitter used to generate code for index expressions in a linkage
     * term.
     **/
    private JavaEmitter javaEmitter;

    /**
     * Maps from neutral linkage terms to ints to label linkage objects.
     **/
    private UniqueLabel linkageLabel;

    /**
     * The old and current PrintWriters that <code>javaEmitter</code> is using.
     **/
    private PrintWriter oldWriter, currWriter;

    /**
     * Destination of captured output from <code>javaEmitter</code>.
     **/
    private StringWriter stringWriter;

    ArbiterVisitor(final JavaEmitter javaEmitter,
                   final UniqueLabel linkageLabel) {
        this.javaEmitter = javaEmitter;
        this.linkageLabel = linkageLabel;
    }

    void declareLinkages(final PrintWriter out) {
        for (ArbiterLinkage link : arbiters) {
            out.println("private final Linkage " + link.getName() + ";");
        }
    }

    void initLinkages(final PrintWriter out) throws VisitorException {
        for (ArbiterLinkage link : arbiters) {
            out.print(link.getName() + " = " +
                      "deviceParams.getArbitrationMode() == " +
                      "Arbiter.NON_LINKED ? null : ");
            link.emitInitializer(out);
        }
    }

    void destroyLinkages(final PrintWriter out) {
        for (ArbiterLinkage link : arbiters) {
            final String l = link.getName();
            out.println("if (" + l + " != null) " + l + ".destroy();");
        }
    }

    private static final class ArbiterLinkage {
        private final TermInterface[][] guardTerms;
        private final TermInterface[] neutralState;
        private final int id;

        private ArbiterLinkage(final TermInterface[][] guardTerms,
                               final TermInterface[] neutralState,
                               final int id) {
            this.guardTerms = guardTerms;
            this.neutralState = neutralState;
            this.id = id;
        }

        private void emitInitializer(final PrintWriter out)
            throws VisitorException {
            out.println("new Linkage(");
                emitGuards(guardTerms, out);
                out.println(',');
                out.print("flattenTerms(");
                emitTerms(neutralState, out);
                out.println("), deviceParams.getArbitrationMode()");
            out.println(");");
        }

        private static void emitGuards(final TermInterface[][] guardTerms,
                                       final PrintWriter out)
            throws VisitorException {
            out.println("flattenTerms(new Term[][][]{");
            for (int i = 0; i < guardTerms.length; ++i) {
                emitGuardTerms(guardTerms[i], out);
                if (i < guardTerms.length - 1) out.print(", ");
            }
            out.println("})");
        }

        private static void emitGuardTerms(final TermInterface[] terms,
                                           final PrintWriter out)
            throws VisitorException {
            if (terms[0] instanceof GuardTerm) {
                assert terms.length == 1;
                out.println(terms[0].getCode());
            } else {
                emitTerms(terms, out);
            }
        }

        private static void emitTerms(final TermInterface[] terms,
                                      final PrintWriter out)
            throws VisitorException {
            out.print("new Term[][] {");
            out.print("flattenTerms(new Term[][] {");
            for (int i = 0; i < terms.length; ++i) {
                out.print(terms[i].getCode());
                if (i < terms.length - 1) out.print(", ");
            }
            out.print("})");
            out.print(" }");
        }

        public String getName() {
            return "linkage" + id;
        }
    }

    /**
     * Start capturing output from the JavaEmitter.
     **/
    private void startCapture() {
        oldWriter = javaEmitter.getOutputWriter();
        stringWriter = new StringWriter();
        currWriter = new PrintWriter(stringWriter);
        javaEmitter.setOutputWriter(currWriter);
    }

    /**
     * Stop capturing output from the JavaEmitter, and returned the string
     * captured so far.
     **/
    private String endCapture() {
        currWriter.close();
        javaEmitter.setOutputWriter(oldWriter);
        return stringWriter.toString();
    }

    private interface TermInterface {
        /**
         * Returns an Java expression that generates the appropriate code to
         * reprsent the term.
         **/
        String getCode() throws VisitorException;
    }

    private final class GuardTerm implements TermInterface {
        private final Collection terms;
        private final Range range;
        private final IdentifierExpression var;
        private GuardTerm(final Collection terms, final Range range,
                          final IdentifierExpression var) {
            this.terms = terms;
            this.range = range;
            this.var = var;
        }

        /**
         * Generates code that when evaluated has type Term[].  This flattens
         * the terms associated with one guard:
         * @(<,i:2: (x[i])>, y[0]) becomes @(x[0], x[1], y[0])
         **/
        private void emitSimpleGuard(final TermInterface[] terms,
                                     final StringBuffer buf)
            throws VisitorException {
            buf.append("flattenTerms(new Term[][] { ");
            boolean first = true;
            for (TermInterface term : terms) {
                if (first) first = false;
                else buf.append(", ");
                buf.append(term.getCode());
            }
            buf.append(" })");
        }

        /**
         * Generates code that when evaluated has type Term[][].  The first
         * dimension is the guard.
         **/
        public String getCode() throws VisitorException {
            final StringBuffer buf = new StringBuffer();
            buf.append("(new LinkageHelper.GuardUnroller() {\n");
                buf.append("public Term[][] evaluate(final CspInteger ");
                startCapture();
                var.accept(javaEmitter);
                buf.append(endCapture());
                buf.append(") throws InterruptedException {\n");
                    buf.append("return flattenTerms(new Term[][][] { ");
                    boolean first = true;
                    for (Iterator i = terms.iterator(); i.hasNext(); ) {
                        if (first) first = false;
                        else buf.append(", ");
                        final Object o = (Object) i.next();
                        if (o instanceof GuardTerm) {
                            buf.append(((GuardTerm) o).getCode());
                        } else {
                            buf.append("new Term[][] { ");
                            emitSimpleGuard((TermInterface[]) o, buf);
                            buf.append(" }");
                        }
                    }
                    buf.append("});\n");
                buf.append('}');
            buf.append("}.unroll(");
            startCapture();
            range.getMinExpression().accept(javaEmitter);
            buf.append(endCapture());
            buf.append(", ");
            startCapture();
            range.getMaxExpression().accept(javaEmitter);
            buf.append(endCapture());
            buf.append("))");
            return buf.toString();
        }
    }

    private final class LoopTerm implements TermInterface {
        private final TermInterface term;
        private final Range range;
        private final IdentifierExpression var;
        private LoopTerm(final TermInterface term, final Range range,
                         final IdentifierExpression var) {
            this.term = term;
            this.range = range;
            this.var = var;
        }

        /**
         * Generates code that when evaluated has type Term[].
         **/
        public String getCode() throws VisitorException {
            final StringBuffer buf = new StringBuffer();
            buf.append("(new LinkageHelper.NeutralityUnroller() {\n");
                buf.append("public Term[] evaluate(final CspInteger ");
                startCapture();
                var.accept(javaEmitter);
                buf.append(endCapture());
                buf.append(") throws InterruptedException {\n");
                    buf.append("return ");
                    buf.append(term.getCode());
                    buf.append(';');
                buf.append('}');

            buf.append("}.unroll(");
            startCapture();
            range.getMinExpression().accept(javaEmitter);
            buf.append(endCapture());
            buf.append(", ");
            startCapture();
            range.getMaxExpression().accept(javaEmitter);
            buf.append(endCapture());
            buf.append("))");
            return buf.toString();
        }
    }

    private static final class Term implements TermInterface {
        private static final class Part {
            public final boolean type;
            public final StringBuffer text;
            public Part(final String text, final boolean type) {
                this.text = new StringBuffer(text);
                this.type = type;
            }
        }

        private final List parts;
        private final boolean isNegated;
        private Part last;

        private Term(final boolean isNegated) {
            this.parts = new ArrayList();
            this.isNegated = isNegated;
            this.last = null;
        }

        private void add(final String part, final boolean type) {
            if (last != null && last.type == type) {
                last.text.append(part);
                return;
            }
            last = new Part(part, type);
            parts.add(last);
        }

        public Term append(final char literal) {
            return append(String.valueOf(literal));
        }

        public Term append(final String literal) {
            add(literal, true);
            return this;
        }

        public Term addCode(final String code) {
            add(code, false);
            return this;
        }

        /**
         * Generates code that when evaluated has type Term[].
         **/
        public String getCode() throws VisitorException {
            final StringBuffer buf = new StringBuffer();
            buf.append("getTerm(deviceParams.getName(), ");
            for (Iterator i = parts.iterator(); i.hasNext(); ) {
                final Part part = (Part) i.next();
                if (part.type) {
                    buf.append('"');
                    buf.append(
                        StringUtil.replaceSubstring(part.text, "][", ","));
                    buf.append('"');
                } else {
                    buf.append('(');
                    buf.append(part.text);
                    buf.append(')');
                    buf.append(".toString()");
                }
                if (i.hasNext()) buf.append(" + ");
            }
            buf.append(", ");
            buf.append(isNegated);
            buf.append(')');
            return buf.toString();
        }
    }

    private void unsupported(AbstractASTNodeInterface node)
            throws VisitorException {
        throw new VisitorException("Unsupported syntax in arbiter " +
                                   "linkage at " + node.getParseRange());
    }

    public void visitCSPProgram(CSPProgram p) throws VisitorException {
        // there may be linkage nodes in the function definitions
        for (final Iterator i = p.getFunctionDeclarations(); i.hasNext(); ) {
            final FunctionDeclaration funcDecl =
                (FunctionDeclaration) i.next();
            funcDecl.getBodyStatement().accept(this);
        }

        // body
        final StatementInterface stmt = p.getStatement();
        if (stmt != null) stmt.accept(this);
    }

    public void visitAddExpression(AddExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitAndExpression(AndExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitArrayAccessExpression(ArrayAccessExpression e)
        throws VisitorException {
        unsupported(e);
        // e.getArrayExpression().accept(this);
        // term.append('[');
        // e.getIndexExpression().accept(this);
        // term.append(']');
    }

    public void visitBitRangeExpression(BitRangeExpression e)
            throws VisitorException {
        unsupported(e);
    }

    public void visitConditionalAndExpression(ConditionalAndExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitConditionalOrExpression(ConditionalOrExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitDivideExpression(DivideExpression e)
            throws VisitorException {
        unsupported(e);
    }

    public void visitEqualityExpression(EqualityExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitExponentialExpression(ExponentialExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitFunctionCallExpression(FunctionCallExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitGreaterEqualExpression(GreaterEqualExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitGreaterThanExpression(GreaterThanExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException {
        unsupported(e);
        // term.append(e.getIdentifier());
    }

    public void visitIncDecStatement(IncDecStatement e)
        throws VisitorException {
        // empty
    }

    public void visitInequalityExpression(InequalityExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitIntegerExpression(IntegerExpression e)
            throws VisitorException {
        ((Term) term).append(new BigInteger(e.getValue(), e.getRadix()).toString());
    }

    public void visitLeftShiftExpression(LeftShiftExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitLessEqualExpression(LessEqualExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitLessThanExpression(LessThanExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitLoopExpression(LoopExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitMultiplyExpression(MultiplyExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitNegateExpression(NegateExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitNotExpression(NotExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitOrExpression(OrExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitXorExpression(XorExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitPeekExpression(PeekExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitProbeExpression(ProbeExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitReceiveExpression(ReceiveExpression e) throws VisitorException {
        unsupported(e);
    }

    public void visitRemainderExpression(RemainderExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitRightShiftExpression(RightShiftExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitStringExpression(StringExpression e)
        throws VisitorException {
    }

    public void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException {
        unsupported(e);
        // e.getStructureExpression().accept(this);
        // term.append('.').append(e.getFieldName());
    }

    public void visitMemberAccessExpression(MemberAccessExpression e)
        throws VisitorException {
        unsupported(e);
    }

    public void visitSubtractExpression(SubtractExpression e)
        throws VisitorException {
        unsupported(e);
    }


    public void visitAssignmentStatement(AssignmentStatement s)
        throws VisitorException {
        // empty
    }

    public void visitDeterministicRepetitionStatement(
            DeterministicRepetitionStatement s)
        throws VisitorException {
        processGuardedStatement(s, null);
    }

    private void recurseGuards(Iterator g) throws VisitorException {
        for (final Iterator i = g; i.hasNext(); ) {
            final GuardedCommandInterface gci =
                (GuardedCommandInterface) i.next();
            if (gci instanceof LoopGuard) {
                final LoopGuard lg = (LoopGuard) gci;
                recurseGuards(lg.getGuards().iterator());
            } else {
                final GuardedCommand guardedCommand = (GuardedCommand) gci;
                guardedCommand.getCommand().accept(this);
            }
        }
    }

    private TermInterface getLinkageTerms(final LoopGuard loop)
        throws VisitorException {
        final List result = new ArrayList();
        for (Iterator i = loop.getGuards().iterator(); i.hasNext(); ) {
            final Object guard = i.next();
            if (guard instanceof LoopGuard) {
                result.add(getLinkageTerms((LoopGuard) guard));
            } else {
                final GuardedCommand gc = (GuardedCommand) guard;
                result.add(processLinkageTerms(gc.getLinkageTerms()));
            }
        }
        return new GuardTerm(result, loop.getRange(),
                             loop.getIndexVarExpression());
    }

    private void processGuardedStatement(
        final AbstractGuardedStatement s,
        final LinkageTerms neutralState) throws VisitorException {
        assert guards == null;

        // construct the arbiter at this level first
        if (neutralState != null) {
            guards = new ArrayList();

            for (final Iterator i = s.getGuardedCommands(); i.hasNext(); ) {
                final GuardedCommandInterface gci =
                    (GuardedCommandInterface) i.next();
                if (gci instanceof LoopGuard) {
                    guards.add(new TermInterface[] {
                            getLinkageTerms((LoopGuard) gci) } );
                } else {
                    final GuardedCommand gc = (GuardedCommand) gci;
                    guards.add(processLinkageTerms(gc.getLinkageTerms()));
                }
            }

            arbiters.add
                (new ArbiterLinkage
                    ((TermInterface[][]) guards.toArray(
                        new TermInterface[guards.size()][]),
                     processLinkageTerms(neutralState),
                     linkageLabel.getLabel(neutralState)));
            guards = null;
        }

        if (s.getElseStatement() != null)
            s.getElseStatement().accept(this);

        // then find any lower ones
        recurseGuards(s.getGuardedCommands());
    }

    public void visitDeterministicSelectionStatement(
            DeterministicSelectionStatement s)
        throws VisitorException {
        processGuardedStatement(s, null);
    }

    public void visitErrorStatement(ErrorStatement s) throws VisitorException {
        // empty
    }

    public void visitExpressionStatement(ExpressionStatement s)
        throws VisitorException {
        // empty
    }

    public void visitLoopStatement(LoopStatement s)
        throws VisitorException {
        // empty
        // XXX: Doing nothing isn't quite right.  We could have linked
        // arbiters inside the body, but we'll assume this is not the case.
    }

    public void visitNonDeterministicRepetitionStatement(
            NonDeterministicRepetitionStatement s)
        throws VisitorException {
        assert s.getNeutralState() == null;
        processGuardedStatement(s, null);
    }

    public void visitNonDeterministicSelectionStatement(
            NonDeterministicSelectionStatement s)
        throws VisitorException {
        processGuardedStatement(s, s.getNeutralState());
    }

    public void visitParallelStatement(ParallelStatement s)
        throws VisitorException {
        processCompositeStatement(s);
    }

    private void processCompositeStatement(AbstractCompositeStatement s)
            throws VisitorException {
        for (final Iterator i = s.getStatements(); i.hasNext(); ) {
            final StatementInterface subStmt =
                (StatementInterface) i.next();
            subStmt.accept(this);
        }
    }

    public void visitReceiveStatement(ReceiveStatement s)
        throws VisitorException {
        // empty
    }

    public void visitSendStatement(SendStatement s) throws VisitorException {
        // empty
    }

    public void visitSequentialStatement(SequentialStatement s)
        throws VisitorException {
        processCompositeStatement(s);
    }

    public void visitSkipStatement(SkipStatement s) throws VisitorException {
        // empty
    }

    public void visitVarStatement(VarStatement s) throws VisitorException {
        // empty
    }

    public void visitArrayType(ArrayType t) throws VisitorException {
        assert false;
    }
    public void visitChannelStructureType(ChannelStructureType t)
        throws VisitorException {
        assert false;
    }
    public void visitChannelType(ChannelType t) throws VisitorException {
        assert false;
    }

    public void visitIntegerType(IntegerType t) throws VisitorException {
        assert false;
    }

    public void visitNodeType(NodeType t) throws VisitorException {
        assert false;
    }

    public void visitBooleanType(BooleanType t) throws VisitorException {
        assert false;
    }

    public void visitStringType(StringType t) throws VisitorException {
        assert false;
    }

    public void visitStructureType(StructureType t) throws VisitorException {
        assert false;
    }

    public void visitIdentifierList(IdentifierList il)
            throws VisitorException {
        assert false;
    }

    public void visitLinkageLoopTerm(LinkageLoopTerm loopTerm)
            throws VisitorException {
        loopTerm.getTerm().accept(this);
        term = new LoopTerm(term, loopTerm.getRange(),
                            loopTerm.getIndexVarExpression());
    }

    public void visitLinkageExpressionTerm(LinkageExpressionTerm t)
        throws VisitorException {
        assert term == null;
        term = new Term(t.isInverted());

        t.getExpression().accept(this);
    }

    public void visitLinkageArrayAccessExpression(
            LinkageArrayAccessExpression e)
        throws VisitorException {
        final Term t = (Term) term;
        e.getArrayExpression().accept(this);
        t.append('[');

        startCapture();
        e.getIndexExpression().accept(javaEmitter);

        t.addCode("(");
        t.addCode(endCapture());
        t.addCode(").toString()");
        t.append(']');
    }

    public void visitLinkageIdentifierExpression(
            LinkageIdentifierExpression e)
        throws VisitorException {
        ((Term) term).append(e.getIdentifier());
    }

    public void visitLinkageStructureAccessExpression(
            LinkageStructureAccessExpression e) throws VisitorException {
        e.getStructureExpression().accept(this);
        ((Term) term).append('.').append(e.getFieldName());
    }

    private TermInterface[] processLinkageTerms(final LinkageTerms linkageTerms)
            throws VisitorException {
        assert terms == null;
        terms = new ArrayList();

        for (final Iterator i = linkageTerms.getTerms(); i.hasNext(); ) {
            final LinkageTermInterface linkageTerm =
                (LinkageTermInterface) i.next();
            linkageTerm.accept(this);
            if (term != null) {
                terms.add(term);
                term = null;
            }
        }

        final TermInterface[] ts =
            (TermInterface[]) terms.toArray(new TermInterface[terms.size()]);
        terms = null;
        return ts;
    }

    public void visitLoopGuard(LoopGuard s) throws VisitorException {
        // currently, we don't support arbiter linking in loops
    }
}
