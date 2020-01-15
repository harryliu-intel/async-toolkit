package com.avlsi.csp.util;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.HashSet;
import java.util.Set;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.util.RefinementResolver;
import com.avlsi.csp.util.VisitorByCategory;
import com.avlsi.util.container.Pair;

/**
 * Detect unconditional channels.  Traverse into functions, but give up on
 * array index that's not an integer constant (after constant propagation).
 **/
public class DetectUnconditional extends VisitorByCategory {
    /** Possibly peeked channels */
    private Set<String> peeked;

    /** Definitely unconditional channels */
    private Set<String> uncond;

    /** Resolve function calls to declarations */
    private final RefinementResolver resolver;

    /** Whether encountered an array with a variable index */
    boolean variableIndex = false;

    /** Build up the name of the channel being accessed */
    StringBuilder channelName = null;

    public DetectUnconditional(final RefinementResolver resolver) {
        this.resolver = resolver;
        peeked = new HashSet<>();
        uncond = new HashSet<>();
    }

    public Set<String> getPeeked() {
        return peeked;
    }

    public Set<String> getUncond() {
        return uncond;
    }

    private Set<String> intersect(Set<String> a, Set<String> b) {
        final Set<String> result = new HashSet<>(a);
        result.retainAll(b);
        return result;
    }

    private void nobranch(String name, boolean peek) {
        if (name != null) {
            uncond.add(name);
            if (peek) peeked.add(name);
        }
    }

    private String getChannelName(final ExpressionInterface expr)
        throws VisitorException {
        channelName = new StringBuilder();
        variableIndex = false;
        expr.accept(this);
        final String result = variableIndex ? null : channelName.toString();
        channelName = null;
        return result;
    }

    public void visitSendStatement(SendStatement s)
        throws VisitorException {
        super.visitSendStatement(s);
        nobranch(getChannelName(s.getChannelExpression()), false);
    }

    public void visitReceiveStatement(ReceiveStatement s)
        throws VisitorException {
        super.visitReceiveStatement(s);
        nobranch(getChannelName(s.getChannelExpression()), false);
    }

    public void visitPeekExpression(PeekExpression e)
        throws VisitorException {
        super.visitPeekExpression(e);
        nobranch(getChannelName(e.getChannelExpression()), true);
    }

    public void visitReceiveExpression(ReceiveExpression e)
        throws VisitorException {
        super.visitReceiveExpression(e);
        nobranch(getChannelName(e.getChannelExpression()), false);
    }

    public void visitArrayAccessExpression(ArrayAccessExpression e)
        throws VisitorException {
        if (channelName == null) {
            super.visitArrayAccessExpression(e);
        } else {
            final BigInteger idx =
                CspUtils.getIntegerConstant(e.getIndexExpression());
            if (idx == null) {
                variableIndex = true;
            } else {
                e.getArrayExpression().accept(this);
                final int l = channelName.length();
                if (channelName.charAt(l - 1) == ']') {
                    channelName.setCharAt(l - 1, ',');
                } else {
                    channelName.append('[');
                }
                channelName.append(idx + "]");
            }
        }
    }

    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException {
        super.visitIdentifierExpression(e);
        if (channelName != null) {
            channelName.append(e.getIdentifier());
        }
    }

    public void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException {
        super.visitStructureAccessExpression(e);
        if (channelName != null) {
            channelName.append('.' + e.getFieldName());
        }
    }

    protected void
    processAbstractGuardedStatement(AbstractGuardedStatement s)
        throws VisitorException {
        Set<String> oldUncond = uncond;
        Set<String> merged = null;
        for (Iterator i = s.getGuardedCommands(); i.hasNext(); ) {
            uncond = new HashSet<>();
            final GuardedCommandInterface gci =
                (GuardedCommandInterface) i.next();
            ExpressionInterface guard = null;
            StatementInterface command = null, guardStmt = null;
            if (gci instanceof GuardedCommandWithStatement) {
                final GuardedCommandWithStatement gcws =
                    (GuardedCommandWithStatement) gci;
                guard = gcws.getGuard();
                command = gcws.getCommand();
                guardStmt = gcws.getGuardStatement();
            } else if (gci instanceof GuardedCommand) {
                final GuardedCommand gcws = (GuardedCommand) gci;
                guard = gcws.getGuard();
                command = gcws.getCommand();
            }

            if (guard != null) guard.accept(getVisitor());
            if (guardStmt != null) guardStmt.accept(getVisitor());
            if (command != null) command.accept(getVisitor());
            if (merged == null) merged = uncond;
            else merged = intersect(merged, uncond);
        }
        final StatementInterface els = s.getElseStatement();
        if (els != null) {
            uncond = new HashSet<>();
            els.accept(getVisitor());
            if (merged == null) merged = uncond;
            else merged = intersect(merged, uncond);
        }
        uncond = oldUncond;
        uncond.addAll(merged);
    }

    public void visitFunctionCallExpression(FunctionCallExpression e)
        throws VisitorException {
        super.visitFunctionCallExpression(e);
        final Pair p = (Pair) resolver.getResolvedFunctions().get(e);
        if (p != null && p.getSecond() instanceof FunctionDeclaration) {
            processFunctionDeclaration((FunctionDeclaration) p.getSecond());
        }
    }
}
