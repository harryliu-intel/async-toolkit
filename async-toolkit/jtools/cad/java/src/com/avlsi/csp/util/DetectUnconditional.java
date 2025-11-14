// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.util;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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

    /** Definitely unconditional send/recv channels */
    private Set<String> uncond;

    /** Definitely unconditional peeked channels */
    private Set<String> uncondPeek;

    /** Resolve function calls to declarations */
    private final RefinementResolver resolver;

    /** For type information of channels */
    private final Map<String,Type> portTypes;

    private Type currType;

    /** Possible channel names */
    private List<StringBuilder> channelNames = null;

    public class Results {
        public Set<String> getPeeked() {
            return peeked;
        }

        public Set<String> getUncond() {
            return uncond;
        }

        public Set<String> getUncondPeek() {
            return uncondPeek;
        }
    }

    public DetectUnconditional(final RefinementResolver resolver,
                               final Map<String,Type> portTypes) {
        this.resolver = resolver;
        this.portTypes = portTypes;
        peeked = new HashSet<>();
        uncond = new HashSet<>();
        uncondPeek = new HashSet<>();
    }

    public Results getResults() {
        return new Results();
    }

    public Set<String> getPeeked() {
        return peeked;
    }

    public Set<String> getUncond() {
        return uncond;
    }

    public Set<String> getUncondPeek() {
        return uncondPeek;
    }

    private Set<String> intersect(Set<String> a, Set<String> b) {
        final Set<String> result = new HashSet<>(a);
        result.retainAll(b);
        return result;
    }

    private void nobranch(List<StringBuilder> names, boolean peek) {
        if (peek) {
            for (StringBuilder name : names) {
                peeked.add(name.toString());
            }
        }

        if (names.size() == 1) {
            if (peek) {
                uncondPeek.add(names.get(0).toString());
            } else {
                uncond.add(names.get(0).toString());
            }
        }
    }

    private List<StringBuilder> getChannelName(final ExpressionInterface expr)
        throws VisitorException {
        channelNames = new ArrayList<>();
        currType = null;
        expr.accept(this);
        final List<StringBuilder> result = channelNames;
        channelNames = null;
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
        if (channelNames == null) {
            super.visitArrayAccessExpression(e);
        } else {
            e.getArrayExpression().accept(this);

            if (!(currType instanceof ArrayType)) {
                throw new VisitorException("Expected an array");
            }

            final ArrayType at = (ArrayType) currType;
            currType = at.getElementType();
            final BigInteger idx =
                CspUtils.getIntegerConstant(e.getIndexExpression());
            final BigInteger min;
            final BigInteger max;
            if (idx == null) {
                min = CspUtils.getIntegerConstant(at.getRange().getMinExpression());
                max = CspUtils.getIntegerConstant(at.getRange().getMaxExpression());
            } else {
                min = idx;
                max = idx;
            }
            final List<StringBuilder> oldChannelNames = channelNames;
            channelNames = new ArrayList<StringBuilder>();
            for (StringBuilder channelName : oldChannelNames) {
                final int l = channelName.length();
                if (channelName.charAt(l - 1) == ']') {
                    channelName.setCharAt(l - 1, ',');
                } else {
                    channelName.append('[');
                }
                for (BigInteger bi = min; bi.compareTo(max) <= 0; bi = bi.add(BigInteger.ONE)) {
                    channelNames.add(new StringBuilder(channelName.toString() + bi + "]"));
                }
            }
        }
    }

    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException {
        super.visitIdentifierExpression(e);
        if (channelNames != null) {
            currType = portTypes.get(e.getIdentifier());
            channelNames.add(new StringBuilder(e.getIdentifier()));
        }
    }

    public void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException {
        super.visitStructureAccessExpression(e);
        if (channelNames != null) {
            if (currType instanceof ChannelStructureType) {
                currType =
                    ((ChannelStructureType) currType).getMemberType(e.getFieldName());
            }
            for (StringBuilder channelName : channelNames) {
                channelName.append('.' + e.getFieldName());
            }
        }
    }

    private void mergeBranch(List<Set<String>> accum) {
        if (accum.get(0) == null) {
            accum.set(0, uncond);
            accum.set(1, uncondPeek);
        } else {
            accum.set(0, intersect(accum.get(0), uncond));
            accum.set(1, intersect(accum.get(1), uncondPeek));
        }
    }

    private void evaluateBranch(GuardedCommandInterface gci, List<Set<String>> accum)
        throws VisitorException {
        uncond = new HashSet<>();
        uncondPeek = new HashSet<>();

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

        mergeBranch(accum);
    }

    protected void
    processAbstractGuardedStatement(AbstractGuardedStatement s)
        throws VisitorException {
        final Set<String> oldUncond = uncond;
        final Set<String> oldUncondPeek = uncondPeek;

        final List<Set<String>> accum = Arrays.asList(null, null);
        for (Iterator i = s.getGuardedCommands(); i.hasNext(); ) {
            evaluateBranch((GuardedCommandInterface) i.next(), accum);
        }
        final StatementInterface els = s.getElseStatement();
        if (els != null) {
            uncond = new HashSet<>();
            uncondPeek = new HashSet<>();
            els.accept(getVisitor());
            mergeBranch(accum);
        }

        uncond = oldUncond;
        uncondPeek = oldUncondPeek;
        uncond.addAll(accum.get(0));
        uncondPeek.addAll(accum.get(1));
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
