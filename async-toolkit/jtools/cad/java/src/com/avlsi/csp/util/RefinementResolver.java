package com.avlsi.csp.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Set;

import com.avlsi.csp.ast.*;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.Pair;

public class RefinementResolver extends VisitorByCategory {
    public interface BuiltIn {
        String EVENTQUEUEISEMPTY = "eventQueueIsEmpty";
        String ENABLEDSIMERRORS = "enableDSimErrors";
        String STABLE = "stable";
        String WAIT = "wait";
        String LOG4 = "log4";
        String LOG2 = "log2";
        String CHOOSE = "choose";
        String SRANDOM = "srandom";
        String RANDOM = "random";
        String TIME = "time";
        String ENERGY = "energy";
    }

    // Define built-in functions
    private static Declaration getDeclaration(final String name,
                                              final Type type,
                                              final int dir) {
        final Declarator dcltor =
            new Declarator(new IdentifierExpression(name), type, null, dir);
        final DeclaratorList dcltors = new DeclaratorList();
        dcltors.addDeclarator(dcltor);
        return new Declaration(dcltors);
    }

    private static Declaration getDeclaration(final String name,
                                              final Type type) {
        return getDeclaration(name, type, Declarator.IN);
    }

    private static Declaration getDeclaration(final String name) {
        return getDeclaration(name, new IntegerType());
    }

    private static DeclarationList getDeclarations(final Declaration[] dcls) {
        final DeclarationList result = new DeclarationList();
        for (int i = 0; i < dcls.length; ++i) result.addDeclaration(dcls[i]);
        return result;
    }

    private static DeclarationList getDeclarations(final Declaration dcl) {
        return getDeclarations(new Declaration[] { dcl });
    }

    private static final Map builtinFunctions =
        CollectionUtils.mapify(
            new Object[] {
                BuiltIn.EVENTQUEUEISEMPTY,
                new FunctionDeclaration(BuiltIn.EVENTQUEUEISEMPTY,
                                        new DeclarationList(),
                                        new BooleanType(),
                                        new SequentialStatement()),

                BuiltIn.RANDOM,
                new FunctionDeclaration(BuiltIn.RANDOM,
                                        getDeclarations(getDeclaration("bits")),
                                        new IntegerType(),
                                        new SequentialStatement()),

                BuiltIn.SRANDOM,
                new FunctionDeclaration(BuiltIn.SRANDOM,
                                        getDeclarations(getDeclaration("bits")),
                                        new IntegerType(),
                                        new SequentialStatement()),

                BuiltIn.CHOOSE,
                new FunctionDeclaration(BuiltIn.CHOOSE,
                                        getDeclarations(new Declaration[] {
                                            getDeclaration("condition",
                                                new BooleanType()),
                                            getDeclaration("true_value"),
                                            getDeclaration("false_value")
                                        }),
                                        new IntegerType(),
                                        new SequentialStatement()),

                BuiltIn.LOG2,
                new FunctionDeclaration(BuiltIn.LOG2,
                                        getDeclarations(getDeclaration("val")),
                                        new IntegerType(),
                                        new SequentialStatement()),

                BuiltIn.LOG4,
                new FunctionDeclaration(BuiltIn.LOG4,
                                        getDeclarations(getDeclaration("val")),
                                        new IntegerType(),
                                        new SequentialStatement()),

                BuiltIn.WAIT,
                new FunctionDeclaration(BuiltIn.WAIT,
                                        getDeclarations(getDeclaration("delay")),
                                        null,
                                        new SequentialStatement()),

                BuiltIn.STABLE,
                new FunctionDeclaration(BuiltIn.STABLE,
                                        getDeclarations(
                                            getDeclaration(
                                                "node",
                                                new NodeType(PortDirection.IN),
                                                Declarator.INOUT)),
                                        new IntegerType(),
                                        new SequentialStatement()),

                BuiltIn.ENABLEDSIMERRORS,
                new FunctionDeclaration(BuiltIn.ENABLEDSIMERRORS,
                                        getDeclarations(getDeclaration("val")),
                                        null,
                                        new SequentialStatement()),

                BuiltIn.TIME,
                new FunctionDeclaration(BuiltIn.TIME,
                                        new DeclarationList(),
                                        new IntegerType(),
                                        new SequentialStatement()),
                BuiltIn.ENERGY,
                new FunctionDeclaration(BuiltIn.ENERGY,
                                        getDeclarations(getDeclaration("val")),
                                        null,
                                        new SequentialStatement())
            });

    public static final String RESET_FUNCTION_NAME = "resetNodes";
    public static final FunctionCallExpression RESET_FUNCTION_CALL =
        new FunctionCallExpression(
            new IdentifierExpression(RESET_FUNCTION_NAME));

    public static boolean isBuiltin(final Object decl) {
        return builtinFunctions.values().contains(decl);
    }

    public interface Policy {
        /**
         * Determine whether a function declaration is the function to invoke
         * in a function call expression.
         *
         * @param p program where <code>decl</code> is defined
         * @param decl function declaration
         * @param expr function call expression
         * @return <code>true</code> if the function declaration is compatible
         * with the function call; <code>false</code> otherwise
         **/
        boolean isMatch(CSPProgram p, FunctionDeclaration decl,
                        FunctionCallExpression expr);
        /**
         * Determine whether a function declaration is a constructor call for
         * the given structure.
         **/
        boolean isMatch(CSPProgram p, StructureDeclaration decl,
                        FunctionCallExpression expr);

        /**
         * Determine whether the structure type refers to the given structure
         * declaration.
         **/
        boolean isMatch(CSPProgram p, StructureDeclaration decl,
                        StructureType t);
    }

    /**
     * A policy that determines function and structure compatibility by
     * examining only the name.
     **/
    public static Policy NAME = new Policy() {
        private boolean isMatch(final FunctionCallExpression expr,
                                final String name) {
            final ExpressionInterface func = expr.getFunctionExpression();
            return func instanceof IdentifierExpression &&
                   ((IdentifierExpression) func).getIdentifier().equals(name);
        }
        public boolean isMatch(CSPProgram p,
                               FunctionDeclaration decl,
                               FunctionCallExpression expr) {
            return isMatch(expr, decl.getName());
        }
        public boolean isMatch(CSPProgram p, StructureDeclaration decl,
                               FunctionCallExpression expr) {
            return isMatch(expr, decl.getName());
        }
        public boolean isMatch(CSPProgram p, StructureDeclaration decl,
                               StructureType t) {
            return decl.getName().equals(t.getName());
        }
    };

    private final Policy policy;
    private CSPProgram currentProgram;
    private StatementInterface stmt;
    private SequentialStatement initStmt;
    private FunctionDeclaration currentFunc;
    private StructureDeclaration currentStruct;
    private Map/*<FunctionCallExpression,Pair<CSPProgram, FunctionDeclaration>>*/ functionMap;
    private Map/*<StructureType,Pair<CSPProgram,StructureDeclaration>>*/ structureMap;
    private Set/*<FunctionDeclaration>*/ usedFunctions;
    private Set/*<StructureDeclaration>*/ usedStructures;

    public RefinementResolver(final Policy policy) {
        this.policy = policy;
    }

    public void resolve(final CSPProgram p) throws VisitorException {
        currentProgram = p;
        currentFunc = null;
        currentStruct = null;
        functionMap =
            new IdentityHashMap/*<FunctionCallExpression,
                                  Pair<CSPProgram,FunctionDeclaration>>*/();
        structureMap =
            new IdentityHashMap/*<StructureType,
                                  Pair<CSPProgram,
                                       StructureDeclaration>>*/();
        usedFunctions = new LinkedHashSet/*<FunctionDeclaration>*/();
        usedStructures = new LinkedHashSet/*<StructureDeclaration>*/();
        final Pair stmts = resolveStatement(p);
        initStmt = (SequentialStatement) stmts.getFirst();
        if (initStmt != null) initStmt.accept(getVisitor());
        stmt = (StatementInterface) stmts.getSecond();
        if (stmt != null) {
            resolveFunction(currentProgram, RESET_FUNCTION_CALL);
            stmt.accept(getVisitor());
        }
    }

    public CSPProgram getCSPProgram() {
        final CSPProgram result = new CSPProgram();
        result.epr(currentProgram);

        for (Iterator i = usedFunctions.iterator(); i.hasNext(); ) {
            result.addFunctionDeclaration((FunctionDeclaration) i.next());
        }
        for (Iterator i = usedStructures.iterator(); i.hasNext(); ) {
            result.addStructureDeclaration((StructureDeclaration) i.next());
        }
        if (stmt != null) result.setStatement(stmt);
        if (initStmt != null) result.setInitializerStatement(initStmt);
        return result;
    }

    public StatementInterface getStatement() {
        return stmt;
    }

    public Map/*<FunctionCallExpression,Pair<CSPProgram, FunctionDeclaration>>*/
    getResolvedFunctions() {
        return functionMap;
    }

    public Map/*<StructureType,Pair<CSPProgram,StructureDeclaration>>*/
    getResolvedStructures() {
        return structureMap;
    }

    public boolean isUsed(final FunctionDeclaration decl) {
        return usedFunctions.contains(decl);
    }

    public boolean isUsed(final StructureDeclaration decl) {
        return usedStructures.contains(decl);
    }

    public static Pair resolveStatement(final CSPProgram p) {
        Pair result = new Pair(p.getInitializerStatement(), p.getStatement());
        for (Iterator i = p.getRefinementParents().iterator();
             result.getSecond() == null && i.hasNext(); ) {
            final CSPProgram parent = (CSPProgram) i.next();
            if (!p.inheritDeclarationOnly(parent))
                result = resolveStatement(parent);
        }
        return result;
    }

    private Pair/*<CSPProgram,FunctionDeclaration>*/ findBuiltin(
            final FunctionCallExpression e) {
        for (Iterator i = builtinFunctions.values().iterator(); i.hasNext(); ) {
            final FunctionDeclaration decl = (FunctionDeclaration) i.next();
            if (policy.isMatch(null, decl, e)) {
                return new Pair/*<CSPProgram,FunctionDeclaration>*/(null, decl);
            }
        }
        return null;
    }

    private Pair/*<CSPProgram,FunctionDeclaration>*/ findLocal(
            final CSPProgram p,
            final FunctionCallExpression e) {
        for (Iterator i = p.getFunctionDeclarations(); i.hasNext(); ) {
            final FunctionDeclaration decl = (FunctionDeclaration) i.next();
            if (decl == currentFunc)
                break;
            else if (policy.isMatch(p, decl, e))
                return new Pair/*<CSPProgram,FunctionDeclaration>*/(p, decl);
        }
        for (Iterator i = p.getStructureIterator(); i.hasNext(); ) {
            final StructureDeclaration decl = (StructureDeclaration) i.next();
            if (decl == currentStruct)
                break;
            else if (policy.isMatch(p, decl, e))
                return new Pair/*<CSPProgram,FunctionDeclaration>*/(p, decl);
        }
        return null;
    }

    private Pair/*<CSPProgram,StructureDeclaration>*/ findLocal(
            final CSPProgram p,
            final StructureType t) {
        for (Iterator i = p.getStructureIterator(); i.hasNext(); ) {
            final StructureDeclaration decl = (StructureDeclaration) i.next();
            if (decl == currentStruct)
                break;
            else if (policy.isMatch(p, decl, t))
                return new Pair/*<CSPProgram,StructureDeclaration>*/(p, decl);
        }
        return null;
    }

    private Pair/*<CSPProgram,FunctionDeclaration>*/ findFunction(
            final CSPProgram p,
            final FunctionCallExpression e)
        throws VisitorException {
        Pair/*<CSPProgram,FunctionDeclaration>*/ result = findLocal(p, e);
        for (Iterator i = p.getRefinementParents().iterator();
             i.hasNext() && result == null; ) {
            final CSPProgram parent = (CSPProgram) i.next();
            result = findFunction(parent, e);
        }
        if (result != null) {
            final Object func = result.getSecond();
            if (func instanceof FunctionDeclaration) {
                final CSPProgram oldProg = currentProgram;
                final FunctionDeclaration oldFunc = currentFunc;
                currentProgram = (CSPProgram) result.getFirst();
                currentFunc = (FunctionDeclaration) func;
                processFunctionDeclaration(currentFunc);
                currentProgram = oldProg;
                currentFunc = oldFunc;
            }
        }
        return result;
    }

    private Pair/*<CSPProgram,StructureDeclaration>*/ findStructure(
            final CSPProgram p,
            final StructureType t)
        throws VisitorException {
        Pair/*<CSPProgram,StructureDeclaration>*/ result = findLocal(p, t);
        for (Iterator i = p.getRefinementParents().iterator();
             i.hasNext() && result == null; ) {
            final CSPProgram parent = (CSPProgram) i.next();
            result = findStructure(parent, t);
        }
        if (result != null) {
            final CSPProgram oldProg = currentProgram;
            final StructureDeclaration oldStruct = currentStruct;
            currentProgram = (CSPProgram) result.getFirst();
            currentStruct = (StructureDeclaration) result.getSecond();
            processStructureDeclaration(currentStruct);
            currentProgram = oldProg;
            currentStruct = oldStruct;
        }
        return result;
    }

    private void resolveFunction(final CSPProgram p,
                                 final FunctionCallExpression e)
        throws VisitorException {
        final boolean isBuiltin;
        Pair/*<CSPProgram,FunctionDeclaration>*/ decl = findBuiltin(e);
        if (decl == null) {
            isBuiltin = false;
            decl = findFunction(p, e);
        } else {
            isBuiltin = true;
        }
        functionMap.put(e, decl);
        if (decl != null && !isBuiltin) {
            final Object o = decl.getSecond();
            if (o instanceof FunctionDeclaration) {
                usedFunctions.add(o);
            } else {
                assert o instanceof StructureDeclaration;
                usedStructures.add(o);
            }
        }
    }

    private void resolveStructure(final CSPProgram p,
                                  final StructureType t)
        throws VisitorException {
        Pair/*<CSPProgram,StructureDeclaration>*/ decl = findStructure(p, t);
        structureMap.put(t, decl);
        if (decl != null) usedStructures.add(decl.getSecond());
    }

    protected void processGuardedCommandInterface(GuardedCommandInterface gci)
        throws VisitorException {
        if (gci instanceof GuardedCommand) {
            final GuardedCommand g = (GuardedCommand) gci;
            if (g instanceof GuardedCommandWithStatement) {
                final GuardedCommandWithStatement gcws =
                    (GuardedCommandWithStatement) g;
                gcws.getGuardStatement().accept(getVisitor());
            }
            g.getGuard().accept(getVisitor());
            if (g.getLinkageTerms() != null)
                processLinkageTerms(g.getLinkageTerms());
            g.getCommand().accept(getVisitor());
        } else if (gci instanceof LoopGuard) {
            final LoopGuard g = (LoopGuard) gci;
            for (Iterator i = g.getGuards().iterator(); i.hasNext(); ) {
                final GuardedCommandInterface one =
                    (GuardedCommandInterface) i.next();
                processGuardedCommandInterface(one);
            }
        }
    }

    public void visitFunctionCallExpression(final FunctionCallExpression e)
        throws VisitorException {
        resolveFunction(currentProgram, e);
        super.visitFunctionCallExpression(e);
    }

    public void visitArrayType(final ArrayType t) throws VisitorException {
        t.getElementType().accept(getVisitor());
    }

    public void visitStructureType(final StructureType t)
        throws VisitorException {
        resolveStructure(currentProgram, t);
    }

    public void visitIntegerType(IntegerType t) throws VisitorException {
        final ExpressionInterface width = t.getDeclaredWidth();
        if (width != null) width.accept(getVisitor());
    }
}
