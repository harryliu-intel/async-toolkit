package com.avlsi.csp.util;

import java.lang.reflect.Constructor;
import java.math.BigInteger;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.stream.Collectors;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.util.container.Pair;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.csp.util.RefinementResolver;
import com.avlsi.csp.util.VariableAnalyzer;
import com.avlsi.csp.util.VariableAnalysisException;
import com.avlsi.util.math.BigIntegerUtil;

public class FunctionPreprocessor extends VisitorByCategory {
    private class Usage {
        private final Declarator decl;
        private final List<AbstractASTNode> write;
        private final List<AbstractASTNode> read;
        public Usage(final Declarator decl) {
            this.decl = decl;
            this.write = new ArrayList<AbstractASTNode>();
            this.read = new ArrayList<AbstractASTNode>();
        }
        public void addWrite(final AbstractASTNode node) {
            write.add(node);
        }
        public void addRead(final AbstractASTNode node) {
            read.add(node);
        }
        public List<AbstractASTNode> getWrites() {
            return write;
        }
        public boolean isAssigned() {
            return !write.isEmpty();
        }
    }

    private AbstractASTNodeInterface result;
    private SymbolTable<Usage> table;

    private int tempVars = 0;
    private LinkedList/*<StatementInterface>*/ preamble;
    private LinkedList/*<LinkedList<StatementInterface>>*/ preambleStack;
    private List/*<StatementInterface>*/ initializers;
    private boolean simpleAssign;
    private boolean inGuard = false;
    private boolean formalParamList = false;
    private boolean isLvalue = false;
    private CSPProgram ncsp;
    private CSPProgram currentProgram;
    private final VariableAnalyzer analyzer;
    private VariableAnalyzer.Results analysisResults;
    private Map<FunctionDeclaration,List<Usage>> copiesNeeded;

    private static final ResourceBundle INVALID = new NullResourceBundle();
    private static ResourceBundle resourceBundle = null;
    private final Collection<Problem> problems;

    /**
     * A list of arithmetic operators for reporting purposes.  It's also
     * overloaded to mean that processing is in the context of Cast2RTL.
     **/
    private final Collection<ArithmeticOperator> arithOps;

    public static ResourceBundle getResourceBundle() {
        if (resourceBundle == null) {
            try {
                resourceBundle = ResourceBundle.getBundle(
                        "com.avlsi.csp.resources.preprocessor");
            } catch (MissingResourceException e) {
                System.err.println(
                        "Cannot load function preprocessor error messages.");
                resourceBundle = INVALID;
            }
        }
        return resourceBundle;
    }

    private static class AssignToInput extends SimpleProblem {
        public AssignToInput(final AbstractASTNode node, final String func,
                             final Declarator param) {
            super("preprocessor.assign.to.input", node.getParseRange(),
                  new Object[] { param.getIdentifier().getIdentifier(), func });
        }
        public String getMessage() {
            final ResourceBundle rb = getResourceBundle();
            if (rb != INVALID) {
                try {
                    return MessageFormat.format(rb.getString(getCode()), args);
                } catch (MissingResourceException e) { }
            }

            return errorCode;
        }
    }

    public FunctionPreprocessor(
            final CSPCellInfo cellInfo,
            final Collection<ArithmeticOperator> arithOps) {
        this(new VariableAnalyzer(cellInfo), new ArrayList<Problem>(),
             arithOps);
        copiesNeeded = new HashMap<FunctionDeclaration,List<Usage>>();
    }

    private FunctionPreprocessor(
            final VariableAnalyzer analyzer,
            final Collection<Problem> problems,
            final Collection<ArithmeticOperator> arithOps) {
        this.analyzer = analyzer;
        this.problems = problems;
        this.arithOps = arithOps;
        preamble = new LinkedList/*<StatementInterface>*/();
        preambleStack = new LinkedList/*<LinkedList<StatementInterface>>*/();
        initializers = new ArrayList();
    }

    private FunctionPreprocessor(
            final RefinementResolver resolver,
            final VariableAnalyzer analyzer,
            final int tempVars,
            final Map<FunctionDeclaration,List<Usage>> copiesNeeded,
            final Collection<Problem> problems,
            final Collection<ArithmeticOperator> arithOps) {
        this(analyzer, problems, arithOps);
        this.resolver = resolver;
        this.tempVars = tempVars;
        this.copiesNeeded = copiesNeeded;
    }
    
    protected void setResult(AbstractASTNodeInterface node) {
        result = node;
    }

    public AbstractASTNodeInterface getResult() {
        return result;
    }

    public Collection<Problem> getProblems() {
        return problems;
    }

    private RefinementResolver resolver = null;

    private IdentifierExpression gensym() {
        return new IdentifierExpression("temp$" + tempVars++);
    }

    private void clearPreamble() {
        preamble.clear();
        // care must be taken in reusing temporary variables, because of
        // parallel statements
        //tempVars = 0;
    }

    private void pushPreamble() {
        preambleStack.addFirst(preamble);
        preamble = new LinkedList/*<StatementInterface>*/();
    }

    private void popPreamble() {
        preamble = (LinkedList) preambleStack.removeFirst();
    }

    protected VisitorInterface getVisitor() {
        return this;
    }

    protected void
    processDeclarator(Declarator d) throws VisitorException {
        if (table != null) {
            table.bind(d.getIdentifier().getIdentifier(),
                       formalParamList ? new Usage(d) : null);
        }

        final ExpressionInterface init = d.getInitializer();
        if (init == null) {
            setResult(d);
        } else {
            init.accept(getVisitor());
            final ExpressionInterface newInit;
            if (getResult() instanceof IntegerExpression) {
                newInit = (ExpressionInterface) getResult();
            } else {
                newInit = null;
                for (Iterator i = preamble.iterator(); i.hasNext(); ) {
                    final StatementInterface s = (StatementInterface) i.next();
                    if (!(s instanceof VarStatement)) {
                        i.remove();
                        initializers.add(s);
                    }
                }
                initializers.add(
                    new AssignmentStatement(d.getIdentifier(),
                                            (ExpressionInterface) getResult())
                    .epr(d));
            }
            setResult(new Declarator(d.getIdentifier(),
                                     d.getTypeFragment(),
                                     newInit,
                                     d.getDirection()));
        }
    }

    protected void
    processDeclaratorList(final DeclaratorList dclr) throws VisitorException {
        final DeclaratorList ndclr = new DeclaratorList();
        boolean same = true;
        for (Iterator j = dclr.getDeclarators(); j.hasNext(); ) {
            final Declarator d = (Declarator) j.next();
            processDeclarator(d);
            same &= d == getResult();
            ndclr.addDeclarator((Declarator) getResult());
        }
        setResult(same ? dclr : ndclr);
    }

    protected void
    processDeclarationList(DeclarationList l) throws VisitorException {
        final DeclarationList nl = new DeclarationList();
        boolean same = true;
        for (Iterator i = l.getDeclarations(); i.hasNext(); ) {
            final Declaration decl = (Declaration) i.next();
            processDeclaratorList(decl.getDeclaratorList());
            if (decl.getDeclaratorList() == getResult()) {
                nl.addDeclaration(decl);
            } else {
                same = false;
                nl.addDeclaration(
                        new Declaration((DeclaratorList) getResult()));
            }
        }
        setResult(same ? l : nl);
    }

    protected void
    processFunctionDeclaration(FunctionDeclaration decl)
    throws VisitorException {
        final SymbolTable<Usage> usages = new SymbolTable<Usage>();
        table = usages;
        formalParamList = true;
        processDeclarationList(decl.getFormals());
        formalParamList = false;
        final Type rtype = decl.getReturnType();
        if (rtype != null) rtype.accept(getVisitor());
        decl.getBodyStatement().accept(getVisitor());
        setResult(new FunctionDeclaration(decl.getName(),
                                          decl.getFormals(),
                                          rtype,
                                          (StatementInterface) getResult())
                  .epr(decl));
        final List<Usage> copyNeeded = new ArrayList<Usage>();
        (new DeclarationProcessor() {
            public void process(final Declarator d) throws VisitorException {
                copyNeeded.add(
                    usages.lookup(d.getIdentifier().getIdentifier()));
            }
        }).process(decl.getFormals());
        copiesNeeded.put(decl, copyNeeded);
        table = null;
    }
    protected void
    processStructureDeclaration(StructureDeclaration decl)
    throws VisitorException {
        //processDeclarationList(decl.getDeclarations());
        setResult(decl);
    }
    protected void
    processRefinementParent(CSPProgram p) throws VisitorException {
        final FunctionPreprocessor prep =
            new FunctionPreprocessor(resolver, analyzer, tempVars,
                                     copiesNeeded, problems, arithOps);
        prep.visitCSPProgram(p);
        setResult(prep.getResult());
        tempVars = prep.tempVars;
    }
    private class SortFunctions extends VisitorByCategory {
        private final Set<FunctionDeclaration> seen;
        public SortFunctions(final Set<FunctionDeclaration> seen) {
            this.seen = seen;
        }
        public void processFunctionDeclaration(FunctionDeclaration decl)
        throws VisitorException {
            if (!seen.contains(decl)/* && !copiesNeeded.containsKey(decl)*/) {
                super.processFunctionDeclaration(decl);
                seen.add(decl);
            }
        }
        public void visitFunctionCallExpression(FunctionCallExpression e)
        throws VisitorException {
            final Pair p = (Pair) resolver.getResolvedFunctions().get(e);

            final DeclarationList list;
            final String name;
            final Object resolved = p == null ? null : p.getSecond();

            if (resolved instanceof FunctionDeclaration) {
                processFunctionDeclaration((FunctionDeclaration) resolved);
            }

            super.visitFunctionCallExpression(e);
        }
    }
    private void addAnalyzerErrors() {
        problems.addAll(
            analysisResults.getErrors(false)
                           .stream()
                           .filter(e -> e.getCode().equals("type.checker.implicit.pack.truncated"))
                           .collect(Collectors.toList()));
    }
    public void visitCSPProgram(CSPProgram p) throws VisitorException
    {
        if (resolver == null) {
            resolver = new RefinementResolver(RefinementResolver.NAME);
            resolver.resolve(p);
            problems.addAll(resolver.getProblems());
        }

        currentProgram = p;
        ncsp = new CSPProgram();
        for (Iterator i = p.getRefinementParents().iterator(); i.hasNext(); ) {
            processRefinementParent((CSPProgram) i.next());
            ncsp.refineFrom((CSPProgram) getResult());
        }

        final CSPProgram resolvedProgram = resolver.getCSPProgram();
        final SequentialStatement initStmt = p.getInitializerStatement();

        final LinkedHashSet<FunctionDeclaration> seen =
            new LinkedHashSet<FunctionDeclaration>();
        final SortFunctions sorter = new SortFunctions(seen);
        for (Iterator i = p.getFunctionDeclarations(); i.hasNext(); ) {
            final FunctionDeclaration decl = (FunctionDeclaration) i.next();
            if (resolver.isUsed(decl)) {
                sorter.processFunctionDeclaration(decl);
            }
        }

        for (FunctionDeclaration decl : seen) {
            if (resolver.isUsed(decl)) {
                try {
                    analysisResults =
                        analyzer.getResults(decl, initStmt, resolver);
                    addAnalyzerErrors();
                } catch (VariableAnalysisException x) {
                    throw new VisitorException(x.getMessage(), x);
                }
                processFunctionDeclaration(decl);
            } else {
                setResult(decl);
            }
            ncsp.addFunctionDeclaration((FunctionDeclaration) getResult());
        }

        for (Iterator i = p.getStructureIterator(); i.hasNext(); ) {
            processStructureDeclaration((StructureDeclaration) i.next());
            ncsp.addStructureDeclaration((StructureDeclaration) getResult());
        }

        try {
            analysisResults = analyzer.getResults(p, resolver);
            addAnalyzerErrors();
        } catch (VariableAnalysisException x) {
            throw new VisitorException(x.getMessage(), x);
        }

        if (initStmt != null) {
            if (initStmt == resolvedProgram.getInitializerStatement()) {
                initStmt.accept(getVisitor());
            } else {
                setResult(initStmt);
            }
            ncsp.setInitializerStatement((SequentialStatement) getResult());
        }

        final StatementInterface stmt = p.getStatement();
        if (stmt != null) {
            if (stmt == resolvedProgram.getStatement()) {
                stmt.accept(getVisitor());
            } else {
                setResult(stmt);
            }
            ncsp.setStatement((StatementInterface) getResult());
        }
        setResult(ncsp);
    }

    private static AbstractASTNodeInterface
    constructNode(final AbstractASTNodeInterface node) {
        try {
            final AbstractASTNodeInterface ast =
                (AbstractASTNodeInterface) node.getClass().newInstance();
            return ast.epr(node);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static AbstractASTNodeInterface
    constructNode(final AbstractASTNodeInterface node,
                  final ExpressionInterface[] args) {
        try {
            final Class c = node.getClass();
            final Class[] argType = new Class[args.length];
            for (int i = 0; i < args.length; ++i)
                argType[i] = ExpressionInterface.class;
            final Constructor ctor = c.getConstructor(argType);
            return
                ((AbstractASTNodeInterface)
                    ctor.newInstance((Object[]) args)).epr(node);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static AbstractASTNodeInterface
    constructNode(final AbstractASTNodeInterface node,
                  final ExpressionInterface e) {
        return constructNode(node, new ExpressionInterface[] { e });
    }

    private static AbstractASTNodeInterface
    constructNode(final AbstractASTNodeInterface node,
                  final ExpressionInterface e1,
                  final ExpressionInterface e2) {
        return constructNode(node, new ExpressionInterface[] { e1, e2 });
    }

    private void addArithmeticOp(final ExpressionInterface e,
                                 final IntegerType ty) {
        if (!(e instanceof AbstractBinaryExpression)) return;

        final AbstractBinaryExpression abe = (AbstractBinaryExpression) e;
        final BigInteger lhs = CspUtils.getIntegerConstant(abe.getLeft());
        final BigInteger rhs = CspUtils.getIntegerConstant(abe.getRight());

        if (e instanceof MultiplyExpression) {
            if ((lhs == null || !BigIntegerUtil.isPowerOf2(lhs)) &&
                (rhs == null || !BigIntegerUtil.isPowerOf2(rhs))) {
                arithOps.add(new ArithmeticOperator(e, ty));
            }
        } else if (e instanceof DivideExpression ||
                   e instanceof RemainderExpression) {
            if (rhs == null || !BigIntegerUtil.isPowerOf2(rhs)) {
                arithOps.add(new ArithmeticOperator(e, ty));
            }
        } else if (e instanceof LeftShiftExpression ||
                   e instanceof RightShiftExpression) {
            if (rhs == null) arithOps.add(new ArithmeticOperator(e, ty));
        }
    }

    // if the expression type is integer, assign the previous result to a
    // temporary variable, and make that temporary variable the new result
    private void createTempInt(final ExpressionInterface e)
        throws VisitorException {
        if (analysisResults.getType(e) instanceof IntegerType &&
            !(e instanceof IdentifierExpression)) {
            final ExpressionInterface temp = gensym();
            final VarStatement stmt = 
                createVarStatement(temp, new TemporaryIntegerType()); 
            preamble.add(stmt);
            preamble.add(
                new AssignmentStatement(temp,
                                        (ExpressionInterface) getResult())
                .epr(e));
            temp.epr(e);
            setResult(temp);
            if (arithOps != null) {
                final IntegerType[] ty = new IntegerType[1];
                (new DeclarationProcessor() {
                    public void process(final Declarator d)
                        throws VisitorException {
                        ty[0] = (IntegerType) d.getTypeFragment();
                    }
                }).process(stmt.getDeclarationList());
                addArithmeticOp(e, ty[0]);
            }
        }
    }

    protected void
    processAbstractBinaryExpression(final AbstractBinaryExpression e)
        throws VisitorException {
        e.getLeft().accept(getVisitor());
        final AbstractASTNodeInterface left = getResult();
        e.getRight().accept(getVisitor());
        if (e.getLeft() == left && e.getRight() == getResult()) {
            setResult(e);
        } else {
            setResult(constructNode(e, (ExpressionInterface) left,
                                       (ExpressionInterface) getResult())
                      .epr(e));
        }

        if (arithOps != null) createTempInt(e);
    }

    // This function makes sure the two operands of the expression are both
    // identifiers.  Division and remainder require this, because Cast2RTL will
    // translate, for example, a % b as b == 0 ? a : a % b, and we do not want
    // to evaluate "b" twice.  Since the ternary operator is short-circuiting,
    // we must also evaluate "a", otherwise, it would be incorrect to translate
    // a / b as b == 0 ? 0 : a / b, since a might not be evaluated.
    private void makeTempOperands(final AbstractBinaryExpression e)
        throws VisitorException {
        e.getLeft().accept(getVisitor());
        createTempInt((ExpressionInterface) getResult());
        ExpressionInterface lhs = (ExpressionInterface) getResult();

        e.getRight().accept(getVisitor());
        createTempInt((ExpressionInterface) getResult());
        ExpressionInterface rhs = (ExpressionInterface) getResult();

        if (lhs == e.getLeft() && rhs == e.getRight()) {
            setResult(e);
        } else {
            setResult(constructNode(e, lhs, rhs));
        }
    }

    public void visitDivideExpression(DivideExpression e)
        throws VisitorException {
        if (arithOps == null) {
            super.visitDivideExpression(e);
        } else {
            makeTempOperands(e);
            createTempInt(e);
        }
    }

    public void visitRemainderExpression(RemainderExpression e)
        throws VisitorException {
        if (arithOps == null) {
            super.visitRemainderExpression(e);
        } else {
            makeTempOperands(e);
            createTempInt(e);
        }
    }

    private void processConditionalExpression(AbstractBinaryExpression e,
                                              boolean and)
        throws VisitorException {
        final ExpressionInterface val = gensym();
        preamble.add(createVarStatement(val, new BooleanType()));

        pushPreamble();
        e.getLeft().accept(getVisitor());
        ExpressionInterface guardExpr = (ExpressionInterface) getResult();
        if (!and) guardExpr = new NotExpression(guardExpr);
        final Collection guardPreamble = preamble;
        popPreamble();
        final SequentialStatement guardStmt = packagePreamble(guardPreamble);

        pushPreamble();
        e.getRight().accept(getVisitor());
        final Collection actionPreamble = preamble;
        popPreamble();
        final StatementInterface actionStmt =
            packagePreamble(
                actionPreamble,
                new AssignmentStatement(val,
                                        (ExpressionInterface) getResult()));
        actionStmt.epr(e.getRight());

        final GuardedCommand gc;
        if (guardStmt == null) {
            gc = new GuardedCommand(guardExpr, null, actionStmt);
        } else {
            guardStmt.epr(e.getLeft());
            gc = new GuardedCommandWithStatement(guardExpr, null, actionStmt,
                                                 guardStmt);
        }
        gc.epr(e.getLeft());
        gc.epr(e.getRight());

        final StatementInterface elseStmt =
            new AssignmentStatement(val, new BooleanExpression(!and));

        final AbstractGuardedStatement select =
            new DeterministicSelectionStatement();
        select.addGuardedCommand(gc);
        select.addElseStatement(elseStmt);
        select.epr(e);
        preamble.add(select);

        setResult(val);
    }

    public void visitConditionalAndExpression(ConditionalAndExpression e)
        throws VisitorException {
        processConditionalExpression(e, true);
    }
    public void visitConditionalOrExpression(ConditionalOrExpression e)
        throws VisitorException {
        processConditionalExpression(e, false);
    }

    protected void
    processAbstractChannelExpression(final AbstractChannelExpression e)
        throws VisitorException {
        e.getChannelExpression().accept(getVisitor());
        if (inGuard) {
            if (getResult() == e.getChannelExpression()) setResult(e);
            else setResult(constructNode(e, (ExpressionInterface) getResult()));
        } else {
            final ExpressionInterface temp = gensym();
            final Type ty =
                e instanceof ProbeExpression ? new BooleanType()
                                             : new TemporaryIntegerType();
            preamble.add(createVarStatement(temp, ty));
            preamble.add(new AssignmentStatement(temp, 
                (AbstractChannelExpression)
                constructNode(e, (ExpressionInterface) getResult())).epr(e));
            setResult(temp);
        }
    }

    public void visitReceiveExpression(final ReceiveExpression e)
        throws VisitorException {
        if (inGuard) {
            throw new VisitorException("Receive expressions not supported in guard expression at " + e.getParseRange().fullString());
        }

        e.getChannelExpression().accept(getVisitor());
        final ExpressionInterface temp = gensym();
        preamble.add(createVarStatement(temp, new IntegerType())); 
        preamble.add(new ReceiveStatement((ExpressionInterface) getResult(),
                                          temp).epr(e));
        setResult(temp);
    }

    /**
     * Package the given statement with any preambles necessary into a
     * sequential statement and return it.  Any preamble is cleared.  If there
     * is no preamble, return the argument with no modification.
     **/
    private SequentialStatement packagePreamble() {
        if (preamble.isEmpty()) {
            return null;
        } else {
            final SequentialStatement seq = new SequentialStatement();
            for (Iterator i = preamble.iterator(); i.hasNext(); ) {
                seq.addStatement((StatementInterface) i.next());
            }
            clearPreamble();
            return seq;
        }
    }

    private StatementInterface packagePreamble(final StatementInterface s)
    {
        final SequentialStatement seq = packagePreamble();
        if (seq == null) {
            return s;
        } else {
            seq.addStatement(s);
            seq.epr(s);
            return seq;
        }
    }

    private SequentialStatement packagePreamble(final Collection somePreamble) {
        if (somePreamble.isEmpty()) {
            return null;
        } else {
            final SequentialStatement seq = new SequentialStatement();
            for (Iterator i = somePreamble.iterator(); i.hasNext(); ) {
                final StatementInterface stmt = (StatementInterface) i.next();
                if (stmt instanceof VarStatement) {
                    preamble.add(stmt);
                } else {
                    seq.addStatement(stmt);
                }
            }
            return seq;
        }
    }

    private StatementInterface packagePreamble(final Collection somePreamble,
                                               final StatementInterface s) {
        final SequentialStatement seq = packagePreamble(somePreamble);
        if (seq == null) {
            return s;
        } else {
            seq.addStatement(s);
            seq.epr(s);
            return seq;
        }
    }

    public void visitReceiveStatement(ReceiveStatement s)
        throws VisitorException {
        s.getChannelExpression().accept(getVisitor());
        final ExpressionInterface chanExpr = (ExpressionInterface) getResult();
        ExpressionInterface rhs = s.getRightHandSide();
        final StatementInterface last;
        if (rhs == null) {
            last = (AbstractChannelStatement) constructNode(s, chanExpr, null);
        } else {
            final Type ty = analysisResults.getType(rhs);
            if (ty instanceof StructureType || ty instanceof ArrayType) {
                final ExpressionInterface temp = gensym();
                preamble.add(createVarStatement(temp, new IntegerType())); 
                preamble.add(new ReceiveStatement(chanExpr, temp).epr(s));
                final FunctionCallExpression funcall =
                    new FunctionCallExpression(
                        new IdentifierExpression("unpack"));
                funcall.addActual(rhs);
                funcall.addActual(temp);
                funcall.epr(s);
                funcall.accept(getVisitor());
                last = (StatementInterface)
                    new ExpressionStatement(
                        (ExpressionInterface) getResult()).epr(s);
            } else {
                isLvalue = true;
                rhs.accept(getVisitor());
                isLvalue = false;
                last = (AbstractChannelStatement)
                    constructNode(s, chanExpr,
                                  (ExpressionInterface) getResult());
            }
        }
        setResult(packagePreamble(last));
    }

    public void visitSendStatement(SendStatement s) throws VisitorException {
        s.getChannelExpression().accept(getVisitor());
        final ExpressionInterface chanExpr = (ExpressionInterface) getResult();
        ExpressionInterface rhs = s.getRightHandSide();
        if (rhs == null) {
            setResult(null);
        } else {
            final Type ty = analysisResults.getType(rhs);
            if (ty instanceof StructureType || ty instanceof ArrayType) {
                final FunctionCallExpression funcall =
                    new FunctionCallExpression(
                        new IdentifierExpression("pack"));
                funcall.addActual(rhs);
                funcall.epr(rhs);
                rhs = funcall;
            }
            rhs.accept(getVisitor());
            // the emitters depend on the RHS being a simple identifier,
            // because it might be evaluated multiple times
            if (!(getResult() instanceof IdentifierExpression)) {
                final ExpressionInterface temp = gensym();
                preamble.add(createVarStatement(temp,
                    ty instanceof BooleanType ? new BooleanType()
                                              : new TemporaryIntegerType())); 
                preamble.add(
                    new AssignmentStatement(temp,
                                            (ExpressionInterface) getResult())
                    .epr(rhs));
                setResult(temp);
            }
        }
        setResult(
            packagePreamble(
                (AbstractChannelStatement)
                constructNode(s, chanExpr, (ExpressionInterface) getResult())));
    }

    private void addInitializers(final AbstractCompositeStatement ns) {
        for (Iterator i = preamble.iterator(); i.hasNext(); ) {
            final StatementInterface s = (StatementInterface) i.next();
            ns.addStatement(s);
        }
        clearPreamble();
        for (Iterator i = initializers.iterator(); i.hasNext(); ) {
            ns.addStatement((StatementInterface) i.next());
        }
        initializers.clear();
    }

    protected void
    processAbstractCompositeStatement(final AbstractCompositeStatement s)
    throws VisitorException {
        final AbstractCompositeStatement ns =
            (AbstractCompositeStatement) constructNode(s);
        for (Iterator i = s.getStatements(); i.hasNext(); ) {
            final StatementInterface ss = (StatementInterface) i.next();
            if (!(ss instanceof VarStatement)) addInitializers(ns);
            ss.accept(getVisitor());
            ns.addStatement((StatementInterface) getResult());
        }
        addInitializers(ns);
        setResult(ns);
    }

    public void visitSequentialStatement(final SequentialStatement s)
        throws VisitorException {
        if (table != null) table.enterScope();
        processAbstractCompositeStatement(s);
        if (table != null) table.leaveScope();
    }

    protected void
    processAbstractUnaryExpression(final AbstractUnaryExpression e)
    throws VisitorException {
        e.getExpression().accept(getVisitor());
        if (e.getExpression() == getResult()) {
            setResult(e);
        } else {
            setResult((AbstractUnaryExpression)
                      constructNode(e, (ExpressionInterface) getResult()));
        }

        if (arithOps != null) createTempInt(e);
    }

    /* Primary expressions */
    protected void processPrimaryExpression(ExpressionInterface e)
    throws VisitorException {
        setResult(e);
    }

    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException {
        final Usage u = table == null ? null 
                                      : table.lookup(e.getIdentifier());
        if (isLvalue) {
            if (u != null) u.addWrite(e);
            isLvalue = false;
        } else {
            if (u != null) u.addRead(e);
        }
        processPrimaryExpression(e);
    }

    public void visitArrayAccessExpression(ArrayAccessExpression e)
        throws VisitorException {
        e.getArrayExpression().accept(getVisitor());
        final ExpressionInterface arrayExpr = (ExpressionInterface) getResult();
        e.getIndexExpression().accept(getVisitor());
        if (!(getResult() instanceof IntegerExpression))
            createTempInt((ExpressionInterface) getResult());
        if (e.getArrayExpression() == arrayExpr &&
            e.getIndexExpression() == getResult()) {
            setResult(e);
        } else {
            setResult(new ArrayAccessExpression(arrayExpr, (ExpressionInterface)
                                                getResult())
                      .epr(e));
        }
    }

    public void visitBitRangeExpression(BitRangeExpression e)
        throws VisitorException {
        e.getBitsExpression().accept(getVisitor());
        final ExpressionInterface bitsExpr = (ExpressionInterface) getResult();
        if (e.getMinExpression() == null) setResult(null);
        else {
            e.getMinExpression().accept(getVisitor());
            if (!(getResult() instanceof IntegerExpression))
                createTempInt((ExpressionInterface) getResult());
        }
        final ExpressionInterface minExpr = (ExpressionInterface) getResult();
        e.getMaxExpression().accept(getVisitor());
        if (!(getResult() instanceof IntegerExpression))
            createTempInt((ExpressionInterface) getResult());
        if (e.getBitsExpression() == bitsExpr &&
            e.getMinExpression() == minExpr &&
            e.getMaxExpression() == getResult()) {
            setResult(e);
        } else {
            setResult(new BitRangeExpression(bitsExpr, minExpr,
                                             (ExpressionInterface) getResult())
                      .epr(e));
        }
    }

    private boolean isAssignable(final AbstractASTNodeInterface n) {
        return n instanceof IdentifierExpression ||
               n instanceof ArrayAccessExpression ||
               n instanceof StructureAccessExpression ||
               n instanceof MemberAccessExpression;
    }

    private static IntegerType getIntArrayBaseType(final Type ty) {
        if (ty instanceof ArrayType) {
            final Type baseType = CspUtils.getBaseType(ty);
            if (baseType instanceof IntegerType) {
                return (IntegerType) baseType;
            }
        }
        return null;
    }

    public void visitFunctionCallExpression(FunctionCallExpression e)
        throws VisitorException {
        // Find function call declaration using refinement resolver
        // to determine types of arguments and return value
        final Pair p = (Pair) resolver.getResolvedFunctions().get(e);

        final DeclarationList list;
        final String name;
        final List<Usage> copyNeeded;
        final Object resolved = p == null ? null : p.getSecond();

        e.getFunctionExpression().accept(getVisitor());
        final FunctionCallExpression funcall =
            new FunctionCallExpression((ExpressionInterface) getResult());
        funcall.epr(e);

        boolean specialNoCopy = false;
        if (resolved instanceof FunctionDeclaration) {
            final FunctionDeclaration decl = (FunctionDeclaration) resolved;
            list = decl.getFormals();
            name = decl.getName();
            copyNeeded = copiesNeeded.get(decl);
        } else if (resolved instanceof StructureDeclaration) {
            final StructureDeclaration decl = (StructureDeclaration) resolved;
            list = decl.getDeclarations();
            name = decl.getName();
            copyNeeded = null;
        } else {
            list = null;
            final ExpressionInterface funExpr = funcall.getFunctionExpression();
            if (funExpr instanceof IdentifierExpression) {
                name = ((IdentifierExpression) funExpr).getIdentifier();
                specialNoCopy = name.equals("print") || name.equals("pack") ||
                                name.equals("string");
            } else {
                name = null;
            }
            copyNeeded = null;
        }

        final Iterator types;
        if (list == null) {
            types = null;
        } else {
            final Collection c = new ArrayList();
            (new DeclarationProcessor() {
                public void process(final Declarator d)
                    throws VisitorException {
                    c.add(d);
                }
            }).process(list);
            types = c.iterator();
        }

        List<AbstractASTNodeInterface> copyback = new ArrayList<>();
        int count = 0;
        for (Iterator i = e.getActuals(); i.hasNext(); ++count) {
            final ExpressionInterface arg = (ExpressionInterface) i.next();
            final Type currType = analysisResults.getType(arg);
            final IntegerType currElemType = getIntArrayBaseType(currType);

            final Declarator d;
            final int dir;
            final IntegerType formElemType;
            if (types == null) {
                d = null;
                dir = Declarator.NONE;
                formElemType = null;
            } else {
                if (types.hasNext()) {
                    d = (Declarator) types.next();
                    dir = d.getDirection();
                    formElemType = getIntArrayBaseType(d.getTypeFragment());
                } else throw new VisitorExceptionWithLocation(
                        "More arguments to function " + name +
                        " than expected (expecting " + count + ")",
                        arg);
            }

            final boolean diffWidthIntArray =
                currElemType != null &&
                formElemType != null &&
                CspUtils.getIntegerConstant(currElemType.getDeclaredWidth()) !=
                    CspUtils.getIntegerConstant(formElemType.getDeclaredWidth());

            final Usage u = copyNeeded == null ? null : copyNeeded.get(count);
            final boolean needCopy =
                (u == null ? true : (dir == Declarator.IN && u.isAssigned()));
            if (needCopy && u != null) {
                for (AbstractASTNode node : u.getWrites()) {
                    problems.add(new AssignToInput(node, name, d));
                }
            }

            final boolean larg = dir != Declarator.IN && !specialNoCopy &&
                                 isAssignable(arg);
            if (larg) isLvalue = true;
            arg.accept(getVisitor());
            if (larg) isLvalue = false;

            final ExpressionInterface actual;
            if ((dir != Declarator.IN || !needCopy) &&
                isAssignable(getResult()) &&
                !diffWidthIntArray) {
                actual = (ExpressionInterface) getResult();
            } else {
                actual = gensym(); // temp var for actual
                // create var statement for actual for resolved
                // functions
                if (diffWidthIntArray) {
                    final ArrayType aty = (ArrayType) currType;
                    preamble.add(
                            createVarStatement(actual,
                                CspUtils.setBaseType(aty, formElemType)));
                    if (larg) {
                        copyback.add(
                            new AssignmentStatement(
                                (ExpressionInterface) getResult(), actual)
                            .epr(arg));
                    }
                } else if (currType != null) {
                    final Type varType =
                        currType instanceof IntegerType ? new TemporaryIntegerType()
                                                        : currType;
                    preamble.add(createVarStatement(actual, varType));
                }
                actual.epr(arg);
                preamble.add(
                    new AssignmentStatement(actual,
                                            (ExpressionInterface) getResult())
                    .epr(arg));
            }
            funcall.addActual(actual);
        }
        if (types != null && types.hasNext()) {
            throw new VisitorExceptionWithLocation(
                    "Fewer actual arguments to function " + name +
                    " than expected (found " + count + ")", funcall);
        }

        // Create return variable 
        final ExpressionInterface temp = gensym();
        // And process its type 
        if (resolved instanceof StructureDeclaration) { 
            setResult(funcall);
        } else {
            boolean noReturn = false;
            if (resolved instanceof FunctionDeclaration) {
                final FunctionDeclaration decl2 =  
                          (FunctionDeclaration) p.getSecond();
                // Find return type of function 
                final Type type = decl2.getReturnType();
                // Then create var statement and add it to program
                if (type == null) noReturn = true;
                else preamble.add(createVarStatement(temp, type)); 
            } else if (name.equals("string")) {
                preamble.add(createVarStatement(temp, new StringType()));
            } else if (name.equals("print") || name.equals("assert") ||
                       name.equals("cover") || name.equals("unpack")) {
                noReturn = true;
            } else if (name.equals("pack")) {
                preamble.add(createVarStatement(temp,
                                                new TemporaryIntegerType())); 
            }

            if (noReturn) {
                if (copyback.isEmpty()) {
                    setResult(funcall);
                } else {
                    preamble.add(new ExpressionStatement(funcall).epr(e));
                    preamble.addAll(copyback);
                    setResult(new BooleanExpression(true).epr(e));
                }
            } else {
                preamble.add(new AssignmentStatement(temp, funcall).epr(e));
                preamble.addAll(copyback);
                setResult(temp);
            }
        }
    }

    // Create var statement from id 
    private VarStatement createVarStatement(ExpressionInterface temp, 
        Type type) {
        final IdentifierList idlist = new IdentifierList();
        idlist.addIdentifier((IdentifierExpression) temp);
        final Type cloned = Type.clone(type);
        return(new VarStatement(new Declaration(idlist, cloned))); 
    }
 
    public void visitMemberAccessExpression(MemberAccessExpression e)
        throws VisitorException {
        e.getStructureExpression().accept(getVisitor());
        if (e.getStructureExpression() == getResult()) {
            setResult(e);
        } else {
            setResult(new MemberAccessExpression(
                        (ExpressionInterface) getResult(), e.getMemberName())
                      .epr(e));
        }
    }
    public void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException {
        e.getStructureExpression().accept(getVisitor());
        if (e.getStructureExpression() == getResult()) {
            setResult(e);
        } else {
            setResult(new StructureAccessExpression(
                        (ExpressionInterface) getResult(), e.getFieldName())
                      .epr(e));
        }
    }

    /* Loop */
    public void visitLinkageLoopTerm(LinkageLoopTerm term)
        throws VisitorException {
        processRange(term.getRange());
        final Range r = (Range) getResult();

        if (table != null) {
            table.enterScope();
            table.bind(term.getIndexVar(), null);
        }
        term.getTerm().accept(getVisitor());
        if (table != null) table.leaveScope();

        if (term.getRange() == r && term.getTerm() == getResult()) {
            setResult(term);
        } else {
            setResult(new LinkageLoopTerm(term.getIndexVar(), r,
                                          (LinkageTermInterface) getResult())
                      .epr(term));

        }
    }

    public void visitLoopExpression(LoopExpression e) throws VisitorException {
        processRange(e.getRange());
        final Range r = (Range) getResult();

        if (table != null) {
            table.enterScope();
            table.bind(e.getIndexVar(), null);
        }
        e.getExpression().accept(getVisitor());
        if (table != null) table.leaveScope();

        if (e.getRange() == r && e.getExpression() == getResult()) {
            setResult(e);
        } else {
            setResult(new LoopExpression(e.getIndexVar(), r,
                                         e.getSeparator(),
                                         (ExpressionInterface) getResult())
                      .epr(e));

        }
    }

    public void visitLoopStatement(LoopStatement s) throws VisitorException {
        processRange(s.getRange());
        final Range r = (Range) getResult();

        pushPreamble();
        if (table != null) {
            table.enterScope();
            table.bind(s.getIndexVar(), null);
        }
        s.getStatement().accept(getVisitor());
        if (table != null) table.leaveScope();
        popPreamble();

        if (s.getRange() == r && s.getStatement() == getResult()) {
            setResult(s);
        } else {
            final LoopStatement ls = (LoopStatement)
                new LoopStatement(s.getIndexVar(), r, s.getSeparator(),
                                  (StatementInterface) getResult()).epr(s);
            setResult(packagePreamble(ls));
        }
    }

    protected void processGuardedCommandInterface(GuardedCommandInterface gci)
        throws VisitorException {
        if (gci instanceof GuardedCommand) {
            final GuardedCommand gc = (GuardedCommand) gci;
            inGuard = true;
            pushPreamble();
            gc.getGuard().accept(getVisitor());
            ExpressionInterface ng = (ExpressionInterface) getResult();
            inGuard = false;
            final Collection guardPreamble = preamble;
            popPreamble();

            final SequentialStatement guardStmt;
            if (guardPreamble.isEmpty()) {
                guardStmt = null;
            } else {
                guardStmt = new SequentialStatement();
                for (Iterator i = guardPreamble.iterator(); i.hasNext(); ) {
                    final StatementInterface stmt =
                        (StatementInterface) i.next();
                    if (stmt instanceof VarStatement) {
                        preamble.add(stmt);
                    } else {
                        guardStmt.addStatement(stmt);
                    }
                }
            }

            pushPreamble();
            gc.getCommand().accept(getVisitor());
            final StatementInterface cmd =
                packagePreamble((StatementInterface) getResult());
            popPreamble();
            if ((gc.getCommand() != cmd) || (gc.getGuard() != ng)) {
                if (guardStmt == null) {
                    setResult(new GuardedCommand(ng, 
                                                 gc.getLinkageTerms(),
                                                 cmd)
                              .epr(gci));
                } else {
                    guardStmt.epr(ng);
                    setResult(new GuardedCommandWithStatement(
                                ng, gc.getLinkageTerms(), cmd, guardStmt)
                              .epr(gci));
                }
                return;
            }
        }
        setResult(gci);
    }

    public void visitLoopGuard(LoopGuard s) throws VisitorException {
        // should have been unrolled by ConstantEvaluator
        setResult(s);
    }

    /* Assignment and variable declaration statements */
    public void visitAssignmentStatement(AssignmentStatement s)
        throws VisitorException {
        isLvalue = true;
        s.getLeftHandSide().accept(getVisitor());
        isLvalue = false;
        final ExpressionInterface lhs = (ExpressionInterface) getResult();
        s.getRightHandSide().accept(getVisitor());
        final ExpressionInterface rhs = (ExpressionInterface) getResult();
        
        if (s.getLeftHandSide() == lhs && s.getRightHandSide() == rhs) {
            setResult(s);
        } else {
            final AssignmentStatement as = (AssignmentStatement)
                new AssignmentStatement(lhs, rhs, s.getKind()).epr(s);
            setResult(packagePreamble(as));
        }
    }
    public void visitIncDecStatement(IncDecStatement s)
        throws VisitorException {
        new AssignmentStatement(
            s.getExpression(), new IntegerExpression(1),
            s.isIncrement() ? AssignmentStatement.ADD
                            : AssignmentStatement.SUBTRACT)
        .accept(getVisitor());
    }
    public void visitExpressionStatement(ExpressionStatement s)
        throws VisitorException {
        s.getExpression().accept(getVisitor());
        final StatementInterface si;
        if (getResult() instanceof IdentifierExpression ||
            getResult() instanceof IntegerExpression) {
            si = (StatementInterface) new SkipStatement().epr(s);
        } else {
            si = (StatementInterface) new ExpressionStatement(
                    (ExpressionInterface) getResult()).epr(s.getExpression());
        }
        setResult(packagePreamble(si).epr(s));
    }
    public void visitVarStatement(VarStatement s) throws VisitorException {
        processDeclarationList(s.getDeclarationList());
        final DeclarationList dlst = (DeclarationList) getResult();

        final StatementInterface stmt = s.getStatement();
        if (stmt == null) {
            setResult(null);
        } else {
            stmt.accept(getVisitor());
        }
        setResult(new VarStatement(dlst,
                                   (StatementInterface) getResult()).epr(s));
    }

    // XXX: Figure out what boolean same var is used for. Completely unclear
    // at first glance.
    protected void
    processAbstractGuardedStatement(AbstractGuardedStatement s)
    throws VisitorException {
        final AbstractGuardedStatement ags =
            (AbstractGuardedStatement) constructNode(s);
        boolean same = true;
        final StatementInterface els = s.getElseStatement();
        for (Iterator i = s.getGuardedCommands(); i.hasNext(); ) {
            final GuardedCommandInterface gci =
                (GuardedCommandInterface) i.next();
            processGuardedCommandInterface(gci);
            ags.addGuardedCommand((GuardedCommandInterface) getResult());
            same &= gci == getResult();
        }
        if (els != null) {
            pushPreamble();
            els.accept(getVisitor());
            ags.addElseStatement(
                    packagePreamble((StatementInterface) getResult()));
            popPreamble();
            same &= els == getResult();
        }
        if (same) setResult(s);
        else setResult(ags.epr(s));
    }

    protected void
    processAbstractSelectionStatement(AbstractSelectionStatement s)
    throws VisitorException {
        processAbstractGuardedStatement(s);
    }

    protected void
    processAbstractRepetitionStatement(AbstractRepetitionStatement s)
    throws VisitorException {
        processAbstractGuardedStatement(s);
    }

    public void
        visitNonDeterministicRepetitionStatement
        (NonDeterministicRepetitionStatement s) throws VisitorException
    {
        pushPreamble();
        processAbstractRepetitionStatement(s);
        if (getResult() != s) {
            final NonDeterministicRepetitionStatement ndrs =
                (NonDeterministicRepetitionStatement) getResult();
            ndrs.setNeutralState(s.getNeutralState());
        }
        setResult(packagePreamble((StatementInterface) getResult()));
        popPreamble();
    }

    public void
        visitNonDeterministicSelectionStatement
        (NonDeterministicSelectionStatement s) throws VisitorException {
        pushPreamble();
        processAbstractSelectionStatement(s);
        if (getResult() != s) {
            final NonDeterministicSelectionStatement ndrs =
                (NonDeterministicSelectionStatement) getResult();
            ndrs.setNeutralState(s.getNeutralState());
        }
        setResult(packagePreamble((StatementInterface) getResult()));
        popPreamble();
    }

    public void
        visitDeterministicRepetitionStatement
        (DeterministicRepetitionStatement s) throws VisitorException
    {
        pushPreamble();
        processAbstractRepetitionStatement(s);
        setResult(packagePreamble((StatementInterface) getResult()));
        popPreamble();
    }

    public void
        visitDeterministicSelectionStatement
        (DeterministicSelectionStatement s) throws VisitorException
    {
        pushPreamble();
        processAbstractSelectionStatement(s);
        setResult(packagePreamble((StatementInterface) getResult()));
        popPreamble();
    }


    /* LinkageExpressionInterface */
    protected void
    processLinkageExpressionInterface(LinkageExpressionInterface e)
        throws VisitorException {
        setResult(e);
    }
    public void
    visitLinkageArrayAccessExpression(LinkageArrayAccessExpression e)
        throws VisitorException {
        processLinkageExpressionInterface(e);
    }
    public void visitLinkageExpressionTerm(LinkageExpressionTerm term)
        throws VisitorException {
        term.getExpression().accept(getVisitor());
        if (term.getExpression() == getResult()) {
            setResult(term);
        } else {
            setResult(new LinkageExpressionTerm(
                        (LinkageExpressionInterface) getResult(),
                        term.isInverted())
                      .epr(term));
        }
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
        final ExpressionInterface minExpr = (ExpressionInterface) getResult();
        r.getMaxExpression().accept(getVisitor());
        if (r.getMinExpression() == minExpr &&
            r.getMaxExpression() == getResult()) {
            setResult(r);
        } else {
            setResult(new Range(minExpr, (ExpressionInterface) getResult())
                      .epr(r));
        }
    }

    protected void processLinkageTerms(final LinkageTerms terms)
        throws VisitorException {
        for (Iterator i = terms.getTerms(); i.hasNext(); ) {
            ((LinkageTermInterface) i.next()).accept(getVisitor());
        }
    }

    /* Trivial statements */
    public void visitErrorStatement(ErrorStatement s) throws VisitorException {
        setResult(s);
    }

    public void visitSkipStatement(SkipStatement s) throws VisitorException {
        setResult(s);
    }

    /* Currently unused */
    public void visitIdentifierList(IdentifierList il) throws VisitorException {
        setResult(il);
    }

}
