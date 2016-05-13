//
// Copyright 2000 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

//
// CAST Tree Parser Grammar (for use with antlr http://www.antlr.org/)
//
// author: Jesse Rosenstock
//

header {
    ///////////////////////////////////////////////////////////////////////
    //
    // Copyright 2000 Asynchronous Digital Design.  All rights reserved.
    //
    // Warning:  This file was AUTOMATICALLY GENERATED!!!
    //
    // DO NOT check in.
    // DO NOT modify.
    //
    // You want to modify CastTwoTree.g instead.
    //
    // TODO: make the AST factory a singleton member to save initialization
    //       costs
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;

    import antlr.ASTFactory;
    import antlr.TokenStreamException;
    import antlr.TokenStreamSelector;
    import java.io.FileNotFoundException;
    import java.io.IOException;
    import java.io.StringReader;
    import java.math.BigDecimal;
    import java.math.BigInteger;
    import java.util.*;
    import com.avlsi.cast.impl.AmbiguousLookupException;
    import com.avlsi.cast.impl.AlintFaninValue;
    import com.avlsi.cast.impl.ArrayValue;
    import com.avlsi.cast.impl.AnonymousValue;
    import com.avlsi.cast.impl.ASTWithInfo;
    import com.avlsi.cast.impl.BlockEnvironment;
    import com.avlsi.cast.impl.BoolValue;
    import com.avlsi.cast.impl.CastParserEnvironment;
    import com.avlsi.cast.impl.CastTreeParserInterface;
    import com.avlsi.cast.impl.ChainEnvironment;
    import com.avlsi.cast.impl.CircularImportException;
    import com.avlsi.cast.impl.DenseSubscriptSpec;
    import com.avlsi.cast.impl.Environment;
    import com.avlsi.cast.impl.EnvironmentEntry;
    import com.avlsi.cast.impl.EnvironmentEntryIterator;
    import com.avlsi.cast.impl.EnvironmentIterator;
    import com.avlsi.cast.impl.FieldedValueInterface;
    import com.avlsi.cast.impl.FixedBlockEnvironment;
    import com.avlsi.cast.impl.FloatValue;
    import com.avlsi.cast.impl.ImportEnvironment;
    import com.avlsi.cast.impl.InstanceValue;
    import com.avlsi.cast.impl.IntValue;
    import com.avlsi.cast.impl.InvalidOperationException;
    import com.avlsi.cast.impl.JavaChannelContainerValue;
    import com.avlsi.cast.impl.LocalEnvironment;
    import com.avlsi.cast.impl.LoopEnvironment;
    import com.avlsi.cast.impl.MetaParamValueInterface;
    import com.avlsi.cast.impl.NodeValue;
    import com.avlsi.cast.impl.NullEnvironment;
    import com.avlsi.cast.impl.PRSExpressionValue;
    import com.avlsi.cast.impl.Range;
    import com.avlsi.cast.impl.SelfImportException;
    import com.avlsi.cast.impl.SemanticWrapperException;
    import com.avlsi.cast.impl.SplicingEnvironment;
    import com.avlsi.cast.impl.SubscriptSpecInterface;
    import com.avlsi.cast.impl.Symbol;
    import com.avlsi.cast.impl.SymbolRedeclaredException;
    import com.avlsi.cast.impl.TokenWithInfo;
    import com.avlsi.cast.impl.TopLevelEnvironment;
    import com.avlsi.cast.impl.TupleValue;
    import com.avlsi.cast.impl.TupleGroupValue;
    import com.avlsi.cast.impl.Type;
    import com.avlsi.cast.impl.UserDefinedValue;
    import com.avlsi.cast.impl.Value;
    import com.avlsi.cast2.directive.impl.DirectiveParser;
    import com.avlsi.cast2.directive.impl.DirectiveFactory;
    import com.avlsi.cast2.directive.impl.DirectiveStatement;
    import com.avlsi.cast2.directive.impl.DirectiveImpl;
    import com.avlsi.cast2.directive.impl.DirectiveSyntaxException;
    import com.avlsi.csp.ast.ArrayAccessExpression;
    import com.avlsi.csp.ast.AssignmentStatement;
    import com.avlsi.csp.ast.BooleanExpression;
    import com.avlsi.csp.ast.BooleanType;
    import com.avlsi.csp.ast.CSPProgram;
    import com.avlsi.csp.ast.DeclaratorList;
    import com.avlsi.csp.ast.Declaration;
    import com.avlsi.csp.ast.Declarator;
    import com.avlsi.csp.ast.ExpressionInterface;
    import com.avlsi.csp.ast.IdentifierExpression;
    import com.avlsi.csp.ast.IntegerExpression;
    import com.avlsi.csp.ast.IntegerType;
    import com.avlsi.csp.ast.SequentialStatement;
    import com.avlsi.csp.ast.StatementInterface;
    import com.avlsi.csp.ast.VarStatement;
    import com.avlsi.csp.grammar.CspLexer;
    import com.avlsi.csp.grammar.CspParser;
    import com.avlsi.cell.AttributeInheritanceException;
    import com.avlsi.cell.BadCSPPortException;
    import com.avlsi.cell.CellImpl;
    import com.avlsi.cell.CellInterface;
    import com.avlsi.cell.CellUtils;
    import com.avlsi.cell.ExclusiveNodeSet;
    import com.avlsi.cell.NoSuchSubcellException;
    import com.avlsi.cell.RefinementException;
    import com.avlsi.cell.SubcellCreationException;
    import com.avlsi.fast.BlockInterface;
    import com.avlsi.fast.BlockIterator;
    import com.avlsi.fast.DirectiveBlock;
    import com.avlsi.fast.EnvBlock;
    import com.avlsi.fast.NetlistBlock;
    import com.avlsi.fast.NamedEnvironmentRedeclaredException;
    import com.avlsi.fast.ParamTypeInterface;
    import com.avlsi.fast.VerilogBlock;
    import com.avlsi.fast.metaparameters.ArrayMetaParam;
    import com.avlsi.fast.metaparameters.BooleanMetaParam;
    import com.avlsi.fast.metaparameters.FloatMetaParam;
    import com.avlsi.fast.metaparameters.IntegerMetaParam;
    import com.avlsi.fast.metaparameters.MetaParamDefinition;
    import com.avlsi.fast.metaparameters.MetaParamTypeInterface;
    import com.avlsi.fast.ports.ArrayType;
    import com.avlsi.fast.ports.ChannelType;
    import com.avlsi.fast.ports.NodeType;
    import com.avlsi.fast.ports.PortDefinition;
    import com.avlsi.fast.ports.PortTypeInterface;
    import com.avlsi.fast.ports.StructureType;
    import com.avlsi.file.cdl.parser.CDLLexer;
    import com.avlsi.file.cdl.parser.CDLParser;
    import com.avlsi.file.cdl.parser.CDLWalker;
    import com.avlsi.file.cdl.parser.Template;
    import com.avlsi.file.common.HierName;
    import com.avlsi.file.common.InvalidHierNameException;
    import com.avlsi.prs.ProductionRule;
    import com.avlsi.prs.ProductionRuleSet;
    import com.avlsi.tools.cosim.CoSimChannelNames;
    import com.avlsi.tools.cosim.JavaClassParameter;
    import com.avlsi.tools.cosim.JavaCoSimDevice;
    import com.avlsi.tools.cosim.JavaCoSimInfo;
    import com.avlsi.tools.cosim.CoSimParameters;
    import com.avlsi.util.bool.AndBooleanExpression;
    import com.avlsi.util.bool.BooleanExpressionInterface;
    import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
    import com.avlsi.util.bool.OrBooleanExpression;
    import com.avlsi.util.container.Pair;
    import com.avlsi.util.container.Triplet;
    import com.avlsi.util.debug.Debug;
    import com.avlsi.util.exception.AssertionFailure;
    import com.avlsi.util.text.StringUtil;
}

// bring this into sync with cast.g
// rationalize the names

// import
// handle _ for name

// spiff up using ideas from TinyBasicTreeWalker.g

// _ explanation:
// _ allowed? Places where identifiers appear
//      Y       variableDeclarationStatement    T _
//      Y       metaParam                       def T(X _)()
//      Y       portParam                       def T()(X _)
//      Y       loopStatement                   < _ : N : ... >
//      Y/N     expression:ident                _
//      N       expression:field
//      N       baseType
//      N       typeDeclaration
//
// The only time a _ is allowed as part of an expression
// is as part of the port real parameter list.  Ie X x(a, _)
// 
// For var decl / meta param / port param / loop statement, a variable
// is created with an autogenerated name that can never be specifed by
// a user.
//
// For the port real parameter list, a variable of the appropriate
// type and autogenerated name is created. Ie
// define X()(T t) { }
// X x(_)
// is equivalent to
// T autogen_name;
// X x(autogen_name)

// misc todo:
// ensure only algebraic types in meta params, and non-alg in port
// catch illegal array access

class CastTwoTreeParser extends TreeParser;

options {
    classHeaderSuffix = CastTreeParserInterface;
    importVocab = CastTwo;
    ASTLabelType = "ASTWithInfo";
    defaultErrorHandler = false;
}

{
    // flags used be type[] to indicate where variable is declared
    private static final int PORT_PARAM = 0;
    private static final int META_PARAM = 1;
    private static final int LOCAL_VAR  = 2;

    /** Syntax warning printout? **/
    private boolean verbose = false;

    /** control syntax warning level **/
    public void setVerbosity(boolean verbose) {
        this.verbose = verbose;
    }
    
    /**
     * Cell name that signifies the default refinement hierarchy root.  
     * This is the cellname used to look up the default refinement root.
     **/
    private static final String refinementRootCellName = "CELL";

    /**
     * Cell name that signifies an alternate refinement hierarchy root.  
     * This cell does not refine <code>refinementRootCellName</code>.
     **/
    private static final String refinementNullCellName = "NULL";

    /**
     * The module name for the file being parsed.  Will be null until
     * we encounter the module line, which must be first.
     **/
    private String moduleName = null;

    private CastParserEnvironment cpe = null;

    public void setCastParserEnvironment(final CastParserEnvironment cpe) {
        this.cpe = cpe;
    }

    /**
     * Set of cells that is currently being generated.
     **/
    private Set cellList = new HashSet();

    /**
     * CAST parsing options that may change how the parser works.
     **/
    private CastParsingOption opt = CastParsingOption.DEFAULT;

    public void setCastParsingOption(final CastParsingOption opt) {
        this.opt = opt;
    }

    /**
     * A stack of UserDefinedValues that are currently being processed.  A UDV
     * is pushed to/popped from the stack just before/after the body of the
     * UDV is tree parsed.  This is to help a cell defined in the environment
     * block get at the container cell's port list.
     **/
    private LinkedList activeUDV = new LinkedList();

    /**
     * A stack of missing imports due to bug 7068.  This helps the user add the
     * missing imports so the workaround for bug 7068 can be removed.
     **/
    private static LinkedList bug7068Warnings = new LinkedList();

    /**
     * Handler to process directive warnings.
     **/
    private DirectiveImpl.ErrorHandler directiveErrHandler = null;

    public void setDirectiveErrorHandler(
        final DirectiveImpl.ErrorHandler directiveErrHandler) {
        this.directiveErrHandler = directiveErrHandler;
    }

    /**
     * The RecognitionException has more accurate position information
     * on the error than anything else.
     **/
    private SemanticWrapperException semanticWrapperException(
            final String message,
            final RecognitionException e,
            final ASTWithInfo ast) {
        return semanticWrapperException(message, e, e.getFilename(),
            e.getLine(), -1);
    }

    private SemanticWrapperException semanticWrapperException(
            final String message,
            final Exception e,
            final ASTWithInfo ast) {
        return semanticWrapperException(message, e, ast.getFilename(),
            ast.getLine(), ast.getColumn());
    }

    /**
     * The RecognitionException has more accurate position information
     * on the error than anything else.
     **/
    private SemanticWrapperException semanticWrapperException(
            final RecognitionException e,
            final ASTWithInfo ast) {
        return semanticWrapperException(e, e.getFilename(), e.getLine(), -1);
    }

    private SemanticWrapperException semanticWrapperException(
            final Exception e,
            final ASTWithInfo ast) {
        return semanticWrapperException(e, ast.getFilename(),
            ast.getLine(), ast.getColumn());
    }

    private SemanticWrapperException semanticWrapperException(
            final Exception e,
            final String file,
            final int line,
            final int column) {
        return new SemanticWrapperException(e, file, line, column);
    }

    private SemanticWrapperException semanticWrapperException(
            final String message,
            final Exception e,
            final String file,
            final int line,
            final int column) {
        return new SemanticWrapperException(message, e, file, line, column);
    }

    private String getLocation(ASTWithInfo ast) {
        return ast.getFilename() + ":"
            + ast.getLine() + ","
            + ast.getColumn();
    }

    /**
     * Used to issue warnings.
     **/
    private void warn(final String s, final ASTWithInfo astForLoc) {
        System.err.println(s + " at " + getLocation(astForLoc));
    }

    //
    // commonly used operations to catch and map exceptions
    //

    /**
     * Lookup sym in env.  Turn all exception into
     * SemanticWrapperExceptions.
     **/
    private Value lookup(final Environment env,
                         final Symbol sym,
                         final ASTWithInfo symAST)
        throws SemanticWrapperException {
        return lookup(env, sym, symAST, true);
    }

    /**
     * Lookup sym in env.  Turn all exception into
     * SemanticWrapperExceptions.
     **/
    private Value lookup(final Environment env,
                         final Symbol sym,
                         final ASTWithInfo symAST,
                         final boolean tryFullyQualified) 
        throws SemanticWrapperException {
        try {
            final Value v = env.lookup(sym);
            if (v == null) {
                if (sym.fullyQualified()) {
                    // cache it in case we need it again.
                    return getFullyQualifiedCellValue(sym, env, symAST);
                }
                /*
                else if (tryFullyQualified && moduleName != null)
                    return getFullyQualifiedCellValue
                        (Symbol.create(moduleName + '.' + sym.getString()),
                         env, symAST);
                */
                else
                    throw semanticWrapperException("symbol undefined: " +
                                                   sym, new Exception(),
                                                   symAST);
            }
            return v;
        } catch (AmbiguousLookupException e) {
            throw semanticWrapperException("Symbol " + symAST.getText()
                + " exists in multiple imported files.  "
                + "Use (currently non-existent) import ... as.",
                e, symAST);
        }
    }

    /**
     * @param cell  the cell that will be affected by any connections
     **/
    private void assign(final Value dest,
                        final Value source,
                        final CellImpl cell,
                        final ASTWithInfo astLoc,
                        final String message)
        throws SemanticWrapperException {
        try {
            dest.assign(source, cell);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException(message, e, astLoc);
        }
    }

    /**
     * Produces an int from the given Value or throws an exception trying.
     **/
    private int getInt(final Value v, final ASTWithInfo ast)
        throws SemanticWrapperException {
        try {
            return IntValue.valueOf(v).getValue().intValue();
        } catch (InvalidOperationException e) {
            throw semanticWrapperException(e, ast);
        }
        
    }

    /**
     * Produces a double from the given Value or throws an exception trying.
     **/
    private double getFloat(final Value v, final ASTWithInfo ast)
        throws SemanticWrapperException {
        try {
            return FloatValue.valueOf(v).getValue();
        } catch (InvalidOperationException e) {
            throw semanticWrapperException(e, ast);
        }
        
    }

    /**
     * Turns the string into a Symbol.
     * If the string is "_", return a symbol of an auto-generated name,
     * otherwise, return a symbol of the name.
     **/
    private Symbol generateSymbol(final String s) {
        return Symbol.create(generateName(s));
    }

    /**
     * If the string is "_", return an auto-generated name,
     * otherwise, return the string.
     **/
    private String generateName(final String s) {
        if (s.equals("_"))
            return cpe.nextAnonymousName();
        else
            return s;
    }

    /**
     * Returns a collection of BooleanExpressionInterfaces for 
     * use by LOOP_OR / LOOP_AND (ie the &lt;| ... &gt; /
     * &lt;&amp; ... &gt; syntax.
     **/
    private Collection loopedExpression(
        final ASTWithInfo id,
        final Range r,
        final Environment env,
        final ASTWithInfo prsExpr)
    throws RecognitionException {
        final List l = new ArrayList();
        final Symbol sym = generateSymbol(id.getText());
        final Range.Iterator ri = r.iterator();
        while (ri.hasNext()) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));

            l.add(prsExpression(prsExpr, loopEnv, false));
        }

        return l;
    }

    private interface CombineValue {
        Value combine(final Value a, final Value b)
            throws InvalidOperationException;
        Value identity() throws InvalidOperationException;
    }

    private Value loopedExpression(final ASTWithInfo id, final Range r,
                                   final Environment env,
                                   final ASTWithInfo expr,
                                   final CombineValue f)
    throws RecognitionException, InvalidOperationException {
        final Symbol sym = generateSymbol(id.getText());
        Value result = f.identity();
        for (final Range.Iterator ri = r.iterator(); ri.hasNext(); ) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));
            final Value v = expression(expr, loopEnv, false);
            result = f.combine(result, v);
        }

        return result;
    }

    /**
     * Converts Value to BoolValue for use in an if guard.
     **/
    private BoolValue ifGuard(final Value v, final ASTWithInfo expr)
        throws SemanticWrapperException {
        try {
            return BoolValue.valueOf(v);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("if guard " + v + " not bool",
                e, expr);
        }
    }

    /**
     * Attempts to convert a value to a PRSExpressionValue, throwing
     * a SemanticWrapperException if the conversion is not possible.
     **/
    private PRSExpressionValue toPRSExpressionValue(
        final Value v,
        final ASTWithInfo expr)
    throws SemanticWrapperException {
        try {
            return PRSExpressionValue.valueOf(v);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("type not valid for lhs"
                + " of a production rule", e, expr);
        }
    }

    private HierName oppositePowerSupply(
        final Environment env,
        final HierName powerSupply,
        final ASTWithInfo errorAST) throws SemanticWrapperException {

        final String newPowerSupply;

        if (powerSupply.isVdd())
            newPowerSupply = "GND";
        else if (powerSupply.isGND())
            newPowerSupply = "Vdd";
        else
            throw new AssertionFailure("bad power supply: "+powerSupply.getAsString('.'));

        try {
            return NodeValue.valueOf(
                    lookup(env, Symbol.create(newPowerSupply), errorAST))
                .getInstanceName();
        } catch (InvalidOperationException exn) {
            throw semanticWrapperException("Non-node type for " +
                newPowerSupply + ", implied by complex production rule",
                exn, errorAST);
        }
    }

    public void doInstantiation(
            final AST t,
            final String moduleName,
            final Environment env,
            final CellImpl parent,
            final CellInterface envContainer)
        throws RecognitionException {
        this.moduleName = moduleName;
        variableDeclarationStatement(t, new BlockEnvironment(env),
                false, parent, envContainer, null, false);
    }

    /**
     * Create the proper fake AST to specify the given cell as a
     * refinement parent in the UDV.  errorAST supplies
     * file/line/column info for all of the fake tree.
     **/
    // TODO: caching?
    private ASTWithInfo fakeRefinementAST(String cellName,
                                          ASTWithInfo errorAST) {
        final ASTFactory factory = new ASTFactory();
        factory.setASTNodeClass(ASTWithInfo.class.getName());
        final ASTWithInfo result;
        result = (ASTWithInfo) factory.create(
                CastTwoTreeParserTokenTypes.REFINEMENT);
        result.copyInfo(errorAST);
        final ASTWithInfo child = (ASTWithInfo) factory.create(
                CastTwoTreeParserTokenTypes.IDENT, cellName);
        child.copyInfo(errorAST);
        result.addChild(child);
        return result;
    }

    /**
     * Creates the proper fake AST for an implied parameter list that
     * isn't there.  errorAST supplies file/line/column info for the
     * fake tree
     **/
    private ASTWithInfo fakeEmptyPorts(ASTWithInfo errorAST) {
        final ASTFactory factory = new ASTFactory();
        factory.setASTNodeClass(ASTWithInfo.class.getName());
        final ASTWithInfo result = (ASTWithInfo) factory.create(
                CastTwoTreeParserTokenTypes.FORMALS);
        result.copyInfo(errorAST);
        return result;
    }

    /**
     * Creates the proper fake AST for the implied empty block in a
     * defalias declaration.
     **/
    private ASTWithInfo fakeEmptyBlock(ASTWithInfo errorAST) {
        final ASTFactory factory = new ASTFactory();
        factory.setASTNodeClass(ASTWithInfo.class.getName());
        final ASTWithInfo result = (ASTWithInfo)
            factory.create(CastTwoTreeParserTokenTypes.BLOCK);
        result.copyInfo(errorAST);
        final ASTWithInfo bodyStmts = (ASTWithInfo)
            factory.create(CastTwoTreeParserTokenTypes.BODY_STATEMENT_LIST);
        bodyStmts.copyInfo(errorAST);
        result.addChild(bodyStmts);
        return result;
    }

    /**
     * Looks up the UserDefinedValue for the cell and inserts it into
     * the specified environment by the fully qualified name only.
     * Also returns it.  FIXME: doesn't insert it into the environment yet.
     **/
    public Value getFullyQualifiedCellValue(Symbol sym, Environment env,
                                            final ASTWithInfo errorAST)
        throws SemanticWrapperException {
        try {
            final String fullName = sym.getString();
            final int lastDot = fullName.lastIndexOf('.');
            final String moduleName = fullName.substring(0, lastDot);
            final String cellName = fullName.substring(lastDot + 1);
            final Environment parsed =
                cpe.parseCell(new LinkedList(), moduleName, cellName);
            Value result = lookup(parsed, Symbol.create(cellName), errorAST,
                                  false);
            //            env.bind(sym, result);
            return result;
        }
        catch (Exception e) {
            throw semanticWrapperException(e, errorAST);
        }
    }

    /**
     * Remove all layers of arrays and return the unarrayed base type.
     **/
    private static PortTypeInterface getBaseType(final PortTypeInterface p) {
        return p instanceof ArrayType ? ((ArrayType) p).getArrayedType() : p;
    }
    
    /**
     * Used to be part of baseType[].  This handles the case when the
     * CellImpl had been generated before and only needs to be
     * attached.  Still shouldn't be called by anything but baseType[]
     **/
    // FIXME: variable names suck
    private void attachGeneratedCell(TupleValue tupleVal,
                                     BlockEnvironment paramEnv,
                                     LocalEnvironment localEnv,
                                     UserDefinedValue typeVal,
                                     ArrayList portList,
                                     ArrayList impliedPortList,
                                     ArrayList impliedPortParentNodes,
                                     LocalEnvironment aliasesEnv)
        throws SymbolRedeclaredException, RecognitionException {
        
        final UserDefinedValue envUDV = typeVal.getEnvironmentContainer();
        final AST portParamList;
        final AST impliedPortParamList;
        final AST envExtraPortParamList = typeVal.getEnvExtraParamList();
        final boolean reverseDir;  // should port directionality be reversed?
        if (envUDV == null) {
            portParamList = typeVal.getPortParamList();
            impliedPortParamList = typeVal.getImpliedPortParamList();
            reverseDir = false;
        } else {
            // environments have identical ports as their parent containers,
            // but with the port directions reversed
            portParamList = envUDV.getPortParamList();
            impliedPortParamList = envUDV.getImpliedPortParamList();
            reverseDir = true;
        }

        final CellImpl tempCell = new CellImpl("temp_cell", "temp_module",
                                               CellImpl.SYNTHETIC_CELL);

        // portParamList maybe null if port list has been elided for an
        // attribute cell; in all other cases, because of refinement, this
        // cannot be null.
        assert (portParamList != null ||
                typeVal.getStructureType() == UserDefinedValue.ATTRIBUTES_CELL);

        if (portParamList != null) {
            final Environment portEnv = typeVal.getPortEnvironment(tupleVal);
            portParamList(portParamList,
                          new BlockEnvironment(portEnv, localEnv), tempCell,
                          portList, null, reverseDir);
        }
        if (impliedPortParamList != null)
            portParamList(impliedPortParamList, paramEnv, tempCell,
                          impliedPortList, impliedPortParentNodes, false);
        if (envExtraPortParamList != null)
            portParamList(envExtraPortParamList, paramEnv, tempCell,
                          null, null, false);
        paramEnv.absorbEnv(aliasesEnv);
    }

    private static class CellData {
        public final UserDefinedValue udv;
        public final CellImpl cell;
        public final InstanceValue instance;
        public final TupleValue metaParams;
        public CellData(final UserDefinedValue udv,
                        final CellImpl cell,
                        final InstanceValue instance,
                        final TupleValue metaParams) {
            this.udv = udv;
            this.cell = cell;
            this.instance = instance;
            this.metaParams = metaParams;
        }
    }

    /**
     * @param metaEnv environment used to evaluate the metaparameter list
     **/
    private CellData getCellFromAST(final ASTWithInfo cellName,
                                    final ASTWithInfo metaParams,
                                    final ASTWithInfo errorAST,
                                    final Environment paramEnv,
                                    final Environment metaEnv,
                                    final CellInterface envContainer)
    throws RecognitionException {
        final String name = cellName.getText();
        final Pair baseType =
            baseType(CastTwoUtil.makeBaseTypeAST(name, metaParams, cellName),
                     metaEnv, paramEnv, null, envContainer, null, false);
        final UserDefinedValue udv =
            (UserDefinedValue) lookup(paramEnv, Symbol.create(name), errorAST);

        final TupleValue metaParamsTuple;
        if (metaParams == null) {
            metaParamsTuple = new TupleValue(new Value[0]);
        } else {
            metaParamsTuple = expressionList(metaParams, metaEnv, false);
        }

        final CellImpl cell = udv.getCell(metaParamsTuple);

        return new CellData(udv, cell, (InstanceValue) baseType.getFirst(),
                            metaParamsTuple);
    }

    private CellData getCellFromAST(final ASTWithInfo cellName,
                                    final ASTWithInfo cellMeta,
                                    final ASTWithInfo envName,
                                    final ASTWithInfo envMeta,
                                    final ASTWithInfo errorAST,
                                    final Environment env)
    throws RecognitionException {
        final CellData data = getCellFromAST(cellName, cellMeta, errorAST,
                                             env, env, null);
        if (envName == null) {
            assert envMeta == null;
            return data;
        }

        return getCellFromAST(envName, envMeta, errorAST,
                              data.cell.getEnvironments().getEnvironment(),
                              env, data.cell);
    }

    private class Bug7068Environment implements Environment {
        private final Environment correct, workaround;

        public Bug7068Environment(Environment correct, Environment workaround) {
            this.correct = correct;
            this.workaround = workaround;
        }

        public void bind(final Symbol sym, final Value val)
        throws SymbolRedeclaredException {
            // this will create the same binding in correct, because both share
            // the same LocalEnvironment where the binding occurs
            workaround.bind(sym, val);
        }

        public boolean contains(final Symbol sym) {
            return workaround.contains(sym);
        }

        public Value lookup(final Symbol sym) throws AmbiguousLookupException {
            Value c = null;
            AmbiguousLookupException ce = null;
            try {
                c = correct.lookup(sym);
            } catch (AmbiguousLookupException e) {
                ce = e;
            }

            Value w = null;
            AmbiguousLookupException we = null;
            try {
                w = workaround.lookup(sym);
            } catch (AmbiguousLookupException e) {
                we = e;
            }

            if (c == null && w instanceof UserDefinedValue) {
                final UserDefinedValue udv = (UserDefinedValue) w;
                final Collection warns = (Collection) bug7068Warnings.getLast();
                warns.add(udv.getFullyQualifiedType());
            }

            if (we != null) throw we;
            else return w;
        }

        public EnvironmentIterator iterator() {
            return workaround.iterator();
        }
        
        public EnvironmentEntryIterator entryIterator() {
            return workaround.entryIterator();
        }
    }

    // Convert a MetaParamTypeInterface to a Value
    private Value fromMetaParam(final MetaParamTypeInterface ty) {
        if (ty instanceof BooleanMetaParam) {
            return BoolValue.valueOf(((BooleanMetaParam) ty).toBoolean());
        } else if (ty instanceof FloatMetaParam) {
            return FloatValue.valueOf(((FloatMetaParam) ty).toFloat());
        } else if (ty instanceof IntegerMetaParam) {
            return IntValue.valueOf(((IntegerMetaParam) ty).toBigInteger());
        } else if (ty instanceof ArrayMetaParam) {
            final ArrayMetaParam am = (ArrayMetaParam) ty;
            final int min = am.getMinIndex();
            final int max = am.getMaxIndex();
            final Value[] vals = new Value[max - min + 1];
            for (int i = 0; i < vals.length; ++i) {
                vals[i] = fromMetaParam(am.get(i + min));
            }

            final Range r = new Range(min, max);

            if (vals[0] instanceof ArrayValue) {
                final ArrayValue[] newVals = new ArrayValue[vals.length];
                System.arraycopy(vals, 0, newVals,0, vals.length);
                try {
                    return ArrayValue.augmentDimension(newVals, r);
                } catch (InvalidOperationException e) {
                    throw new AssertionError("Cannot augment array");
                }
            } else {
                return new ArrayValue(vals,
                                      new DenseSubscriptSpec(new Range[] { r }),
                                      null, false);
            }
        } else {
            throw new AssertionError("Cannot create value from: " + ty);
        }
    }

    // Reconstruct the metaparameter list of a cell
    private TupleValue fromMetaParam(final UserDefinedValue udv,
                                     final CellInterface ci) {
        if (CellUtils.getBaseType(ci.getFullyQualifiedType())
                     .equals(udv.getFullyQualifiedType())) {
            final ArrayList<Value> vals = new ArrayList<Value>();
            for (Iterator i = ci.getMetaParamDefinitions(); i.hasNext(); ) {
                final MetaParamDefinition def = (MetaParamDefinition) i.next();
                vals.add(fromMetaParam(def.getType()));
            }
            return new TupleValue(vals.toArray(new Value[0]));
        } else {
            return fromMetaParam(udv, ci.getDirectRefinementParent());
        }
    }

    /**
     * Used to be part of baseType[].  This handles the case where the
     * CellImpl hadn't been generated before.  Still shouldn't be
     * called by anything but baseType[].
     * @param localEnv the child environment of paramEnv.
     * 
     * @throws RecognitionException
     **/
    // FIXME: variable names suck
    private CellImpl generateAndAttachCell(TupleValue tupleVal,
                                           ASTWithInfo errorAST,
                                           final UserDefinedValue typeVal,
                                           BlockEnvironment paramEnv,
                                           LocalEnvironment inheritedConstants,
                                           BlockEnvironment portEnv,
                                           LocalEnvironment metaEnv,
                                           LocalEnvironment localEnv,
                                           List metaParams,
                                           String typeName,
                                           ArrayList portList,
                                           ArrayList impliedPortList,
                                           ArrayList impliedPortParentNodes,
                                           CellInterface envContainer)
        throws RecognitionException {

        final UserDefinedValue envUDV = typeVal.getEnvironmentContainer();
        final AST portParamList;
        final AST impliedPortParamList;
        final AST envExtraPortParamList = typeVal.getEnvExtraParamList();
        final boolean reverseDir;  // should port directionality be reversed?
        if (envUDV == null) {
            portParamList = typeVal.getPortParamList();
            impliedPortParamList = typeVal.getImpliedPortParamList();
            reverseDir = false;
        } else {
            // environments have identical ports as their parent containers,
            // but with the port directions reversed
            portParamList = envUDV.getPortParamList();
            impliedPortParamList = envUDV.getImpliedPortParamList();
            // save the container's port list and implied port list as our own
            typeVal.updatePortParamList(portParamList);
            typeVal.updateImpliedPortParamList(impliedPortParamList);
            typeVal.updateEnvExtraParamList(envExtraPortParamList);
            reverseDir = true;
        }
        final AST inheritanceList = typeVal.getInheritanceList();
        ArrayList inheritanceCellList = null;
        ArrayList inheritanceCellConstantsList = null;
        ArrayList inheritancePrsEnvList = null;
        ArrayList inheritanceSubcellsEnvList = null;
              AST refinementParent = typeVal.getRefinementParent();
        final String moduleName = typeVal.getModuleName();
        final int typeValType = typeVal.getStructureType();
        final boolean isAttributesCell =
            typeValType == UserDefinedValue.ATTRIBUTES_CELL;

        // To be filled out, attached, and returned
        final CellImpl subCell;

        final InstanceValue parentInstance;
        final CellImpl parentCell; // Doesn't come from
                                   // parentInstance, but from
                                   // the environment 
        final LocalEnvironment parentAliases;
        final Environment parentCellConstants;
        Environment parentPrsEnvironment;
        Environment parentSubcellsEnvironment;
        final String refinementName;
        final AST parentImpliedPorts;
        final AST parentPortParamList;
        final AST parentEnvExtraPorts;

        // handle <+ inheritance
        if (inheritanceList != null) {
            inheritanceCellList = new ArrayList();
            inheritanceCellConstantsList = new ArrayList();
            inheritancePrsEnvList = new ArrayList();
            inheritanceSubcellsEnvList = new ArrayList();

            for (AST inheritance = inheritanceList.getFirstChild();
                 inheritance != null;
                 inheritance = inheritance.getNextSibling()) {

                final CellData inheritanceInfo =
                    getCellData(inheritance.getFirstChild(), errorAST,
                                paramEnv, envContainer);

                final CellImpl inheritanceCell = inheritanceInfo.cell;
                final UserDefinedValue inheritanceUDV = inheritanceInfo.udv;
                final TupleValue inheritanceMetaParamsTuple =
                    inheritanceInfo.metaParams;

                if (!inheritanceCell.isAttributesCell()) {
                    throw semanticWrapperException(
                        new AttributeInheritanceException(
                            "Only attributes cells may be inherited from " +
                            "with <+", 
                            typeName, inheritanceCell.getFullyQualifiedType()), 
                        errorAST);
                }

                final Environment inheritanceCellConstants =
                    inheritanceUDV
                        .getCellConstants(inheritanceMetaParamsTuple);
                final Environment inheritancePrsEnvironment =
                    inheritanceUDV
                        .getPrsEnvironment(inheritanceMetaParamsTuple);
                final Environment inheritanceSubcellsEnvironment =
                    inheritanceUDV
                        .getSubcellsEnvironment(inheritanceMetaParamsTuple);

                inheritanceCellList.add(inheritanceCell);
                inheritanceCellConstantsList.add(inheritanceCellConstants);
                inheritancePrsEnvList.add(inheritancePrsEnvironment);
                inheritanceSubcellsEnvList.add(inheritanceSubcellsEnvironment);
            }
        }

        // 8
        final UserDefinedValue parentUDV;
        final TupleValue parentMetaParamsTuple;
        if (refinementParent != null) {
            // The grammar should not allow this to happen
            Debug.assertTrue(!isAttributesCell,
                "Attributes cells are not allowed to refine with <:");

            final CellData refinementInfo =
                getCellData(refinementParent.getFirstChild(), errorAST,
                            paramEnv, envContainer);

            parentCell = refinementInfo.cell;
            parentInstance = refinementInfo.instance;
            refinementName = parentCell.getFullyQualifiedType();
            parentMetaParamsTuple = refinementInfo.metaParams;
            parentUDV = refinementInfo.udv;

            final int parentUDVType = parentUDV.getStructureType();
            if (parentUDVType == UserDefinedValue.ATTRIBUTES_CELL) {
                throw semanticWrapperException(
                    new RefinementException(
                        "can't <: an attributes cell", 
                        typeName, 
                        refinementName), 
                    errorAST);
            } else if ((typeValType == UserDefinedValue.CHANNEL) ^
                       (parentUDVType == UserDefinedValue.CHANNEL)) {
                throw semanticWrapperException(
                    new RefinementException(
                        "only a channel can <: a channel",
                        typeName, 
                        refinementName), 
                    errorAST);
            }

            parentAliases = parentUDV.getAliases(parentMetaParamsTuple);

            parentCellConstants = parentUDV.getCellConstants(parentMetaParamsTuple);
            parentPrsEnvironment =
                parentUDV.getPrsEnvironment(parentMetaParamsTuple);
            parentSubcellsEnvironment =
                parentUDV.getSubcellsEnvironment(parentMetaParamsTuple);
            
            // For future comparison with the child's implied ports
            parentImpliedPorts = parentUDV.getImpliedPortParamList();
            parentPortParamList = parentUDV.getPortParamList();
            parentEnvExtraPorts = parentUDV.getEnvExtraParamList();
        }
        else {
            parentInstance = null;
            refinementName = null;
            parentMetaParamsTuple = null;
            parentUDV = null;
            parentCell = null;
            parentImpliedPorts = null;
            parentAliases = null;
            parentCellConstants = null;
            parentPrsEnvironment = NullEnvironment.getInstance();
            parentSubcellsEnvironment = NullEnvironment.getInstance();
            parentPortParamList = null;
            parentEnvExtraPorts = null;
        }

        // map the UserDefinedValue structure types to the CellImpl
        // definition types
        final int definitionKind;
        if (typeValType == UserDefinedValue.CHANNEL)
            definitionKind = CellImpl.CHANNEL;
        else if (typeValType == UserDefinedValue.CELL)
            definitionKind = CellImpl.CELL;
        else if (typeValType == UserDefinedValue.ALIAS_CELL)
            definitionKind = CellImpl.ALIAS_CELL;
        else {
            Debug.assertTrue
                (typeValType == UserDefinedValue.ATTRIBUTES_CELL);
            definitionKind = CellImpl.ATTRIBUTES_CELL;
        }
        
        // 9
        subCell = new CellImpl(
            envContainer == null ?
                typeName : envContainer.getType() + "_" + typeName + "Env",
            moduleName, definitionKind,
            ((ASTWithInfo) typeVal.getBody()).getFilename());
        opt.beginCellConstruction(subCell);
        if (opt.bug7068HackEnabled()) {
            bug7068Warnings.addLast(new TreeSet());
        }
        for (final Iterator i = metaParams.iterator(); i.hasNext(); ) {
            final MetaParamDefinition metaParam =
                (MetaParamDefinition) i.next();
            subCell.addMetaParamDefinition(metaParam);
        }

        // <+ inherit top-level constants from attributes cells
        final LocalEnvironment childCellConstants = new LocalEnvironment();
        if (inheritanceCellConstantsList != null) {
            final Iterator j = inheritanceCellList.iterator();
            for (Iterator i = inheritanceCellConstantsList.iterator();
                 i.hasNext(); ) {
                final CellImpl inheritanceCell = (CellImpl) j.next();
                try {
                    final Environment inheritedEnv =
                        (Environment) i.next();
                    inheritedConstants.absorbEnv(inheritedEnv);
                } catch (SymbolRedeclaredException e) {
                    throw semanticWrapperException
                        ("trouble importing the top-level constants " +
                         "of the <+ inheritance parent " +
                         inheritanceCell.getFullyQualifiedType(), 
                            e, errorAST);
                }
            }
        }
        
        // <: inherit top-level constants from refinement parent
        if (parentCellConstants != null) {
            try {
                inheritedConstants.absorbEnv(parentCellConstants);
            } catch (SymbolRedeclaredException e) {
                throw semanticWrapperException
                    ("trouble importing the top-level constants " +
                     "of the <: refinement parent", 
                        e, errorAST);
            }
        }

        final boolean childHadPorts;
        if (portParamList == null) {
            childHadPorts = false;
        } else {
            childHadPorts = true;
            final Environment childPortEnv;
            if (typeVal.isPortListInherited() && envUDV == null) {
                Environment parentPortEnv =
                    typeVal.getPortEnvironment(tupleVal);
                if (parentPortEnv == null) {
                    parentPortEnv =
                        parentUDV.getPortEnvironment(parentMetaParamsTuple);
                    typeVal.putPortEnvironment(tupleVal, parentPortEnv);
                }
                childPortEnv = new BlockEnvironment(parentPortEnv, localEnv);
            } else if (envUDV != null && envContainer != null) {
                Environment envPortEnv = typeVal.getPortEnvironment(tupleVal);
                if (envPortEnv == null) {
                    envPortEnv =
                        envUDV.getPortEnvironment(
                            fromMetaParam(envUDV, envContainer));
                    typeVal.putPortEnvironment(tupleVal, envPortEnv);
                }
                childPortEnv = new BlockEnvironment(envPortEnv, localEnv);
            } else {
                childPortEnv = paramEnv;
                typeVal.putPortEnvironment(tupleVal, portEnv);
            }
            portParamList(portParamList, childPortEnv, subCell, portList, null,
                          reverseDir);
        }
        
        // 10
        if (refinementParent != null) {// Give the child the proper port param list
            // Need to compare the child and parent lists before binding anything.
            if (!childHadPorts) {
                // the port list in the refinement child is optional, if it is
                // exactly the same as the port list of the parent
                final Environment parentPortEnv =
                    parentUDV.getPortEnvironment(parentMetaParamsTuple);
                portParamList(parentPortParamList,
                              new BlockEnvironment(parentPortEnv, localEnv),
                              subCell, portList, null, reverseDir);
                typeVal.updatePortParamList(parentPortParamList);
                typeVal.putPortEnvironment(tupleVal, parentPortEnv);
            }
            final boolean childHadImpliedPorts;
            if (impliedPortParamList == null) {
                childHadImpliedPorts = false;
            } else {
                childHadImpliedPorts = true;
                portParamList(impliedPortParamList, paramEnv, 
                    subCell, impliedPortList, impliedPortParentNodes, false);
            }
            if (! childHadImpliedPorts) { // Adopt from parent
                portParamList(parentImpliedPorts, paramEnv, 
                    subCell, impliedPortList, impliedPortParentNodes, false);
                typeVal.updateImpliedPortParamList(parentImpliedPorts);
            }
            else {
                // Check to make sure the parent implied port list is empty
                // or the implied lists agree
                if (parentImpliedPorts.getFirstChild() != null &&
                    !impliedPortParamList.equalsTree(parentImpliedPorts)) {
                    throw semanticWrapperException(
                        new RefinementException(
                            "implied port parameter lists differ", 
                            typeName, 
                            refinementName), 
                        errorAST);
                }
            }

            final boolean childHadExtraPorts;
            if (envExtraPortParamList == null) {
                childHadExtraPorts = false;
            } else {
                childHadExtraPorts = true;
                portParamList(envExtraPortParamList, paramEnv, subCell, null,
                    null, false);
            }
            if (parentEnvExtraPorts != null) {
                if (! childHadExtraPorts) {
                    portParamList(parentEnvExtraPorts, paramEnv, subCell, null,
                        null, false);
                }
                else {
                    // Check to make sure the parent extra port list is empty
                    // or the implied lists agree
                    if (parentEnvExtraPorts.getFirstChild() != null &&
                        !envExtraPortParamList.equalsTree(parentEnvExtraPorts)) {
                        throw semanticWrapperException(
                            new RefinementException(
                                "env extra port parameter lists differ", 
                                typeName, 
                                refinementName), 
                            errorAST);
                    }
                }
            }
            
        }
        else {
            // No refinement, go ahead and process implied ports
            if (impliedPortParamList != null) {
                portParamList(impliedPortParamList, paramEnv, subCell, 
                    impliedPortList, impliedPortParentNodes, false);
            }
            if (envExtraPortParamList != null) {
                portParamList(envExtraPortParamList, paramEnv, subCell, null,
                    null, false);
            }
        }

        // 11 protect the types of parameter arrays from being changed
        // by the body, because the parameters and body are in the
        // same environment to support this: "define Y()(T x[0..1]) {
        // T x[2] = z; }".  This might only be temporary.  Later, when
        // cflat compatability is not an issue, we can do: "T y[0..2] =
        // x @ { z };"
        if (typeValType != UserDefinedValue.CHANNEL) {
            // childCellConstants will be changed by the call to block()
            final BlockEnvironment bodyEnvironment =
                new FixedBlockEnvironment(paramEnv.protectedArrayEnvironment(),
                                          childCellConstants);

            // chain the <+ prs environments
            if (inheritancePrsEnvList != null)
                parentPrsEnvironment =
                    chainEnvironments(inheritancePrsEnvList,
                                      parentPrsEnvironment);

            final LocalEnvironment localPrsEnvironment =
                new LocalEnvironment();
            final Environment childPrsEnvironment =
                spliceEnvironments(childCellConstants,
                                   paramEnv.protectedArrayEnvironment(),
                                   parentPrsEnvironment,
                                   localPrsEnvironment);

            // chain the <+ subcells/subtypes environments
            if (inheritanceSubcellsEnvList != null)
                parentSubcellsEnvironment =
                    chainEnvironments(inheritanceSubcellsEnvList,
                                      parentSubcellsEnvironment);

            final LocalEnvironment localSubcellsEnvironment =
                new LocalEnvironment();
            final Environment splicedSubcellsEnvironment =
                spliceEnvironments(childCellConstants,
                                   paramEnv.protectedArrayEnvironment(),
                                   parentSubcellsEnvironment);
            final Environment childSubcellsEnvironment =
                new FixedBlockEnvironment(splicedSubcellsEnvironment,
                                          localSubcellsEnvironment);
            final Environment childSubtypesEnvironment =
                new BlockEnvironment(splicedSubcellsEnvironment,
                                     localSubcellsEnvironment);

            // Copy node subcells from parent to child.
            // This is needed because setRefinementParent is not called
            // until after we parse the child's body, but we must have
            // the node subcells in the child cell in order for
            // assignDefaultsToImpliedPorts to work if one of the implied
            // ports was inherited from the parent.
            // We should really figure out a way to do setRefinementParent
            // before we parse the body.  --jmr
            if (parentCell != null)
                subCell.inheritNodes(parentCell);

            // Make cell constants visible to the environment block.
            final BlockEnvironment paramConstantEnv =
                new BlockEnvironment(
                    new SplicingEnvironment(paramEnv, childCellConstants));

            // Treeparse the body of the cell
            activeUDV.addLast(typeVal);
            block(typeVal.getBody(), bodyEnvironment, subCell, envContainer,
                  paramConstantEnv, childPrsEnvironment,
                  childSubcellsEnvironment, childSubtypesEnvironment);
            if (metaParams.isEmpty()) typeVal.clearBody();
            activeUDV.removeLast();

            // Archive the cell-level constants.
            typeVal.putCellConstants(tupleVal,
                new SplicingEnvironment(inheritedConstants,
                                        childCellConstants));

            final Environment paramWithoutBodyEnv =
                new BlockEnvironment(
                    new BlockEnvironment(inheritedConstants, metaEnv),
                    localEnv.protectedArrayEnvironment());

            // Archive the prs environment
            final Environment splicedPrs =
                spliceEnvironments(childCellConstants, paramWithoutBodyEnv,
                                   parentPrsEnvironment, localPrsEnvironment);
            typeVal.putPrsEnvironment(tupleVal,
                opt.bug7068HackEnabled() ?
                    new Bug7068Environment(splicedPrs,
                                           childPrsEnvironment)
                :   splicedPrs);

            // Archive the subcells/subtypes environment
            final Environment splicedSubcells =
                spliceEnvironments(childCellConstants, paramWithoutBodyEnv,
                                   parentSubcellsEnvironment,
                                   localSubcellsEnvironment);
            typeVal.putSubcellsEnvironment(tupleVal,
                opt.bug7068HackEnabled() ?
                    new Bug7068Environment(splicedSubcells,
                                           childSubcellsEnvironment)
                :   splicedSubcells);
        }
        else {
            try {
                paramEnv.absorbEnv(parentAliases);
            } catch (SymbolRedeclaredException e) {
                throw semanticWrapperException
                ("trouble importing the aliases of the refinement parent", 
                    e, errorAST);
            }
            final LocalEnvironment localAliases = 
                channelBody(typeVal.getBody(), paramEnv, subCell);
            if (metaParams.isEmpty()) typeVal.clearBody();
            final LocalEnvironment childAliases =
                localAliases == null ? parentAliases : localAliases;
            if (childAliases != parentAliases) {
                try {
                    childAliases.absorbEnv(parentAliases);
                } catch (SymbolRedeclaredException e) {
                    throw semanticWrapperException
                    ("child redeclares aliases defined in refinement parent", 
                        e, errorAST);
                }
            }
            typeVal.putAliases(tupleVal, childAliases);
        }

        try {
            // finish <+ inheritance handling
            if (inheritanceCellList != null) {
                for (Iterator i = inheritanceCellList.iterator();
                     i.hasNext(); ) {
                    final CellImpl inheritanceCell = (CellImpl) i.next();
                    subCell.setInheritance(inheritanceCell);
                }
            }
        
            // 12
            if (refinementParent != null) {
                // Ports should match or the parent port list should be null.
                boolean portsMatched = false; // Initialized to placate the compiler
                Symbol[] refinedPorts = (Symbol [])portList.toArray(new Symbol[0]);
                Symbol[] parentPorts = parentInstance.getPortParams();
                if (refinedPorts.length != parentPorts.length) {
                    if (parentPorts.length == 0) {
                        portsMatched = false;
                    }
                    else {
                        throw new RefinementException("mismatch between the "+
                            "lengths of port lists", typeName, refinementName);
                    }
                }
                else if (!childHadPorts) {
                    // if the child has no ports, then the ports are copied
                    // from the parent, so there cannot be any mismatch
                    portsMatched = true;
                }
                else {
                    AST currentPort = portParamList.getFirstChild();
                    for (int i=0; i<refinedPorts.length; i++) {
                        errorAST = (ASTWithInfo) currentPort.getFirstChild();
                        if (! refinedPorts[i].equals(parentPorts[i])) {
                            throw new RefinementException
                                ("name mismatch on port " + (i + 1) + ": " + 
                                    refinedPorts[i] + " doesn't equal " + 
                                    parentPorts[i], typeName, refinementName);
                        }
                        // The port of the refined class needs to be
                        // ( refined from )* the port of the base
                        // class.
                        
                        final Value refinedPortValue = 
                            lookup(paramEnv, refinedPorts[i], errorAST);
                        final Value basePortValue = 
                            lookup(parentInstance.getParamEnv(),
                                   parentPorts[i],
                                   errorAST);
                        
                        try {
                            if (! refinedPortValue.eventuallyRefinesFrom(basePortValue))
                                throw new RefinementException
                            ("type mismatch on port " + (i + 1) + ": " + 
                                refinedPortValue.getType().getString() + 
                                " can't refine from " + 
                                basePortValue.getType().getString(), 
                                typeName, refinementName);
                        } catch (InvalidOperationException e) {
                            throw semanticWrapperException
                            ("trouble matching ports in refinement", e, errorAST);
                        }

                        // e1of4[2] X and e1of4 X[0..1] are considered to be
                        // equivalent above, but they are not, for the purpose
                        // of refinement, so here we check that the types are
                        // actually equivalent
                        final String sport = refinedPorts[i].getString();
                        final PortDefinition refinedDef =
                            subCell.getPortDefinition(sport);
                        final PortDefinition baseDef =
                            parentCell.getPortDefinition(sport);

                        // check port directionality match
                        if (refinedDef.getDirection() != baseDef.getDirection())
                            syntaxError("port " + sport +
                                        " has different directions refining " +
                                        typeName + " from " + refinementName,
                                        errorAST);

                        final PortTypeInterface refinedPort =
                            getBaseType(refinedDef.getType());
                        final PortTypeInterface basePort =
                            getBaseType(baseDef.getType());
                        final boolean widthMatched;
                        // Only node and channels matter, because only they can
                        // be "wide"
                        if (refinedPort instanceof NodeType) {
                            widthMatched =
                                ((NodeType) refinedPort).getWidth() ==
                                ((NodeType) basePort).getWidth();
                        } else if (refinedPort instanceof ChannelType) {
                            widthMatched =
                                ((ChannelType) refinedPort).getWidth() ==
                                ((ChannelType) basePort).getWidth();
                        } else {
                            widthMatched = true;
                        }
                        if (!widthMatched) {
                            throw new RefinementException
                                ("type mismatch on port " + (i + 1) + ": " +
                                 sport + " must have consistent width ",
                                 typeName, refinementName);
                        }
                        currentPort = currentPort.getNextSibling();
                    }
                    portsMatched = true;
                }
                
                subCell.setRefinementParent(parentCell, portsMatched, opt);
            }
        } catch (RefinementException e) {
            throw semanticWrapperException(e, errorAST);
        }
        
        typeVal.putCell(tupleVal, subCell);
        opt.endCellConstruction(subCell);
        if (opt.bug7068HackEnabled()) {
            final Collection warns = (Collection) bug7068Warnings.removeLast();
            if (!warns.isEmpty()) {
                System.err.println("Warning: " +
                                   subCell.getFullyQualifiedType() +
                                   " requires the following missing imports: ");
                for (Iterator i = warns.iterator(); i.hasNext(); ) {
                    final String module = (String) i.next();
                    System.err.println("    " + module);
                }
            }
        }
        return subCell;
    }

    /**
     * Returns a new environment with all symbols defined in the
     * <code>inheritanceEnvList</code>.  Symbols in environments with
     * lower indices take priority.  Helper function for
     * <code>generateAndAttachCell</code>.
     **/
    private Environment chainEnvironments(final ArrayList inheritanceEnvList,
                                          Environment parentEnv) {
        // chain the <+ environments
        for (int i = inheritanceEnvList.size(); --i >= 0; ) {
            final Environment inheritanceEnv =
                (Environment) inheritanceEnvList.get(i);
            parentEnv = new SplicingEnvironment(parentEnv, inheritanceEnv);
        }

        return parentEnv;
    }

    private Environment spliceEnvironments(final Environment e1,
                                           final Environment e2,
                                           final Environment e3) {
        return new SplicingEnvironment(
                    // parent
                    e3,
                    // local
                    new SplicingEnvironment(e2, e1));
    }

    /**
     * Returns a new environment with all symbols defined in
     * the three environments.  Symbols in <code>e1</code> take precedence
     * over those in <code>e2</code>, etc.  Helper function for
     * <code>generateAndAttachCell</code>.
     **/
    private Environment spliceEnvironments(final Environment e1,
                                           final Environment e2,
                                           final Environment e3,
                                           final LocalEnvironment le) {
        return new FixedBlockEnvironment(spliceEnvironments(e1, e2, e3), le);
    }

    // TODO: make this a method on PortTypeInterface to avoid the ugly elseifs
    private PortTypeInterface substituteChannelWidth(
        final ParamTypeInterface type,
        final int width,
        final ASTWithInfo errorAST)
        throws SemanticException {
        if (type instanceof ArrayType) {
            final ArrayType arrayType = (ArrayType) type;
            return new ArrayType(
                substituteChannelWidth(arrayType.getArrayedType(), width, errorAST),
                arrayType.getMinIndex(), arrayType.getMaxIndex());
        } else if (type instanceof ChannelType) {
            final ChannelType channelType = (ChannelType) type;

            Debug.assertTrue(channelType.getWidth() == 1);
            return new ChannelType(channelType.iterator(),
                                   channelType.getTypeName(), width,
                                   channelType.getNumValues());
        } else if (type instanceof NodeType) {
            final NodeType nodeType = (NodeType) type;

            Debug.assertTrue(nodeType.getWidth() == 1);
            return new NodeType(width);
        } else if (type instanceof StructureType) {
            final StructureType structureType = (StructureType) type;
            return new ArrayType(structureType, 0, width-1);
        } else {
            throw semanticWrapperException("Type " + type +
                " not a channel type and hence unsuitable for channel" +
                " width specification.", new Exception(), errorAST);
        }
    }

    /**
     * Similar to CastTwoParser's syntaxError().  Takes a message and
     * an AST for location information, spits out a warning.
     * Eventually will turn into exceptions or some other method of
     * halting parsing.
     **/
    private void syntaxError(String message, ASTWithInfo ast) {
        if (verbose) {
            System.err.println("SYNTAX: " + ast.getFilename() + ": "
                               + ast.getLine() + ": " + message);
        }
    }

    /** Prints message and dies. **/
    private void fatalError(String message, ASTWithInfo ast) {
        System.err.println("fatal error in " + ast.getFilename() + " at " +
                           ast.getLine() + ":" + ast.getColumn() + ":");
        System.err.println(message);
        System.exit(1);
    }

    /**
     * Binds the symbol to the value in the environment, properly
     * augmenting if the symbol is already bound to an array that can
     * be augmented by the new value
     **/
    private void bindAndAugment(Environment env, Symbol sym, Value v,
                                ASTWithInfo errorAST, boolean aliasP)
        throws SemanticWrapperException {
        try {
            env.bind(sym, v);
        } catch (SymbolRedeclaredException e) {
            // it could be a sparse array, check

            // 1: get the old value
            final Value vOld = lookup(env, sym, errorAST);

            // 2: The old and new values must both be arrays.
            if (!(v instanceof ArrayValue && vOld instanceof ArrayValue))
                throw semanticWrapperException("variable " + sym.getString()
                                               + " redeclared", e, errorAST);

            // this function is called twice for each declaration in an alias
            // block; don't augment during the second call, since the same
            // ArrayValue object must have been successfully augmented already
            // in the previous call
            if (!aliasP) {
                final ArrayValue av = (ArrayValue) v;
                final ArrayValue avOld = (ArrayValue) vOld;

                // 3: attempt to augment the array with the new indices
                //    this may fail if:
                //      the element types are not the same
                //      the dimensions don't agree
                //      the indices are not disjoint
                try {
                    avOld.augment(av);
                } catch (InvalidOperationException ee) {
                    throw semanticWrapperException("Unable to augment sparse"
                        + " array " + sym.getString(), ee, errorAST);
                }
            }
        }
    }

    private static SequentialStatement makeCSPConstantInitializers
            (final Environment env) throws InvalidOperationException {
        final SequentialStatement stmt = new SequentialStatement();
        for (EnvironmentEntryIterator iEE = env.entryIterator();
                iEE.hasNext(); ) {
            final EnvironmentEntry entry = (EnvironmentEntry) iEE.next();
            final String name = entry.getName().getString();
            final Value val = entry.getValue();

            final IdentifierExpression lhs = new IdentifierExpression(name);
            if (val instanceof BoolValue || val instanceof IntValue) {
                // jikes 1.17 thinks these can't be final
                /*final*/ com.avlsi.csp.ast.Type type;
                /*final*/ ExpressionInterface initExpr;
                if (val instanceof BoolValue) {
                    final BoolValue bv = (BoolValue) val;
                    // System.out.println("bool " + name + " = " +
                    //                    bv.getValue());
                    type = new BooleanType(true);
                    initExpr =
                        new BooleanExpression(bv.getValue());
                } else {
                    final IntValue iv = (IntValue) val;
                    // System.out.println("int " + name + " = " +
                    //                    iv.getValue());
                    type = new IntegerType(true);
                    initExpr =
                        new IntegerExpression(iv.getValue().toString(), 10);
                }

                // make the DeclaratorList for the VarStatement
                final DeclaratorList declList = new DeclaratorList();
                final Declarator declarator =
                    new Declarator(lhs, null, initExpr);
                declList.addDeclarator(declarator);

                // make the VarStatement and add it
                stmt.addStatement
                    (new VarStatement(new Declaration(declList, type)));
            } else if (val instanceof ArrayValue) {
                final ArrayValue av = (ArrayValue) val;
                final SubscriptSpecInterface spec = av.getSpec();
                if (spec instanceof DenseSubscriptSpec) {
                    final DenseSubscriptSpec denseSpec =
                        (DenseSubscriptSpec) spec;

                    // get the base type
                    com.avlsi.csp.ast.Type type;
                    if (av.getElementType() == IntValue.TYPE) {
                        // System.out.println("int[] " + name);
                        type = new IntegerType(true);
                    } else if (av.getElementType() == BoolValue.TYPE) {
                        // System.out.println("bool[] " + name);
                        type = new BooleanType(true);
                    } else {
                        continue;
                    }

                    // build up the array type
                    final int nDims = denseSpec.getNumDimensions();
                    for (int i = nDims; --i >= 0; ) {
                        final Range r = denseSpec.getRange(i);
                        type = new com.avlsi.csp.ast.ArrayType
                            (new com.avlsi.csp.ast.Range
                                (new IntegerExpression(r.getMin()),
                                 new IntegerExpression(r.getMax())),
                             type);
                    }

                    // declare the array
                    final DeclaratorList declList = new DeclaratorList();
                    final Declarator declarator =
                        new Declarator(lhs, null, null);
                    declList.addDeclarator(declarator);
                    stmt.addStatement
                        (new VarStatement(new Declaration(declList, type)));

                    // fill in all the values
                    final int nVals = denseSpec.getNumElements();
                    for (int i = 0; i < nVals; ++i) {
                        final int[] idx = spec.indexOf(i);
                        final Value v = av.accessArray(idx);
                        stmt.addStatement
                            (new AssignmentStatement
                                (makeArrayLHS(name, idx), makeCSPValue(v)));
                    }
                }
            }
        }

        return stmt;
    }

    private static ExpressionInterface makeArrayLHS(String name, int[] idx) {
        ExpressionInterface lhs = new IdentifierExpression(name);

        final int nDims = idx.length;
        for (int i = 0; i < nDims; ++i) {
            lhs = new ArrayAccessExpression(lhs,
                                            new IntegerExpression(idx[i]));
        }

        return lhs;
    }

    private static ExpressionInterface makeCSPValue(Value val)
            throws InvalidOperationException {
        if (val instanceof BoolValue) {
            final BoolValue bv = (BoolValue) val;
            return new BooleanExpression(bv.getValue());
        } else {
            final IntValue iv = (IntValue) val;
            return new IntegerExpression(iv.getValue().toString(), 10);
        }
    }

    /**
     * Given a pair of HierNames which represent Nodes (although they
     * may be in arrays, in which case the relevant NodeValues won't
     * exist) into a BooleanExpressionInterface which can either
     * become the guard of a production rule or be combined with other
     * BooleanExpressionInterfaces to become the guard.
     **/
    private BooleanExpressionInterface makeAssertPair(final Environment env,
                                                      final int hiLo,
                                                      final HierName node1,
                                                      final HierName node2,
                                                      final ASTWithInfo errorAST)
        throws SemanticWrapperException {
        final List nodes = new ArrayList(2);
        nodes.add(0, new HierNameAtomicBooleanExpression(true, node1));
        nodes.add(1, new HierNameAtomicBooleanExpression(true, node2));
        
        if (hiLo == ExclusiveNodeSet.HI) {
            return new AndBooleanExpression(true, nodes);
        } else if (hiLo == ExclusiveNodeSet.LO) {
            return new OrBooleanExpression(true, nodes);
        } else {
            throw new AssertionFailure("Invalid hiLo value: " + hiLo);
        }
    }

    /**
     * Given a list of BooleanExpressionInterfaces representing pairs
     * of nodes (see comment inside assertExclStatement[]) and the
     * exclusivity type, produces an asserted production rule.
     **/
    private ProductionRule makeAssertRule(final int hiLo,
                                          final List guards) {
        if (hiLo == ExclusiveNodeSet.HI) {
            final BooleanExpressionInterface guard = new OrBooleanExpression(true, guards);
            return new ProductionRule(guard, HierName.makeHierName("ERROR!"),
                                      HierName.makeHierName("GND!"),
                                      ProductionRule.UP, false, false, false,
                                      false, 100, false);
        }
        else if (hiLo == ExclusiveNodeSet.LO) {
            final BooleanExpressionInterface guard = new AndBooleanExpression(false, guards);
            return new ProductionRule(guard, HierName.makeHierName("ERROR!"),
                                      HierName.makeHierName("GND!"),
                                      ProductionRule.UP, false, false, false,
                                      false, 100, false);
        } else {
            throw new AssertionFailure("Invalid hiLo value: " + hiLo);
        }
    }

    private static String modulizeOldJavaChannel(String channelDecl) {
        final int idx = channelDecl.indexOf('(');
        if (idx == -1 || idx == channelDecl.length() - 1)
            return channelDecl;
        else
            return channelDecl.substring(0, idx + 1) +
                   "standard.channel." +
                   channelDecl.substring(idx + 1);
    }

    private ASTWithInfo parseDumbBlockAs(final ASTWithInfo info,
                                         final CastTwoUtil.ParserFactory f)
        throws RecognitionException, TokenStreamException {
        return (ASTWithInfo)
            CastTwoUtil.parseStringAs(info.getText(), info.getLine(),
                                      info.getColumn(), info.getFilename(),
                                      verbose, f);
    }

    private void parseDumbBlockAs(final ASTWithInfo info,
                                  final CastTwoUtil.ParserFactory f,
                                  final CastTwoUtil.ParserCallback cb)
        throws RecognitionException, TokenStreamException {
        CastTwoUtil.parseStringAs(info.getText(), info.getLine(),
                                  info.getColumn(), info.getFilename(),
                                  verbose, f, cb);
    }

    private void parseDirectives(final String s, final int lineNum,
                                 final int column, final String filename,
                                 final Environment env, final String type,
                                 final BlockInterface parent)
    throws RecognitionException {
        final DirectiveParser parser =
            new DirectiveParser(new StringReader(s), new DirectiveFactory(),
                                lineNum, column - 1, filename);
        final DirectiveStatement[] statements;
        try {
            statements = parser.parseStatements();
        } catch (IOException e) {
            throw semanticWrapperException("IOException while parsing directive block", e, filename, lineNum, column);
        } catch (DirectiveSyntaxException e) {
            throw semanticWrapperException(e, e.getFilename(), e.getLine(),
                                           e.getColumn());
        }
        final DirectiveImpl impl =
            new DirectiveImpl(type, env, directiveErrHandler);
        for (int i = 0; i < statements.length; ++i) {
            statements[i].visit(impl);
        }
        parent.iterator(BlockInterface.DIRECTIVE).merge(new DirectiveBlock(impl.getDirectiveInterface()));
    }

    private void initializeImpliedPorts(final Value v, final CellImpl cell, 
                                        final ASTWithInfo errorAST)
    throws RecognitionException {
        if (v instanceof InstanceValue) {
            try {
                ((InstanceValue) v).assignDefaultsToImpliedPorts(cell);
            } catch (InvalidOperationException e) {
                throw semanticWrapperException("unable to bind implied ports to defaults", e, errorAST);
            }
        } else if (!opt.bug3771HackEnabled() && v instanceof ArrayValue) {
            for (Iterator i = ((ArrayValue) v).getIterator(); i.hasNext(); ) {
                initializeImpliedPorts((Value) i.next(), cell, errorAST);
            }
        }
    }

    public Value expr(AST ast, Environment env, boolean anonymousAllowedP)
    throws RecognitionException {
        return expr(ast, env, anonymousAllowedP, false);
    }

    public TupleValue expressionList(AST ast, Environment env,
                                     boolean anonymousAllowedP)
    throws RecognitionException {
        return expressionList(ast, env, anonymousAllowedP, false);
    }

    public Value expression(AST ast, Environment env, boolean anonymousAllowedP)
    throws RecognitionException {
        return expression(ast, env, anonymousAllowedP, false);
    }


    private interface Binder {
        Value lookup() throws AmbiguousLookupException,
                              InvalidOperationException;
        void bind(Value v) throws SymbolRedeclaredException,
                                  InvalidOperationException;
    }

    private static class SimpleBinder implements Binder {
        private final Environment env;
        private final Symbol sym;
        public SimpleBinder(final Environment env, final Symbol sym) {
            this.env = env;
            this.sym = sym;
        }
        public Value lookup() throws AmbiguousLookupException,
                                     InvalidOperationException {
            return env.lookup(sym);
        }
        public void bind(Value v) throws SymbolRedeclaredException,
                                         InvalidOperationException {
            env.bind(sym, v);
        }
    }

    private static class ArrayBinder implements Binder {
        private final ArrayValue val;
        private final SubscriptSpecInterface index;
        public ArrayBinder(final ArrayValue val,
                           final SubscriptSpecInterface index) {
            this.val = val;
            this.index = index;
        }
        public Value lookup() throws AmbiguousLookupException,
                                     InvalidOperationException {
            return val.accessArray(index);
        }
        public void bind(Value v) throws SymbolRedeclaredException,
                                         InvalidOperationException {
            val.replaceArray(
                new ArrayValue(new Value[] { v }, index,
                               val.getInstanceName(), false));
        }
    }

    private static class TrivialParserCallback
    implements CastTwoUtil.ParserCallback {
        public void bodyStatement(final ASTWithInfo ast)
            throws RecognitionException {}
        public void ifStart(final ASTWithInfo expr)
            throws RecognitionException {}
        public void ifEnd() throws RecognitionException {}
        public void loopStart(final ASTWithInfo ident, final ASTWithInfo range)
            throws RecognitionException {}
        public void loopEnd() throws RecognitionException {}
    }

    private class SubcellsParserCallback extends TrivialParserCallback {
        private final Environment env;
        private final CellImpl cell;
        SubcellsParserCallback(final Environment env,
                               final CellImpl cell) {
            this.env = env;
            this.cell = cell;
        }

        public void bodyStatement(final ASTWithInfo ast)
            throws RecognitionException {
            CastTwoTreeParser.this.subcellsStatements(ast, env, cell);
        }
    }

    private class SubtypesParserCallback extends TrivialParserCallback {
        private final Environment env;
        private final Map copied;
        private final CellImpl cell;
        SubtypesParserCallback(final Environment env,
                               final Map copied,
                               final CellImpl cell) {
            this.env = env;
            this.copied = copied;
            this.cell = cell;
        }

        public void bodyStatement(final ASTWithInfo ast)
            throws RecognitionException {
            CastTwoTreeParser.this.subtypesStatements(ast, env, copied, cell);
        }
    }
}

goal[CellImpl cell, String moduleName, LinkedList fileList]
returns [Environment exportedEnv]
    {
        exportedEnv = null;
        Debug.assertTrue(cpe != null);
        // We'd like to be able to assert this, but it breaks
        // parsing by file name in jflat, and possibly other places
        // Debug.assertTrue(moduleName != null,
            // "Not parsed via fully qualified name");
    }
    : exportedEnv=compilationUnit[cell, moduleName, fileList]
      EOF
    ;

compilationUnit[CellImpl cell,
                String instantiatorModuleName,
                LinkedList fileList]
returns [Environment exportedEnv]
    {
        exportedEnv = null;
        final ImportEnvironment importEnv = cpe.newImportEnvironment();
        final TopLevelEnvironment env = new TopLevelEnvironment(importEnv);
    }
    : ( moduleDeclaration[instantiatorModuleName] )
      ( importDeclaration[cell, importEnv, fileList] )*
      ( globalBodyStatement[env, cell]
        | typeDeclaration[env, false]
        | channelDeclaration[env]
        | aliasDeclaration[env]
      )*
    { exportedEnv = env.getExportedEnvironment(); }
    ;

moduleDeclaration[String instantiatorModuleName]
    : #( m:MODULE id:IDENT )
    {
        moduleName = id.getText();
        if (instantiatorModuleName != null &&
            !instantiatorModuleName.equals(moduleName)) {
            final SemanticException se
                = new SemanticException("Declared module name '" +
                    moduleName +
                    "' doesn't match instantiator module name '" + 
                    instantiatorModuleName + '\'',
                    id.getFilename(), id.getLine(), id.getColumn());
            se.column = id.getColumn();
            throw se;
        }
    }
    ;

importDeclaration[CellImpl cell, ImportEnvironment importEnv,
                  LinkedList fileList]
    : #( IMPORT id:IMPORT_IDENT )
    {
        final String imported;
        imported = id.getText();
        int lastDot = imported.lastIndexOf('.');
        Debug.assertTrue(lastDot != -1);

        final String moduleName = imported.substring(0, lastDot);
        String cellName = imported.substring(lastDot + 1);
        if (cellName.equals("*")) cellName = null;

        try {
            final Environment parsed = cellName == null ?
                // all cells in file
                cpe.parseModule(fileList, moduleName) :
                cpe.parseCell(fileList, moduleName, cellName);
            if (cellName == null) {
                importEnv.addEnvironment(parsed);
            }
            else {
                final Environment cellEnv = new LocalEnvironment();
                Symbol cellSymbol = Symbol.create(cellName);
                try {
                    cellEnv.bind(cellSymbol, lookup(parsed, cellSymbol, id));
                }
                catch (SymbolRedeclaredException e) {
                    throw semanticWrapperException(e, id);
                }
                importEnv.addEnvironment(cellEnv);
            }
        } catch (CircularImportException e) {
            throw semanticWrapperException("Circular import of " +
            e.getCurrentImport() + "; " + "already imported " +
            e.getImportedFiles(), e, id);
        } catch (SelfImportException e) {
            warn("Warning: self-import " + imported, id);
        } catch (FileNotFoundException e) {
            throw semanticWrapperException("Import not found: "
                + imported, e, id);
        } catch (IOException e) {
            throw semanticWrapperException("Error in import", e, id);
        } catch (antlr.RecognitionException e) {
            throw semanticWrapperException("Error in import", e, id);
        } catch (antlr.TokenStreamException e) {
            throw semanticWrapperException("Error in import", e, id);
        }
    }
    ;

// bodyStatementLists don't appear at the top level
// pass globalP == false to bodyStatement
//
// See block[] for description of prsEnv
//
// paramEnv can be null if an env block is not allowed at this level.
// Otherwise it's the environment in which the metaparameters, ports,
// and implied ports of cell are bound, but none of the body of cell.
// It's read-only.
bodyStatementList[Environment env, CellImpl cell, CellInterface envContainer,
                  BlockEnvironment paramEnv, Environment prsEnv,
                  Environment subcellsEnv, Environment subtypesEnv]
    : #( BODY_STATEMENT_LIST
         ( bodyStatement[env, false, cell, envContainer, paramEnv, prsEnv,
                         subcellsEnv, subtypesEnv] )* )
    ;

globalBodyStatement[Environment env, CellImpl cell]
    : bodyStatement[env, true, cell, null, null, null, null, null]
    ;

// globalP: whether the body statement is at a global level
//
// See block[] for description of prsEnv
//
// paramEnv can be null if an env block is not allowed at this level.
// Otherwise it's the environment in which the metaparameters, ports,
// implied ports, and constants of cell are bound, but none of the body of
// cell.  It's read-only.
bodyStatement[Environment env, boolean globalP, CellImpl cell,
              CellInterface envContainer, BlockEnvironment paramEnv,
              Environment prsEnv, Environment subcellsEnv,
              Environment subtypesEnv]
    {
        //boolean processEnv = !globalP && null !=
        //    ((UserDefinedValue) activeUDV.getLast()).getEnvironmentContainer();
        boolean processEnv = envContainer != null;
        BlockInterface dirBlock = processEnv ?
            cell.getBlockInterface().iterator(BlockInterface.ENV).next()
          : cell.getBlockInterface();
    }
    : prs:prsBlock[prsEnv, cell]
      { if (cell.containsSubcells())
          fatalError("shouldn't have prs and subcells in the same cell",
                     prs);
      }
    | subs:subcellsBlock[subcellsEnv, cell]
      { if (cell.containsCompletePrs() || cell.containsFragmentPrs())
          fatalError("shouldn't have prs and subcells in the same cell",
                     subs);
      }
    | cspBlock[env, cell, envContainer]
    | envBlock[cell, paramEnv]
    | javaBlock[new FixedBlockEnvironment(env), cell, !processEnv]
    | netlistBlock[new FixedBlockEnvironment(env), cell]
    | directiveBlock[env, dirBlock,
                     processEnv ? BlockInterface.ENV : BlockInterface.CELL ]
    | block[new FixedBlockEnvironment(env), cell, envContainer, null, null,
            null, null]
    | variableDeclarationStatement[env, globalP, cell, envContainer, null,
                                   false]
    | loopStatement[env, cell, envContainer, paramEnv, prsEnv, subcellsEnv,
                    subtypesEnv]
    | ifStatement[env, cell, envContainer, paramEnv, prsEnv, subcellsEnv,
                  subtypesEnv]
    | assignmentStatement[env, cell]
    | subtypesBlock[subtypesEnv, cell]
    | verilogBlock[new FixedBlockEnvironment(env), cell]
    ;

// paramEnv can be null if an env block is not allowed at this level.
// Otherwise it's the environment in which the metaparameters, ports,
// and implied ports of cell are bound, but none of the body of cell.
// It's read-only.
//
// prsEnv can be null if a prs block is not allowed at this level.
// Otherwise it's the environment in which the production rules
// are defined.  Its local environment is initially empty, but variables
// declared in prs blocks are added to it.  Its parent environment
// has a local environment of the cell constants and a parent of
// the inheritance parent's prsEnv.
// NB:  Multiple prs blocks go in the same environment with this
//      implementation.  We should probably allow only one prs block
//      to hide this.
//
// env is the Environment block[] should be modifying.  It may already
// have some values in it.  When block[] finishes, env will contain
// any constants defined at the top-level of this block.
block[Environment env, CellImpl cell, CellInterface envContainer,
      BlockEnvironment paramEnv, Environment prsEnv, Environment subcellsEnv,
      Environment subtypesEnv]
    : #( BLOCK bodyStatementList[env, cell, envContainer, paramEnv, prsEnv,
                                 subcellsEnv, subtypesEnv] )
    ;

// globalP: true if the variableDeclarationStatement occurs at the
// top-level (globally).  If so, then a ! will be appended to the 
// node name in primitiveType.
// aliasesEnv is null if this statement is anywhere but in an aliases block.
// In an aliases block it is the running accumulator of aliases to export
// prsP: true if this declaration is in a prs block.  If so, then all
// subcell defns are automatically flattened
// envContainer: if processing an environment cell, the cell containing the
// environment, or null otherwise.
variableDeclarationStatement[Environment env, boolean globalP, CellImpl cell,
    CellInterface envContainer, LocalEnvironment aliasesEnv, boolean prsP]
    { final Pair p; Value vInit = null; String varName = null; 
      boolean inlineP = false, flattenP = prsP, aliasP = (aliasesEnv != null); }
    : #( VAR_DECL id:IDENT { varName = generateName(id.getText()); }
         ( INLINE { inlineP = true; } )?
         ( FLATTEN { flattenP = true; } )?
         p=type[env, cell, envContainer, HierName.makeHierName(varName),
                globalP, inlineP, flattenP, id, LOCAL_VAR]
         ( vInit=variableInitializer[env] )? )
    {
        // Variables declared in the alias block must be
        // externally accessible.  The environment passed in to
        // the alias block is the parameter environment for this
        // reason.  The call to type[] bound them externally.
        final Symbol sym = Symbol.create(varName);
        final Value v = (Value) p.getFirst();

        if (aliasP && vInit == null)
            throw semanticWrapperException("aliases must be initialized as part of their declaration", new Exception(), id);
        
        // NOTE that the environment is augmented after the initializer
        // is walked

        bindAndAugment(env, sym, v, id, false);
        if (aliasP) bindAndAugment(aliasesEnv, sym, v, id, true);

        final Value override = opt.getOverrideValue(cell, sym, vInit, env);
        if (override != null) vInit = override;

        // assignment of value = tuple takes care of connections.
        if (vInit != null)
            assign(v, vInit, cell, id, "Incompatible types for " +
                   (override == null ? "initializer" : "overrider") +
                   " or port list.");
        else
            initializeImpliedPorts(v, cell, id);
    }
    ;

// ASSIGN:    returns a non-tuple expression
// PORT_LIST: returns a     tuple expression or a pair of tuples (real and implied ports)
variableInitializer[Environment env] returns [Value v]
    { v = null;
      TupleValue implied = null; }
    : #( ASSIGN v=expression[env, false] )
    | #( PORT_LIST
         v=expressionList[env, true]
         ( implied=expressionList[env, true]
           { v = new TupleGroupValue((TupleValue)v, implied); }
         )?
       )
    ;

// Returns the definitive pair of Value and ParamTypeInterface for type[],
// adjusting based on channel width if necessary.
setFinalWidth[Environment env, Pair baseTypePair, Pair arrayedPair]
returns [Pair p]
    {
        final Value baseValue = (Value) baseTypePair.getFirst();
        final Value startingValue;
        final ParamTypeInterface baseParamType = (ParamTypeInterface) baseTypePair.getSecond();
        final ParamTypeInterface startingType;
        final int startingDimensions;
        final DenseSubscriptSpec s;
        final int channelWidth;
        final Value channelWidthValue;
    }
    : #( errorAST:CHANNEL_WIDTH
         ( channelWidthValue=expr:expression[env,false]
           | /* empty */ { channelWidthValue = null; }
         )
       )
    {
        // Dimensions before allowing for possible channel width
        if (arrayedPair == null) {
            s = null;
            startingDimensions = 0;
            startingValue = baseValue;
            startingType = baseParamType;
        }
        else {
            s = (DenseSubscriptSpec) arrayedPair.getFirst();
            startingDimensions = s.getNumDimensions();
            startingValue = ArrayValue.makeArray(baseValue, s);
            startingType = (ParamTypeInterface) arrayedPair.getSecond();
        }

        // If channel didn't really have a width, leave it alone
        if (channelWidthValue == null)
            p = new Pair(startingValue, startingType);
        else {
            // Interpreting channel width
            try {
                channelWidth = ((IntValue) channelWidthValue).getValue().intValue();
            } catch (InvalidOperationException e) {
                throw semanticWrapperException("error understanding channelwidth",
                                               e, errorAST);
            }
            if (channelWidth < 1)
                throw semanticWrapperException("channel width must be positive, not: " + channelWidth, new Exception(), errorAST);
            else {
                // augment the spec with the new dimension
                Range[] newDimensions = new Range[startingDimensions + 1];
                for (int i=0; i<startingDimensions; i++) {
                    newDimensions[i] = s.getRange(i);
                }
                newDimensions[startingDimensions] = new Range(0, channelWidth-1);
                final SubscriptSpecInterface newSpec=
                    new DenseSubscriptSpec(newDimensions);
                
                final ParamTypeInterface newType;
                if (startingType == null) { // If this was internal to the cell
                    newType = null;
                } else {
                    newType =
                        substituteChannelWidth(startingType,
                                               channelWidth,
                                               errorAST);
                }
                p = new Pair(ArrayValue.makeWideArray(baseValue, newSpec),
                             newType);
            }
        }
    }
    ;

// returns Pair of an uninitialized value of the appropriate type
//     and a ParamTypeInterface decribing the type
// cell: the cell to which nodes and connections should be added
//   if processing a port-param list, or body variable declaration,
//   null if processing a meta-param list
type[Environment env, CellImpl cell, CellInterface envContainer,
     HierName varName, boolean globalP, boolean inlineP, boolean flattenP,
     ASTWithInfo id, int declKind]
returns [Pair p]
    { final Pair pb, pWidth; Pair ps = null; p = null; }
    : #( TYPE pb=bt:baseType[env, env, cell, envContainer, varName, globalP]
         ( ps=arraySelector[env, declKind != LOCAL_VAR
                             ? (ParamTypeInterface) pb.getSecond()
                             : (ParamTypeInterface) null,
                            declKind == META_PARAM]
         )?
         ( pWidth=setFinalWidth[env, pb, ps] )
       )
    {
        try {
            // value to be returned
            final Value v = (Value) pWidth.getFirst();

            // type to be returned
            final ParamTypeInterface paramType = (ParamTypeInterface) pWidth.getSecond();

            // value of base type
            final Value vb = (Value) pb.getFirst();

            // XXX: keep track of which subcells are port subcells

            // if we have a subcell or node, then add to the list of subcells
            if (vb instanceof InstanceValue || vb instanceof NodeValue) {
                final CellInterface subcell;
                // TODO: refactor this shit, maybe getting rid of NodeValue
                if (vb instanceof InstanceValue) {
                    subcell = ((InstanceValue) vb).getCell();
                } else {
                    subcell = ((NodeValue) vb).getCell();
                }

                final Iterator i = v instanceof ArrayValue ?
                    ((ArrayValue) v).getIterator() :
                    Collections.singletonList(vb).iterator();

                while (i.hasNext()) {
                    final Value elem = (Value) i.next();
                    final HierName subcellName = elem.getInstanceName();
                    cell.addSubcellPair(subcellName, subcell,
                        declKind == PORT_PARAM,
                        id == null ? -1 : id.getLine(),
                        id == null ? -1 : id.getColumn());
                    if (vb instanceof NodeValue) cell.addNode(subcellName);

                    Debug.assertTrue(!(inlineP && flattenP));

                    final InstanceValue inst = elem instanceof InstanceValue ?
                        (InstanceValue) elem : null;

                    if ((inlineP || subcell.isAutoInline()) &&
                        opt.processInline(subcell)) {
                        cell.inlineSubcell(subcellName);
                        if (inst != null)
                            inst.setInline(InstanceValue.INLINE_INLINE);
                    }
                    if (flattenP) {
                        cell.flattenSubcell(subcellName);
                        if (inst != null)
                            inst.setInline(InstanceValue.INLINE_FLATTEN);
                    }
                }
            }

            // package return values
            p = new Pair(v, paramType);

        } catch (NoSuchSubcellException e) {
            throw semanticWrapperException("subcell not found to be " +
                                           "flattened or inlined.", e, bt);
        } catch (SubcellCreationException e) {
            throw semanticWrapperException(e, bt); 
        }
    }
    ;

// returns a pair of an instance value of the given type and a
// PortTypeInterface
// cell: the cell to which nodes and connections should be added
//   if processing a port-param list, or body variable declaration,
//   null if processing a meta-param list
// FIXME: kwallmar: optimize more, refactor in general
baseType[Environment metaEnv, Environment env, CellImpl cell,
         CellInterface envContainer, HierName varName, boolean globalP]
        returns [Pair p]
    { p = null; }
    : { TupleValue tupleVal; }
      #( USER_TYPE id:IDENT
                   tupleVal=expressionList[metaEnv, false] )
      {
          // These index numbers have changed recently (Jan 2002)
          // FIXME: should be able to look up old InstanceValue if
          // this has already been defined?
          // 1: check that there is a symbol of the given name
          // 2: check that the symbol has a type value
          // 3: Restore the environment under which the
          //    body should be evaluated (the definitin could
          //    have come from another file)
          //    Restore the AST for the parameters and bod4y
          // 4: make a local environment that will be used for 
          //    the parameters
          // 5: bind the meta parameters w/ values
          // 6: have we already generated the CellInterface
          //    for this type + meta params? If so, go to 7.  If
          //    not, go to 8.
          // 7: attach the ports in the environment.  Use a dummy cell
          //    so as not to change the predefined type.  Done with
          //    pre-generated cell.
          // 8: If necessary, get the cell + InstanceValue for the
          //    refinement parent.  Get its implied ports.
          // 9: Bind the ports w/ empty values.
          // 10:Bind the implied ports w/ empty values, modifying due
          //    to refinement if necessary.
          // 11:evaluate the body
          // 12:If necessary, apply the refinement parent

          // the port params are bound in variableDeclarationStatement
          //   by assigning a tuple to the value

          // 1
          final String idText = id.getText();
          final Symbol sym = Symbol.create(idText);
          final Value val = lookup(env, sym, id);

          // 2
          if (!(val instanceof UserDefinedValue)) {
              final SemanticException se
                  = new SemanticException(idText +
                      " not a user defined type",
                      id.getFilename(), id.getLine(), id.getColumn());
              se.column = id.getColumn();
              throw se;
          }
          final UserDefinedValue typeVal = (UserDefinedValue) val;

          // 3
          final Environment bodyEnv = typeVal.getEnvironment();
          final LocalEnvironment inheritedConstants = new LocalEnvironment();

          final LocalEnvironment metaParamEnv = new LocalEnvironment();
          final BlockEnvironment portEnv =
              new BlockEnvironment(
                  new BlockEnvironment(bodyEnv, inheritedConstants),
                  metaParamEnv);

          // 4
          final LocalEnvironment localEnv = new LocalEnvironment();
          final BlockEnvironment paramEnv =
              new BlockEnvironment(portEnv, localEnv);
          
          // 5
          final AST metaParamList = typeVal.getMetaParamList();
          final List metaParams;
          try {
              metaParams = metaParamList(metaParamList, portEnv, tupleVal);
          } catch (InvalidOperationException e) {
              throw semanticWrapperException("trouble assigning metaparameters when instantiating "+idText, e, id);
          }

          // 6
          CellImpl subCell = (CellImpl) typeVal.getCell(tupleVal);
          final String typeName
              = UserDefinedValue.getTypeName(
                  idText.substring(idText.lastIndexOf(".") + 1),
                  tupleVal);
          final ArrayList portList = new ArrayList();
          final ArrayList impliedPortList = new ArrayList();
          final ArrayList impliedPortParentNodes = new ArrayList();
          if (subCell != null) {
              // 7
              try {
                  attachGeneratedCell(tupleVal, paramEnv, localEnv, typeVal,
                      portList, impliedPortList, impliedPortParentNodes,
                      typeVal.getAliases(tupleVal));
              } catch (SymbolRedeclaredException e) {
                  throw semanticWrapperException("trouble attaching pre-generated cell: " + e.getMessage(), e, id);
              }
          }
          else {
              final String fullName;
              if (envContainer == null) {
                  fullName = UserDefinedValue.getTypeName(
                      typeVal.getFullyQualifiedType(), tupleVal);
              } else {
                  fullName = envContainer.getFullyQualifiedType() + "_" +
                             UserDefinedValue.getTypeName(
                                typeVal.getCellTypeName(), tupleVal);
              }
              if (!cellList.add(fullName)) {
                  throw new SemanticException("recursive use of " + fullName,
                              id.getFilename(), id.getLine(), id.getColumn());
              }
              try {
                  subCell = generateAndAttachCell(tupleVal, id, typeVal,
                      paramEnv, inheritedConstants, portEnv, metaParamEnv,
                      localEnv, metaParams, typeName, portList, impliedPortList,
                      impliedPortParentNodes, envContainer);
              } catch (SemanticException e) {
                  // Wrap the exception again to give better error messages.
                  // Now we will get the whole stack of cells that were trying
                  // to be generated, insead of just the one that failed.
                  // That way people can figure out why a cell was being
                  // instantiated.  Helpful for cells with metaparameters.

                  // Cast e to Exception to force semanticWrapperException
                  // to use the id for the position, not the
                  // SemanticException.  (There are two overloaded 
                  // semanticWrapperException()s, one taking Exception,
                  // and one taking RecognitionException, of which 
                  // SemanticException is a subtype
                  throw semanticWrapperException("Error instantiating " +
                                                 typeName, (Exception) e, id);
              }
              cellList.remove(fullName);
          }

          final String moduleName;
          final String fullyQualifiedTypeName;
          if (typeVal.getModuleName() == null) {
              moduleName = "";
              fullyQualifiedTypeName = typeName;
          } else {
              moduleName = typeVal.getModuleName();
              fullyQualifiedTypeName = moduleName + '.' + typeName;
          }
          PortTypeInterface portType;
          if (CellUtils.isAsyncChannel(subCell)) {
              portType = new ChannelType(subCell.getPortDefinitions(),
                                         fullyQualifiedTypeName,
                                         CellUtils.getNumValues(subCell));
          } else {
              portType = new StructureType(subCell.getPortDefinitions(),
                                           fullyQualifiedTypeName);
          }

          p = new Pair(
              new InstanceValue(varName, subCell,
                  Symbol.create(typeVal.getFullyQualifiedType()),
                  tupleVal, paramEnv,
                  typeVal.getPrsEnvironment(tupleVal),
                  typeVal.getSubcellsEnvironment(tupleVal),
                  (Symbol []) portList.toArray(new Symbol[0]),
                  (Symbol []) impliedPortList.toArray(new Symbol[0]),
                  (String []) impliedPortParentNodes.toArray(new String[0]),
                  new int[] { InstanceValue.INLINE_NONE }),
              portType);
      }
    | p=primitiveType[env, cell, varName, globalP]
    ;

// return Pair of the default value of the appropriate type
//     and ParamTypeInterface for that type, or null if the type is not a node
// (default is un-initialized, for algebraic types).
// cell: the cell to which nodes and connections should be added
//   if processing a port-param list, or body variable declaration,
//   null if processing a meta-param list
primitiveType[Environment env, CellImpl cell, HierName varName,
              boolean globalP]
returns [Pair p]
    { p = null; }
    : BOOL   { BoolValue  v = new BoolValue();  p = new Pair(v, v); }
    | INT    { IntValue   v = new IntValue();   p = new Pair(v, v); }
    | FLOAT  { FloatValue v = new FloatValue(); p = new Pair(v, v); }
    | NODE   
      {
          Debug.assertTrue(varName.getNumComponents() == 1);

          // if the node was declared globally, append '!' to the name
          // to flag that it is a global node.  this will be used by
          // flatten() so that the cell name in not prepended.
          if (globalP) {
              varName = HierName.makeHierName(
                  varName.getSuffixString() + '!');
          }

          p = new Pair(new NodeValue(varName), new NodeType());
      }
    ;

channelDeclaration[Environment env]
    : #( DEFCHAN
         name:IDENT
         ( metas:FORMALS ( netGroup:FORMALS )? )?
         ( inheritanceList:INHERITANCE_LIST )?
         ( refinement:REFINEMENT )?
         body:BODY_STATEMENT_LIST
       )
    {
        try {
            final Symbol cellTypeSymbol = Symbol.create(name.getText());
            if (opt.orphanBdcHackEnabled() &&
                moduleName.equals("standard.channel") &&
                name.getText().equals("bdc")) {
                refinement = null;
            }
            env.bind(cellTypeSymbol, 
                     new UserDefinedValue(env, cellTypeSymbol, metas,
                                          netGroup, fakeEmptyPorts(name),
                                          null, inheritanceList,
                                          refinement, body, moduleName,
                                          UserDefinedValue.CHANNEL));
        } catch (SymbolRedeclaredException e) {
            throw semanticWrapperException("type " + name.getText()
                + " redeclared", e, name);
        }
        //        Debug.assertTrue(false, "don't use defchan yet");
    }
    ;

// bodyBlocks is a map from String (name of the block) to an
// ASTWithInfo containing the unparsed contents of the block and
// file/line information
// aliasesEnv is a temporary environment containing the aliases, to be
// added to the relevant UserDefinedValue 
channelBody[Environment env, CellImpl cell]
returns [LocalEnvironment aliasesEnv]
    { aliasesEnv = new LocalEnvironment(); }
    : #( BODY_STATEMENT_LIST ( channelStatement[env, cell, aliasesEnv] )* )
    ;

channelStatement[Environment env, CellImpl cell, LocalEnvironment aliasesEnv]
    : aliasBlock[env, cell, aliasesEnv]
    | assertBlock[env, cell]
    | directiveBlock[env, cell.getBlockInterface(), BlockInterface.CELL ]
    ;

aliasDeclaration[Environment env]
    : #( DEFALIAS
         name:IDENT
         ( metas:FORMALS )?
         refinement:REFINEMENT
       )
    {
        try {
            // construct a fake AST for an empty block, i.e., { }
            final Symbol cellTypeSymbol = Symbol.create(name.getText());
            final ASTWithInfo block = fakeEmptyBlock(refinement);
            env.bind(cellTypeSymbol, 
                     new UserDefinedValue(env, cellTypeSymbol, metas,
                                          null, null, null, null,
                                          refinement, block, moduleName,
                                          UserDefinedValue.ALIAS_CELL));
        } catch (SymbolRedeclaredException e) {
            throw semanticWrapperException("type " + name.getText()
                + " redeclared", e, name);
        }
    }
    ;

//
// Assert stuff
//

assertBlock[Environment env, CellImpl cell]
    : asserts:ASSERT
    {
        ASTWithInfo parsedAsserts;
        try {
            parsedAsserts =
                parseDumbBlockAs(asserts, CastTwoUtil.ASSERT_PARSER);
        } catch (TokenStreamException e) {
            throw semanticWrapperException("error parsing assert block", e, asserts);
        }
        assertStatements(parsedAsserts, env, cell);
    }
    ;

// Takes the tree returned by CastAssertParser.goal()
assertStatements[Environment env, CellImpl cell]
    : #( BODY_STATEMENT_LIST ( assertStatement[env, cell] )* )
    ;

assertStatement[Environment env, CellImpl cell]
    : assertPrsStatement[env, cell]
    | assertExclStatement[env, cell]
    | assertLoopStatement[env, cell]
    | assertIfStatement[env, cell]
    ;

// It produces the equivalent of the interesting half of "guard =>
// ERROR-" (~guard -> ERROR+), because ERROR is already being driven
// low by default.
assertPrsStatement[Environment env, CellImpl cell]
    {
        BooleanExpressionInterface guard;
    }
    : guard=errorAST:prsExpression[env, false]
    {
        final HierName errorName = HierName.makeHierName("ERROR!");
        final HierName railName = HierName.makeHierName("GND!");
        final ProductionRule pr =
            new ProductionRule(guard.negated(), errorName, railName,
                               ProductionRule.UP, false, false, false,
                               false, 100, false);
        cell.getAssertedProductionRuleSet().addProductionRule(pr);
      }
    ;

assertExclStatement[Environment env, CellImpl cell]
    { int hiLo; List l; ASTWithInfo errorAST; }
    : ( #( hi:EXCLHI l=assertExclNodes[env] { hiLo = ExclusiveNodeSet.HI; errorAST=hi; } )
      | #( lo:EXCLLO l=assertExclNodes[env] { hiLo = ExclusiveNodeSet.LO; errorAST=lo; } )
      | #( cc:EXCLCC l=assertExclNodes[env] { hiLo = ExclusiveNodeSet.CC; errorAST=cc; } )
      | #( nocc:NOCC l=assertExclNodes[env] { hiLo = ExclusiveNodeSet.NOCC; errorAST=nocc; } )
      )
      {
          cell.getLocalExclusiveNodeSets().addExclusiveNodeSet(new ExclusiveNodeSet(hiLo, l));
          // Generate all the asserted production rules which
          // correspond to the excl node set.

          // "assert { exclhi(a,b,c,d); }" corresponds to
          // "assert { ~(a&b | a&c | a&d | b&c ...); }"
          // "assert { excllo(w,x,y,z); }" corresponds to
          // "assert { ((a|b) & (a|c) & ...); }"

          int numNodes = l.size();
          if (numNodes == 1 || hiLo == ExclusiveNodeSet.CC
                            || hiLo == ExclusiveNodeSet.NOCC) {
              // One node is automatically exclhi and excllo with
              // itself.  It can be ignored.

              // Do not generate asserted rules for cap coupling.
          }
          else {
              int position = 0;
              // Will contain BooleanExpressionInterfaces
              final List guards = new ArrayList(numNodes*(numNodes-1)/2);
              for (int i=0; i<numNodes; i++) {
                  for (int j=i+1; j<numNodes; j++) {
                      guards.add(position, makeAssertPair(env, hiLo,
                                                          (HierName)l.get(i),
                                                          (HierName)l.get(j),
                                                          errorAST));
                      position++;
                  }
              }

              cell.getAssertedProductionRuleSet().addProductionRule(makeAssertRule(hiLo, guards));
          }
      }
    ;

// returns a List of HierNames.  assertExclStatement[] assumes it
// contains at least one element
assertExclNodes[Environment env] returns [ArrayList l]
    { Value v; l = new ArrayList(); }
    : ( v=exp:expr[env, false]
        {
            if (v instanceof NodeValue)
                l.add(v.getInstanceName());
            else if (v instanceof ArrayValue) {
                final ArrayValue av = (ArrayValue) v;
                final HierName arrayName = av.getInstanceName();
                final SubscriptSpecInterface s = av.getSpec();
                final int nElems = s.getNumElements();
                final String arraySuffix = arrayName.getSuffixString();
                
                for (int i = 0; i < nElems; ++i) {
                    // this duplicates code in ArrayValue.accessArray() and in type[]
                    final HierName nodeName;
                    nodeName = HierName.makeSiblingName(arrayName,
                                arraySuffix +
                                DenseSubscriptSpec.idxToString(s.indexOf(i)));
                    l.add(nodeName);
                }
            }
            else
                throw semanticWrapperException("Expected array or node type "
                    + ", not " + v, new Exception(), exp);
        }
      )+
    ;

// TODO: refactor with other loops and ifs
assertIfStatement[Environment env, CellImpl cell]
    { Value v; }
    : #( IF v=expr:expression[env, false] assertList:BODY_STATEMENT_LIST )
    {
        final BoolValue bv = ifGuard(v, expr);

        try {
            if (bv.getValue())
                assertStatements(assertList, env, cell);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("If guard uninitialized.",
                e, expr);
        }
    }
    ;

// TODO: refactor with other loops and ifs
assertLoopStatement[Environment env, CellImpl cell]
    { Range r; }
    : #( LOOP id:IDENT r=range[env, true] assertList:BODY_STATEMENT_LIST )
    {
        final Symbol sym = generateSymbol(id.getText());
        final Range.Iterator ri = r.iterator();
        while (ri.hasNext()) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));
            assertStatements(assertList, loopEnv, cell);
        }
    }
    ;


//
// Alias stuff
//

aliasBlock[Environment env, CellImpl cell, LocalEnvironment aliasesEnv]
    : aliases:ALIAS
    {
        ASTWithInfo parsedAliases;
        try {
            parsedAliases =
                parseDumbBlockAs(aliases, CastTwoUtil.ALIASES_PARSER);
        } catch (TokenStreamException e) {
            throw semanticWrapperException("error parsing aliases block", e,
                                           aliases);
        }
        aliasStatements(parsedAliases, env, cell, aliasesEnv);
    }
    ;

aliasStatements[Environment env, CellImpl chan, LocalEnvironment aliasesEnv]
    : #( BODY_STATEMENT_LIST ( aliasStatement[env, chan, aliasesEnv] )* )
    ;

aliasStatement[Environment env, CellImpl chan, LocalEnvironment aliasesEnv]
    : variableDeclarationStatement[env, false, chan, null, aliasesEnv, false]
    | aliasIfStatement[env, chan, aliasesEnv]
    | aliasLoopStatement[env, chan, aliasesEnv]
    ;

// TODO: refactor with other loops and ifs
aliasIfStatement[Environment env, CellImpl cell, LocalEnvironment aliasesEnv]
    { Value v; }
    : #( IF v=expr:expression[env, false] aliasList:BODY_STATEMENT_LIST )
    {
        final BoolValue bv = ifGuard(v, expr);

        try {
            if (bv.getValue())
                aliasStatements(aliasList, env, cell, aliasesEnv);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("If guard uninitialized.",
                e, expr);
        }
    }
    ;

// TODO: refactor with other loops and ifs
aliasLoopStatement[Environment env, CellImpl cell, LocalEnvironment aliasesEnv]
    { Range r; }
    : #( LOOP id:IDENT r=range[env, true] aliasList:BODY_STATEMENT_LIST )
    {
        final Symbol sym = generateSymbol(id.getText());
        final Range.Iterator ri = r.iterator();
        while (ri.hasNext()) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));
            aliasStatements(aliasList, loopEnv, cell, aliasesEnv);
        }
    }
    ;

// all declarations occur in the same environment.  Module name is supplied
// so the defined cells know their own fully-qualified names.
typeDeclaration[Environment env, boolean isEnv]
    { boolean isAttributeCell = false; }
    : #( TYPE_DEFINITION id:IDENT
                         ( m:FORMALS ( p:FORMALS ( implied:FORMALS )? )? )?
                         ( ATTRIBUTES { isAttributeCell = true; }
                           ( inheritanceList1:INHERITANCE_LIST )?
                         | ( inheritanceList2:INHERITANCE_LIST )?
                           ( refinement:REFINEMENT )? )
                         b:BLOCK )
    {
        final ASTWithInfo inheritanceList =
            isAttributeCell ? inheritanceList1 : inheritanceList2;
        // set refinement parent to default
        if (! isAttributeCell && refinement == null &&
            ! id.getText().equals(refinementRootCellName) &&
            ! id.getText().equals(refinementNullCellName)) {
            if (!isEnv && env.contains(Symbol.create(refinementRootCellName))) {
                refinement = fakeRefinementAST(refinementRootCellName, id);
            } else if (env.contains(Symbol.create(refinementNullCellName))) {
                refinement = fakeRefinementAST(refinementNullCellName, id);
            } else {
                // The default refinement parents "CELL" and "NULL" were not
                // imported
                // NB: This exception is thrown when the file is parsed, not
                // when the cell is instantiated.  Is that a problem? --jmr
                // Eventually make NULL built-in so it will always be found
                if (isEnv) {
                    throw semanticWrapperException("No cell named \"NULL\" " +
                        "found, required as the default refinement parent " +
                        "of environment cells.  Did you forget to import or " +
                        "define \"NULL\"?",
                        new Exception(), id);
                } else {
                    throw semanticWrapperException("No cell named \"CELL\" " +
                        "or \"NULL\" found.  Did you forget to import or " +
                        "define \"CELL\" and \"NULL\"?",
                        new Exception(), id);
                }
            }
        }
        try {
            final Symbol cellTypeSymbol = Symbol.create(id.getText());
            final int structureType =
                isAttributeCell ? UserDefinedValue.ATTRIBUTES_CELL 
                                : UserDefinedValue.CELL;
            env.bind(cellTypeSymbol,
                     new UserDefinedValue(env, cellTypeSymbol, m,
                                          (isEnv ? null : p),
                                          implied,
                                          (isEnv ? p : null),
                                          inheritanceList,
                                          refinement, b, moduleName,
                                          structureType));
        } catch (SymbolRedeclaredException e) {
            throw semanticWrapperException("type " + id.getText()
                + " redeclared", e, id);
        }
    }
    ;

// Defines the metaparameter names in env and assigns them the values in tupleVal
metaParamList[Environment env, TupleValue tupleVal]
returns [List metaParams]
throws InvalidOperationException // If assignment of metas doesn't work
    { int i = 0; metaParams = new ArrayList(tupleVal.getSize()); }
    :
    ( #( f:FORMALS (
        {
            if (i >= tupleVal.getSize()) {
                int expected = 0;
                AST child = f.getFirstChild();
                while (child != null) {
                    expected++;
                    child = child.getNextSibling();
                }
                throw new InvalidOperationException("too few metaparameters: got " + tupleVal.getSize() + " and expected " + expected);
            }
        }
        metaParam[env, metaParams, tupleVal.accessTuple(i++)] )*
    )
    | /* nothing */
    )
    {
        if (i != tupleVal.getSize())
            throw new InvalidOperationException("too many metaparameters: got " + tupleVal.getSize() + " and expected " + i);
    }
    ;

// XXX: enforce that only int,bool,float can be meta-params
metaParam[Environment env, List metaParams, Value tv]
throws InvalidOperationException // If assignment of metas doesn't work
    { Pair p; }
    : #( VAR_DECL id:IDENT
         p=type[env, null, null, HierName.makeHierName(id.getText()), false,
                false, false, id, META_PARAM] )
    {
        try {
            final Value v = (Value) p.getFirst();
            final Symbol sym = generateSymbol(id.getText());
            env.bind(sym, v);
            // cell is null because it shouldn't be used by assign
            v.assign(tv, null);

            // save MetaParamDefinition for later addition to cell
            metaParams.add(new MetaParamDefinition(
                id.getText(),
                ((MetaParamValueInterface) tv).toMetaParam()));
        } catch (SymbolRedeclaredException e) {
            throw semanticWrapperException("meta param " + id.getText()
                + " redeclared ", e, id);
        }
    }
    ;

// modifies portList to contain a list of the variable names
// declared in the port parameter list,
// also modifies env by binding an uninitialized value to
// all the variables declared in the port list.
// env: the environment for the subcell
// impliedPortParentNodes: the neames of the nodes in the parent to
//   which the implied ports should be connected, null if the port
//   list is not an implied port list
// reverseDirection: whether to interpret - as output and + as input, which is
// the opposite of the usual interpretation; useful for constructing the port
// list of a environment cell
// Implied ports can only be nodes.
portParamList[Environment env, CellImpl cell, ArrayList portList,
              ArrayList impliedPortParentNodes, boolean reverseDirection]
    : #( FORMALS ( portParam[env, cell, portList, impliedPortParentNodes,
                             reverseDirection] )* )
    ;

envExtraPortIdent[Environment env, StringBuffer buf]
    : #( id:IDENT {
          buf.append(id.getText());
      })
    | #( FIELD_ACCESS envExtraPortIdent[env, buf] fi:FIELD_IDENT ) {
          buf.append('.');
          buf.append(fi.getText());
      }
    | #( ARRAY_ACCESS envExtraPortIdent[env, buf] { buf.append('['); }
                      subtypesArraySelector[env, buf] { buf.append(']'); } )
    ;

portParam[Environment env, CellImpl cell, ArrayList portList,
          ArrayList impliedPortParentNodes, boolean reverseDirection]
    { Pair p; boolean in = false , out = false;
      StringBuffer buf = new StringBuffer();
    }
    : #( VAR_DECL 
         id:IDENT
         ( PLUS {if (reverseDirection) in=true; else out=true;} )?
         ( MINUS {if (reverseDirection) out=true; else in=true;} )?
         p=type[env, cell, null, HierName.makeHierName(id.getText()), false,
                false, false, id, PORT_PARAM]
         // :ident will only be there in the case of implied ports
         ( COLON id2:IDENT )?
         ( envExtraPortIdent[env, buf] )?
      )
    {
        final boolean impliedP = impliedPortParentNodes != null;
        final boolean envExtraP = buf.length() > 0;
        assert !impliedP || !envExtraP :
               "Implied ports and env extra ports are mutually exclusive";
        try {
            final Value v = (Value) p.getFirst();
            if (impliedP && ! (v instanceof NodeValue))
                throw semanticWrapperException("implied ports must be nodes", new Exception(), id);
            final Symbol sym = generateSymbol(id.getText());
            env.bind(sym, v);
            if (portList != null) portList.add(sym);
            if (impliedP) {
                final String parentNode =
                    id2 != null ? id2.getText() : id.getText();
                impliedPortParentNodes.add(parentNode);
                cell.addImpliedPortMapping(id.getText(), parentNode);
            }
            if (envExtraP) {
                cell.addEnvExtraPortMapping(id.getText(), buf.toString());
            }
        } catch (SymbolRedeclaredException e) {
            throw semanticWrapperException("port param " + id.getText()
                + " redeclared", e, id);
        }

        // add port definition
        if (! (p.getSecond() instanceof PortTypeInterface)) {
            throw semanticWrapperException("invalid type for port: " + p.getSecond() + ".  Ports should be nodes, channels, or arrays of same", new Exception(), id);
        }
        final PortTypeInterface portType = (PortTypeInterface) p.getSecond();
        final int dir = in ? (out ? PortDefinition.INOUT : PortDefinition.IN)
                           : (out ? PortDefinition.OUT : PortDefinition.NONE);
        cell.addPortDefinition(new PortDefinition(id.getText(), portType, dir));
    }
    ;

// refactor: remove duplication with prsLoopStatement
loopStatement[Environment env, CellImpl cell, CellInterface envContainer,
              BlockEnvironment paramEnv, Environment prsEnv,
              Environment subcellEnv, Environment subtypesEnv]
    { Range r; }
    : #( LOOP id:IDENT r=range[env, true] slist:BODY_STATEMENT_LIST )
    {
        final Symbol sym = generateSymbol(id.getText());
        final Range.Iterator ri = r.iterator();
        while (ri.hasNext()) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));
            bodyStatementList(slist, loopEnv, cell, envContainer, paramEnv,
                              prsEnv, subcellEnv, subtypesEnv);
        }
    }
    ;

// refactor: remove duplication with prsIfStatement
ifStatement[Environment env, CellImpl cell, CellInterface envContainer,
            BlockEnvironment paramEnv, Environment prsEnv,
            Environment subcellEnv, Environment subtypesEnv]
    { Value v; }
    : #( IF v=expr:expression[env, false] slist:BODY_STATEMENT_LIST )
    {
        final BoolValue bv = ifGuard(v, expr);

        try {
            if (bv.getValue())
                bodyStatementList(slist, env, cell, envContainer, paramEnv,
                                  prsEnv, subcellEnv, subtypesEnv);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("If guard uninitialized.",
                e, expr);
        }
    }
    ;

// fix me up later?
// ( = expr+ expr )
assignmentStatement[Environment env, CellImpl cell]
    { final ArrayList l = new ArrayList(), eList = new ArrayList();
      Value v0, vv; }
    : #( ASSIGN v0=e0:expr[env, false]
         ( vv=ee:expression[env, false] { l.add(vv); eList.add(ee);} )* )
    { 
        // xxx use the cell

        // change the * to a +, and this goes away!
        Debug.assertTrue(l.size() > 0);

        for (int i = l.size() - 1; i > 0; --i)
            assign((Value) l.get(i - 1), (Value) l.get(i), cell,
                   (ASTWithInfo) eList.get(i - 1),
                   "assignment failed");

        assign(v0, (Value) l.get(0), cell, e0, "assignment failed");
    }
    ;

// returns a tuple, this will be converted to an array if needed
// anonymousAllowedP: true if anonymous variables (_) are allowed
//   as expressions in the expressionList
expressionList[Environment env,
               boolean anonymousAllowedP,
               boolean deepP] returns [TupleValue v]
    { final List l = new ArrayList(); Value ve; Range r = null; v = null; }
    : #( EXPRESSION_LIST
         (
           ve=expression[env, anonymousAllowedP, deepP] { l.add(ve); }
         | ( id:IDENT r = range[env, true] expr:EXPRESSION_LIST ) {
              final Symbol sym = generateSymbol(id.getText());
              final Range.Iterator ri = r.iterator();
              while (ri.hasNext()) {
                  final int i = ri.next();
                  final Environment loopEnv =
                      new LoopEnvironment(env, sym, IntValue.valueOf(i));
                  final TupleValue tv =
                      expressionList(expr, loopEnv, anonymousAllowedP, deepP);
                  for (int j = 0; j < tv.getSize(); ++j) {
                      try {
                          l.add(tv.accessTuple(j));
                      } catch (InvalidOperationException e) {
                          throw semanticWrapperException(e, id);
                      }
                  }
              }
           }
         )*
       )
    { v = new TupleValue((Value []) l.toArray(new Value[0])); }
    ;

// anonymousAllowedP: true if anonymous variables (_) are allowed
//   as expressions
expression[Environment env, boolean anonymousAllowedP,
           boolean deepP] returns [Value v]
    { v = null; }
    : #( EXPRESSION v=expr[env, anonymousAllowedP, deepP] )
    ;

// Used by CspCallback to verify channel types
arrayAccessExpr[Environment env] returns [Pair p]
    { Value v; p = null; }
    : #( o18:ARRAY_ACCESS v=expr[env, false]
         p=arraySelector[env, null, false] )
      { p = new Pair(v, p.getFirst()); }
    ;

// anonymousAllowedP: true if anonymous variables (_) are allowed
//   as expressions
// deepP: true if field access restrictions should not be observed;
//   use with caution
expr[Environment env, boolean anonymousAllowedP, boolean deepP]
returns [Value v]
    // logical
    { Value v1, v2; v = null; ASTWithInfo t = null; 
      Range r; }
    : #( o01:OR v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o01; v = v1.or(v2); }
    | #( o02:AND v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o02; v = v1.and(v2); }
    | #( o03:NOT v1=expr[env, false, deepP] )
      { t = o03; v = v1.not(); }
    | #( oXOR:XOR v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = oXOR; v = v1.xor(v2); }
    // arithmetic
    | #( oExp:EXP v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = oExp; v = v1.pow(v2); }
    | #( o04:PLUS v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o04; v = v1.add(v2); }
    | #( o05:MINUS v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o05; v = v1.subtract(v2); }
    | #( o06:TIMES v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o06; v = v1.multiply(v2); }
    | #( o07:DIV v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o07; v = v1.divide(v2); }
    | #( o08:MOD v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o08; v = v1.mod(v2); }
    | #( o09:UNARY_PLUS v1=expr[env, false, deepP] )
      { t = o09; v = v1.unaryPlus(); }
    | #( o10:UNARY_MINUS v1=expr[env, false, deepP] )
      { t = o10; v = v1.negate(); }
    // relational
    | #( o11:LT v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o11; v = v1.lt(v2);        }
    | #( o12:LE v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o12; v = v1.le(v2);        }
    | #( o13:EQ v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o13; v = v1.eq(v2);        }
    | #( o14:NE v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o14; v = v1.ne(v2);        }
    | #( o15:GE v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o15; v = v1.ge(v2);        }
    | #( o16:GT v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o16; v = v1.gt(v2);        }
    // selection
    | #( o17:FIELD_ACCESS v1=expr[env, false, deepP] fi:FIELD_IDENT )
      {
          t = o17;
          if (! (v1 instanceof FieldedValueInterface))
              throw semanticWrapperException("can't access fields of value " + v, new Exception(), t);
          v = ((FieldedValueInterface) v1).accessField(
                  Symbol.create(fi.getText()),
                  deepP ? FieldedValueInterface.ALL_PERMISSION
                        : FieldedValueInterface.INSTANCE_PERMISSION);
      }
    | { Pair p; }
      #( o18:ARRAY_ACCESS v1=expr[env, false, deepP]
         p=arraySelector[env, null, false] )
      { t = o18; v = ArrayValue.valueOf(v1).accessArray(
          (SubscriptSpecInterface) p.getFirst()); }
    // primary
    | { v2 = null; }
      id:IDENT ( v2 = expressionList[env, false] )?
      {
          t = id;
          final String s = id.getText();

          if (v2 == null) {
              if (s.equals("_")) {
                  if (anonymousAllowedP)
                      v = new AnonymousValue();
                  else {
                      final SemanticException se
                          = new SemanticException("_ not allowed here",
                              id.getFilename(), id.getLine(), id.getColumn());
                      se.column = id.getColumn();
                      throw se;
                  }
              } else
                  v = lookup(env, Symbol.create(s), id);
          } else {
              v = Functions.invokeFunction(s, (TupleValue) v2);
          }
      }
    | n:NUM_INT   { t = n; v = IntValue.valueOf(n.getText()); }
    | h:NUM_HEX   { t = h; v = IntValue.hexValueOf(h.getText()); }
    | f:NUM_REAL  { t = f; v = FloatValue.valueOf(f.getText()); }
    | o19:TRUE        { t = o19; v = BoolValue.valueOf(true); }
    | o20:FALSE       { t = o20; v = BoolValue.valueOf(false); }
    | #( o21:ARRAY v1=expressionList[env, anonymousAllowedP] )
      { t = o21; v = ArrayValue.fromTuple(v1); }
    // prs stuff
    /// XXX!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // uhm!!! should be looking symbols up in symbol table!!!!!!!
    | #( o22:PRS_AND v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o22; v = PRSExpressionValue.valueOf(v1).and(v2); }
    | #( o23:PRS_OR v1=expr[env, false, deepP] v2=expr[env, false, deepP] )
      { t = o23; v = PRSExpressionValue.valueOf(v1).or(v2); }
    | #( o24:PRS_NOT v1=expr[env, false, deepP] )
      { t = o24; v = PRSExpressionValue.valueOf(v1).not(); }
    // merge these two into something like:
    // LOOPED_OP ( AND | OR ) IDENT range EXPRESSION
    | #( o25:LOOP_PRS_OR id2:IDENT r=range[env, true] e2:PRS_EXPRESSION )
      { v = new PRSExpressionValue(
          new OrBooleanExpression(true,
              loopedExpression(id2, r, env, e2))); }
    | #( o26:LOOP_PRS_AND id3:IDENT r=range[env, true] e3:PRS_EXPRESSION )
      { v = new PRSExpressionValue(
          new AndBooleanExpression(true,
              loopedExpression(id3, r, env, e3))); }
    | #( o27:LOOP_TIMES id4:IDENT r=range[env, true] e4:EXPRESSION )
      { final ASTWithInfo ast = e4;
        t = o27;
        v = loopedExpression(id4, r, env, e4, new CombineValue() {
          public Value combine(final Value a, final Value b)
          throws InvalidOperationException {
              return a.multiply(b);
          }
          public Value identity() throws InvalidOperationException {
              return IntValue.valueOf(1);
          }
        });
      }
    | #( o28:LOOP_PLUS id5:IDENT r=range[env, true] e5:EXPRESSION )
      { final ASTWithInfo ast = e5;
        t = o28;
        v = loopedExpression(id5, r, env, e5, new CombineValue() {
          public Value combine(final Value a, final Value b)
          throws InvalidOperationException {
              return a.add(b);
          }
          public Value identity() throws InvalidOperationException {
              return IntValue.valueOf(0);
          }
        });
      }
    | #( o28x:LOOP_XOR id6:IDENT r=range[env, true] e6:EXPRESSION )
      { final ASTWithInfo ast = e6;
        t = o28x;
        v = loopedExpression(id6, r, env, e6, new CombineValue() {
          public Value combine(final Value a, final Value b)
          throws InvalidOperationException {
              return a.xor(b);
          }
          public Value identity() throws InvalidOperationException {
              return IntValue.valueOf(0);
          }
        });
      }
    | #( o28o:LOOP_OR id7:IDENT r=range[env, true] e7:EXPRESSION )
      { final ASTWithInfo ast = e7;
        t = o28o;
        v = loopedExpression(id7, r, env, e7, new CombineValue() {
          public Value combine(final Value a, final Value b)
          throws InvalidOperationException {
              return a.or(b);
          }
          public Value identity() throws InvalidOperationException {
              return IntValue.valueOf(0);
          }
        });
      }
    | #( o28a:LOOP_AND id8:IDENT r=range[env, true] e8:EXPRESSION )
      { final ASTWithInfo ast = e8;
        t = o28a;
        v = loopedExpression(id8, r, env, e8, new CombineValue() {
          public Value combine(final Value a, final Value b)
          throws InvalidOperationException {
              return a.and(b);
          }
          public Value identity() throws InvalidOperationException {
              return IntValue.valueOf(-1);
          }
        });
      }
    // cast expressions
    | #( o29:BOOL v1 = expr[env, false, deepP] )
      { t = o29; v = BoolValue.castFrom(v1); }
    | #( o30:INT v1 = expr[env, false, deepP] )
      { t = o30; v = IntValue.castFrom(v1); }
    | #( o31:FLOAT v1 = expr[env, false, deepP] )
      { t = o31; v = FloatValue.castFrom(v1); }
    | #( o32:NODE v1 = expr[env, false, deepP] )
      { t = o32; v = NodeValue.castFrom(v1); }
    // alint scenario directives
    | #( o33:ALINT_FANIN v = alintFanin[env] )
      { t = o33; }
    ;
    exception
    catch [InvalidOperationException e] {
        throw semanticWrapperException(e, t);
    }
    catch [ArithmeticException e] {
        throw semanticWrapperException(e, t);
    }

// loopP indicates how a singleton range should be interpreted
// true: it will be interpreted as 0..n-1
// false: it will be interpreted as n..n
range[Environment env, boolean loopP] returns [Range r]
    { Value v1, v2 = null; r = null; }
    : #( RANGE v1=e1:expression[env, false]
             ( v2=e2:expression[env, false] )? )
    {
        final int i1 = getInt(v1, e1);

        if (v2 == null) {
            if (loopP)
                r = new Range(0, i1 - 1);
            else
                r = new Range(i1, i1);
        } else {
            final int i2 = getInt(v2, e2);

            r = new Range(i1, i2);
        }
    }
    ;

// returns Pair of SubscriptSpecInterface and ArrayType or ArrayMetaParam
// metaParamP only matters if baseType != null
arraySelector[Environment env, ParamTypeInterface baseType, boolean metaParamP]
returns [Pair p]
    { ArrayList l = new ArrayList(); p = null; }
    : #( arrayExpr:ARRAY_SUB
         ( {Range r;} r=rangeExpr:range[env, false]
           { if (r.getMin() > r.getMax())
                 throw semanticWrapperException
                     ("Array subscript minimum value of " + r.getMin() +
                      " is greater than maximum of " + r.getMax(),
                      new Exception(), arrayExpr);
             l.add(r);
           }
         )*
       )
    {
        for (int i = l.size() - 1; i >= 0; --i) {
            final Range r = (Range) l.get(i);
            if (baseType != null) {
                if (metaParamP) {
                    baseType = null;
                    /* baseType = new ArrayMetaParam( */
                        /* (MetaParamTypeInterface) baseType, */
                        /* r.getMin(), r.getMax()); */
                } else {
                    baseType = new ArrayType(
                        (PortTypeInterface) baseType,
                        r.getMin(), r.getMax());
                }
            }
        }

        p = new Pair(
            new DenseSubscriptSpec((Range []) l.toArray(new Range[0])),
            baseType);
    }
    ;


//
// PRS stuff
//

prsBlock[Environment env, CellImpl cell]
    { boolean isFragment = false; }
    : #( prs:PRS ( FRAGMENT { isFragment=true; } )? )
    {
        final ASTWithInfo prsAST;
        try {
            prsAST = parseDumbBlockAs(prs, CastTwoUtil.PRS_PARSER);
        } catch (TokenStreamException e) {
            throw semanticWrapperException("error parsing prs block", e, prs);
        }
        prsStatements(prsAST, env, cell);

        if (isFragment)
            cell.setHasFragmentPrsBlock();
        else
            cell.setHasCompletePrsBlock();
       
    }
    ;

prsStatements[Environment env, CellImpl cell]
    : #( BODY_STATEMENT_LIST ( prsStatement[env, cell] )* )
    ;

prsStatement[Environment env, CellImpl cell]
    : prsIfStatement[env, cell]
    | prsLoopStatement[env, cell]
    | prsAction[env, cell, cell.getProductionRuleSet(), false]
    | directiveBlock[env,
                     cell.getBlockInterface()
                         .iterator(BlockInterface.PRS)
                         .next(),
                     BlockInterface.PRS]
    | variableDeclarationStatement[env, false, cell, null, null, true]
    | assignmentStatement[env, cell]
    | assertBlock[env, cell]
    ;

// todo: refactor: remove duplication with ifStatement
prsIfStatement[Environment env, CellImpl cell]
    { Value v; }
    : #( IF v=expr:expression[env, false] prslist:BODY_STATEMENT_LIST )
    {
        final BoolValue bv = ifGuard(v, expr);

        try {
            if (bv.getValue())
                prsStatements(prslist, env, cell);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("If guard uninitialized.",
                e, expr);
        }
    }
    ;

// todo: refactor, remove duplication with loopStatement
prsLoopStatement[Environment env, CellImpl cell]
    { Range r; }
    : #( LOOP id:IDENT r=range[env, true] prslist:BODY_STATEMENT_LIST )
    {
        final Symbol sym = generateSymbol(id.getText());
        final Range.Iterator ri = r.iterator();
        while (ri.hasNext()) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));
            prsStatements(prslist, loopEnv, cell);
        }
    }
    ;

prsAction[Environment env, CellImpl cell, ProductionRuleSet prs, boolean deepP]
    { ProductionRule pr1, pr2 = null; }
    // merge these
    : ( #( ARROW pr1=prsActionBody[env, cell, deepP] )
      | #( celem:CELEM_ARROW pr1=pab:prsActionBody[env, cell, deepP] )
        { pr2 =
            pr1.cElementComplement(
                oppositePowerSupply(env, pr1.getPowerSupply(), celem)); }
      | #( comb:COMB_ARROW pr1=prsActionBody[env, cell, deepP] )
        { pr2 =
            pr1.combinationalComplement(
                oppositePowerSupply(env, pr1.getPowerSupply(), comb)); }
      ) { prs.addProductionRule(pr1);
          if (pr2 != null)
              prs.addProductionRule(pr2);
        }
    ;

prsActionBody[Environment env, CellImpl cell, boolean deepP]
returns [ProductionRule pr]
    {
        Value av, rhsv, d1, d2;
        BooleanExpressionInterface be;
        boolean isochronicP = false, unstabP = false, timedP = false;
        boolean metastableP = false;
        int afterTimeSteps = -1; // negative means unspecified -- AML
        int dir;
        ASTWithInfo exp;
        boolean afterPs = false;
        float fastDelay = -1;
        float slowDelay = -1;
    }
    : ( ISOCHRONIC {isochronicP = true;} )?
      ( UNSTAB {unstabP = true;} | METASTABLE { metastableP = true; } )?
      ( TIMED { timedP = true; fastDelay = 1; slowDelay = 1; }
        ( d1=de1:expression[env, false]
          { slowDelay = fastDelay = (float) getFloat(d1, de1); }
          ( d2=de2:expression[env, false]
            { slowDelay = (float) getFloat(d2, de2); }
          )?
        )?
      )?
      ( ( AFTER | AFTER_PS { afterPs = true; } ) av=e:expression[env, false]
        { afterTimeSteps = getInt(av, e); } )?
       be=prsExpression[env, deepP] rhsv=e2:expr[env, false, deepP]
       ( p:PLUS  { dir=ProductionRule.UP;   exp=p; } 
       | m:MINUS { dir=ProductionRule.DOWN; exp=m; } )
    {
        final NodeValue nv;
        try {
            nv = (NodeValue) rhsv;
        } catch (ClassCastException exn) {
            throw semanticWrapperException("Non-node type for " +
                "target of production rule", exn, e2);
        }
        final HierName rhsNode = nv.getInstanceName();

        final String rail = dir == ProductionRule.UP ? "Vdd" : "GND";
        final HierName railName;
        try {
            final Symbol s = Symbol.create(rail);
            final Value v = lookup(env, s, exp);
            railName = NodeValue.valueOf(v).getInstanceName();
  
            // cell is null if we are parsing a rule in a directive block
            if (cell != null) cell.addNode(railName);
        } catch (InvalidOperationException exn) {
            throw semanticWrapperException("Non-node type for " + rail +
                ", implied by production rule",
                exn, exp);
        }

        pr = new ProductionRule(be, rhsNode, railName, dir, isochronicP,
                                unstabP, metastableP, timedP, afterTimeSteps,
                                afterPs, fastDelay, slowDelay);
    }
    ;

// this returns a BooleanExpressionInterface, suitable for use as the
// lhs of a production rule or an assert, and nowhere else
prsExpression[Environment env, boolean deepP]
returns [BooleanExpressionInterface be]
    { Value prsv; }
    : #( PRS_EXPRESSION prsv=exp:expr[env, false, deepP] )
    { be = toPRSExpressionValue(prsv, exp).getBooleanExpression(); }
    ;

//
// subcells stuff
//

subcellsBlock[Environment env, CellImpl cell]
    : #( subcells:SUBCELLS ( inline:INLINE | fragment:FRAGMENT )? )
    {
        try {
            parseDumbBlockAs(subcells, CastTwoUtil.SUBCELLS_PARSER,
                             new SubcellsParserCallback(env, cell));
        } catch (TokenStreamException e) {
            throw semanticWrapperException("error parsing subcells block", e, subcells);
        }
        if (fragment == null) cell.setHasCompleteSubcellsBlock();
        else cell.setHasFragmentSubcellsBlock();
        cell.setAutoInline(inline!=null && opt.processInline(cell));
    }
    ;

subcellsStatementsList[Environment env, CellImpl cell]
    : #( BODY_STATEMENT_LIST subcellsStatements[env, cell] )
    ;

subcellsStatements[Environment env, CellImpl cell]
    : ( subcellsStatement[env, cell] )*
    ;

subcellsStatement[Environment env, CellImpl cell]
    : variableDeclarationStatement[env, false, cell, null, null, false]
    | assignmentStatement[env, cell]
    | subcellsIfStatement[env, cell]
    | subcellsLoopStatement[env, cell]
    | assertBlock[env, cell]
    | directiveBlock[env,
                     cell.getBlockInterface()
                         .iterator(BlockInterface.SUBCELL)
                         .next(),
                     BlockInterface.SUBCELL]
    ;

// TODO: refactor with other loops and ifs
subcellsIfStatement[Environment env, CellImpl cell]
    { Value v; }
    : #( IF v=expr:expression[env, false] subcellsList:BODY_STATEMENT_LIST )
    {
        final BoolValue bv = ifGuard(v, expr);

        try {
            if (bv.getValue())
                subcellsStatementsList(subcellsList, env, cell);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("If guard uninitialized.",
                e, expr);
        }
    }
    ;

// TODO: refactor with other loops and ifs
subcellsLoopStatement[Environment env, CellImpl cell]
    { Range r; }
    : #( LOOP id:IDENT r=range[env, true] subcellsList:BODY_STATEMENT_LIST )
    {
        final Symbol sym = generateSymbol(id.getText());
        final Range.Iterator ri = r.iterator();
        while (ri.hasNext()) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));
            subcellsStatementsList(subcellsList, loopEnv, cell);
        }
    }
    ;

//
// subtypes stuff
//

// Doesn't change env
subtypesBlock[Environment env, CellImpl cell]
    : subtypes:SUBTYPES
    {
        try {
            parseDumbBlockAs(subtypes, CastTwoUtil.SUBTYPES_PARSER,
                             new SubtypesParserCallback(env,
                                                        new IdentityHashMap(),
                                                        cell));
        } catch (TokenStreamException e) {
            throw semanticWrapperException("error parsing subtypes block", e, subtypes);
        }
        cell.setHasSubtypesBlock();
    }
    ;

subtypesStatementsList[Environment env, Map copied, CellImpl cell]
    : #( BODY_STATEMENT_LIST subtypesStatements[env, copied, cell] )
    ;

subtypesStatements[Environment env, Map copied, CellImpl cell]
    : ( subtypesStatement[env, copied, cell] )*
    ;

subtypesStatement[Environment env, Map copied, CellImpl cell]
    : subtypesRefinement[env, copied, cell]
    | subtypesIfStatement[env, copied, cell]
    | subtypesLoopStatement[env, copied, cell]
    | directiveBlock[env,
                     cell.getBlockInterface()
                         .iterator(BlockInterface.SUBCELL)
                         .next(),
                     BlockInterface.SUBCELL]
    ;

subtypesArraySelector[Environment env, StringBuffer buf]
    : #( ARRAY_SUB subtypesRange[env, buf]
         ( { buf.append(','); } subtypesRange[env, buf] )*
       )
    ;

subtypesRange[Environment env, StringBuffer buf]
    { Range r; }
    : r = range[env, false] { buf.append(r.getMin()); }
    ;

subtypesIdent[Environment env, Map copied, StringBuffer buf]
returns [Binder next]
    { Binder prev; next = null; }
    : #( id:IDENT {
          final String s = id.getText();
          buf.append(s);

          next = new SimpleBinder(env, Symbol.create(s));
      })
    | #( FIELD_ACCESS prev=subtypesIdent[env, copied, buf] fi:FIELD_IDENT ) {
          buf.append('.');
          buf.append(fi.getText());

          try {
              final Value v = prev == null ? null : prev.lookup();
              if (v instanceof InstanceValue) {
                  final boolean found = copied.containsKey(v);
                  final Pair processed =
                      ((InstanceValue) v).processSubtypes(found);
                  if (!found) {
                      copied.put(v, null);
                      prev.bind((Value) processed.getSecond());
                  }
                  next = new SimpleBinder((Environment) processed.getFirst(),
                                          Symbol.create(fi.getText()));
              }
          } catch (Exception e) {
              // e.printStackTrace();
          }
      }
    | #( ARRAY_ACCESS prev=subtypesIdent[env, copied, buf] { buf.append('['); }
                      sub:subtypesArraySelector[env, buf] { buf.append(']'); } )
      {

          try {
              if (prev != null) {
                  final Value v = prev.lookup();
                  final SubscriptSpecInterface spec = (SubscriptSpecInterface)
                      arraySelector(sub, env, null, false).getFirst();
                  if (v instanceof ArrayValue) {
                      final ArrayValue av;
                      if (copied.containsKey(v)) {
                          av = (ArrayValue) v;
                      } else {
                          av = (ArrayValue) v.duplicate();
                          copied.put(av, null);
                          prev.bind(av);
                      }
                      next = new ArrayBinder(av, spec);
                  }
              }
          } catch (Exception e) {
              // e.printStackTrace();
          }
      }
    ;

// Types need parent cells for creation, so these get throwaway parent
// cells.  The connections are taken care of by the subcell
// declaration in the refinement parent.
subtypesRefinement[Environment env, Map copied, CellImpl cell]
    {
        final StringBuffer buf = new StringBuffer();
        final Pair parentPair, childPair;
        final HierName subcellName;
        Pair arrayIndexPair = null;
        CellImpl throwawayCell = new CellImpl("fake", null,
                                              CellImpl.SYNTHETIC_CELL);
        Binder binder;
    }
    : #( v:VAR_DECL
         ( inline:INLINE )?
         binder=subtypesIdent[env, copied, buf]
         {
             // It's impossible to subtype anonymous cells, so it
             // doesn't need to pass through generateName().

             try {
                 subcellName = HierName.makeHierName(buf.toString(), '.');
             } catch (InvalidHierNameException e) {
                 throw new AssertionFailure
                     ("Can't make HierName from " + buf);
             }
         }
         parentPair=type[env, throwawayCell, null,
                         HierName.makeHierName("fake"),
                         false, false, false, null, LOCAL_VAR]
         childPair=type[env, throwawayCell, null,
                        HierName.makeHierName("fake"),
                        false, false, false, null, LOCAL_VAR]
      )
      {
          final CellInterface parent =
              ((InstanceValue) parentPair.getFirst()).getCell();
          final InstanceValue childInst = (InstanceValue) childPair.getFirst();

          final CellInterface child = childInst.getCell();
          try {
              final boolean inlineP =
                  (inline != null || child.isAutoInline()) &&
                  opt.processInline(parent);
              cell.addSubtype(subcellName, parent, child, inlineP,
                              opt.bug16459HackEnabled());
              if (inlineP) {
                  childInst.setInline(InstanceValue.INLINE_INLINE);
              }
          }
          catch (RefinementException e) {
              throw semanticWrapperException(e, v);
          }

          // Rely on methods in CellImpl to determine the correctness of the
          // subtypes block.  It might be better to do that earlier, perhaps
          // here.
          try {
              final Value curr = binder == null ? null : binder.lookup();
              if (curr != null) {
                  Value next = childInst;
                  Value[] vals;
                  if (curr instanceof ArrayValue) {
                      final ArrayValue av = (ArrayValue) curr;
                      final SubscriptSpecInterface spec = av.getSpec();
                      vals = new Value[spec.getNumElements()];
                      for (int i = 0; i < vals.length; ++i) {
                          final int[] idx = spec.indexOf(i);
                          final Value elem = av.accessArray(idx);
                          vals[i] =
                              childInst.duplicate()
                                       .newInstanceName(elem.getInstanceName());
                      }
                      next = new ArrayValue(vals, spec, av.getInstanceName(),
                                            av.isWideChannel());
                  } else {
                      next = childInst.newInstanceName(curr.getInstanceName());
                  }
                  if (!copied.containsKey(curr)) {
                      binder.bind(next);
                      copied.put(next, null);
                  }
              }
          } catch (Exception e) {
             // e.printStackTrace();
          }
      }
    ;

// TODO: refactor with other loops and ifs
subtypesIfStatement[Environment env, Map copied, CellImpl cell]
    { Value v; }
    : #( IF v=expr:expression[env, false] subtypeStmts:BODY_STATEMENT_LIST )
    {
        final BoolValue bv = ifGuard(v, expr);

        try {
            if (bv.getValue())
                subtypesStatementsList(subtypeStmts, env, copied, cell);
        } catch (InvalidOperationException e) {
            throw semanticWrapperException("If guard uninitialized.",
                e, expr);
        }
    }
    ;

// TODO: refactor with other loops and ifs
subtypesLoopStatement[Environment env, Map copied, CellImpl cell]
    { Range r; }
    : #( LOOP id:IDENT r=range[env, true] subtypeStmts:BODY_STATEMENT_LIST )
    {
        final Symbol sym = generateSymbol(id.getText());
        final Range.Iterator ri = r.iterator();
        while (ri.hasNext()) {
            final int i = ri.next();
            final Environment loopEnv
                = new LoopEnvironment(env, sym, IntValue.valueOf(i));
            subtypesStatementsList(subtypeStmts, loopEnv, copied, cell);
        }
    }
    ;

//
// csp stuff
//

// Doesn't change env
cspBlock[Environment env, CellImpl cell, CellInterface envContainer]
    : #( csp:CSP
         {
             // System.err.println("csp text: " + #csp.getText());
             try {
                 final CspLexer cspLexer =
                     new CspLexer(new StringReader(#csp.getText()));
                 cspLexer.setTokenObjectClass(TokenWithInfo.class.getName());
                 final TokenStreamSelector selector =
                     CastTwoUtil.getSelector(cspLexer, "csplexer",
                                             #csp.getLine(), #csp.getColumn(),
                                             #csp.getFilename());

                 final CspParser cspParser = new CspParser(selector);
                 cspParser.setSelector(selector);

                 cspParser.setFilename(#csp.getFilename());
                 final Pair p = cspParser.program();
                 final CSPProgram cspProgram = (CSPProgram) p.getFirst();
                 final TokenWithInfo dir = (TokenWithInfo) p.getSecond();
                 if (dir != null) {
                     parseDirectives(dir.getText(), dir.getLine(),
                                     dir.getColumn(), dir.getFilename(), env,
                                     BlockInterface.CSP,
                                     cell.getBlockInterface()
                                         .iterator(BlockInterface.CSP)
                                         .next());
                 }

                 cspProgram.setInitializerStatement
                     (makeCSPConstantInitializers(env));
                 if (envContainer != null) {
                     cspProgram.refineDeclarationsFrom(
                        envContainer.getCSPProgram());
                 }

                 cell.setCSPProgram(cspProgram);
             } catch (RecognitionException e) {
                 throw semanticWrapperException(e, csp);
             } catch (TokenStreamException e) {
                 throw semanticWrapperException(e, csp);
             } catch (InvalidOperationException e) {
                 throw semanticWrapperException(e, csp);
             }
         }
         ( cspPorts[env, cell] )?
       )
    ;

cspPorts[Environment env, CellImpl cell]
    : #( CSP_PORTS ( cspPort[env, cell] )+ )
    ;

cspPort[Environment env, CellImpl cell] 
    : id:IDENT 
      {
          try {
              cell.addCSPPort(id.getText());
          } catch (BadCSPPortException e) {
              throw semanticWrapperException(e, id);
          }
      }
      ;

//
// Directive block
//

// Doesn't change env
directiveBlock[Environment env, BlockInterface parent, String type]
    : #( directive:DIRECTIVE {
            parseDirectives(#directive.getText(), #directive.getLine(),
                            #directive.getColumn(), #directive.getFilename(),
                            env, type, parent);
         }
       )
    ;

//
// Netlist block
//

netlistBlock[Environment env, CellImpl cell]
    : #( netlist:NETLIST {
            final CDLLexer lexer =
                new CDLLexer(new StringReader(#netlist.getText()), true);
            lexer.setFilename(#netlist.getFilename());
            lexer.setColumn(#netlist.getColumn());
            lexer.setLine(#netlist.getLine());
            final CDLParser parser = new CDLParser(lexer);

            parser.setASTNodeClass(CDLParser.ASTWithToken.class.getName());
            try {
                parser.startDeviceList();
            } catch (TokenStreamException e) {
                throw semanticWrapperException("Error parsing netlist block", e, netlist);
            }
            final AST ast = parser.getAST();
            final CDLWalker walker = new CDLWalker();
            final Template templ = new Template(Collections.EMPTY_MAP);
            final Map params = new HashMap();
            walker.deviceList(ast, new BlockEnvironment(
                                       CDLWalker.sanitizeEnvironment(env),
                                       new LocalEnvironment(params)), templ);
            ((NetlistBlock) cell.getBlockInterface().iterator(BlockInterface.NETLIST).next()).setCDLTemplate(templ, params);
         }
      )
    ;

//
// env block stuff
//

// paramEnv can be null if an env block is not allowed at this level.
// Otherwise it's the environment in which the metaparameters, ports,
// implied ports, and constants of cell are bound, but none of the body of
// cell.  It's read-only.
envBlock[CellImpl cell, BlockEnvironment paramEnv]
    {
        final Environment blockEnv = new LocalEnvironment(new LinkedHashMap());
        final Environment namedEnvBindings =
            new ChainEnvironment(paramEnv, blockEnv);
    }
    : #( ENV ( envNameBlock[namedEnvBindings] )* )
      { EnvBlock block = cell.getEnvironments();
        block.setEnvironment(namedEnvBindings, blockEnv, cpe);
        // iterate over defined environments, and set the container cell for
        // each of the environment
        final UserDefinedValue container =
            (UserDefinedValue) activeUDV.getLast();
        for (EnvironmentEntryIterator i = blockEnv.entryIterator();
             i.hasNext(); ) {
            final EnvironmentEntry entry = i.next();
            final Value val = entry.getValue();
            if (val instanceof UserDefinedValue) {
                ((UserDefinedValue) val).updateEnvironmentContainer(container);
            }
        }
      }
    ;

// A single named environment
envNameBlock[Environment namedEnvBindings]
    : #( NAMED_ENV typeDeclaration[namedEnvBindings, true] )
    ;

//
// Java block
//

// This doesn't change env
javaBlock[Environment env, CellImpl cell, boolean isEnv]
    : oldJavaBlock[env, cell]
    | newJavaBlock[env, cell, isEnv]
    ;

//
// New syntax Java block
//

newJavaBlock[Environment env, CellImpl cell, boolean isEnv]
    : #( j:NEW_JAVA
         newJavaStatements[
           env,
           cell.getBlockInterface().iterator(BlockInterface.JAVA).next(),
           (JavaCoSimInfo) cell.getCoSimInfo(CoSimParameters.JAVA),
           isEnv
         ]
      )
    ;

newJavaStatements[Environment env, BlockInterface javaBlock,
                  JavaCoSimInfo cosimInfo, boolean isEnv]
    : #( BODY_STATEMENT_LIST
         ( newJavaStatement[env, javaBlock, cosimInfo, isEnv] )*
      )
    ;

newJavaStatement[Environment env, BlockInterface javaBlock,
                 JavaCoSimInfo cosimInfo, boolean isEnv]
    : newJavaClassStatement[env, cosimInfo, isEnv]
    | javaChannelStatement[env, cosimInfo]
    | directiveBlock[env, javaBlock, BlockInterface.JAVA]
    ;

newJavaClassStatement[Environment env, JavaCoSimInfo cosimInfo, boolean isEnv]
    {
        MetaParamDefinition[] metaParameters = null;
        // Only contains Strings and arrays of Strings.  See comments in
        // JavaCoSimDevice.
        Vector channelAndNodeNames = new Vector();
        // See comments in JavaCoSimDevice.
        Vector initializers = null;
    }

    : #( NEW_JAVA_CLASS
         className:IDENT
         ( metaParameters=javaClassMetaParameters[env] )?
         instanceName:IDENT
         ( javaClassParameters[env, channelAndNodeNames] )?
         ( initializers=javaClassInitializers[env] )?
       )
       {
           String realInstanceName = instanceName.getText();
           if (realInstanceName.equals("_"))
               realInstanceName = "anonymous device";
           if (metaParameters == null)
               metaParameters = new MetaParamDefinition[0];

           // channelAndNodeNames can be optionally empty (for bug 11893), we
           // could define this to mean that all ports are to be passed to the
           // Java class; however, since the Java block seems to be phased out,
           // this has not yet been implemented

           // this constructor is only called here
           final JavaCoSimDevice classDescriptor =
               new JavaCoSimDevice(className.getText(),
                                   // pass null for device name if we are
                                   // non-env so the device will be called
                                   // pr.ef.ix instead of pr.ef.ix.inst
                                   isEnv ? realInstanceName : null,
                                   metaParameters,
                                   channelAndNodeNames,
                                   initializers);
           cosimInfo.addClass(classDescriptor);
       }
    ;

javaClassMetaParameters[Environment env]
returns [MetaParamDefinition[] metaParams]
    {
        final Vector accum = new Vector();
        Object metaParam = null;
    }
    : #( JAVA_CLASS_META_PARAMETERS
         ( metaParam=javaClassMetaParameter[env] { accum.add(metaParam); } )+
       )
       { metaParams = (MetaParamDefinition[]) accum.toArray(new MetaParamDefinition[0]); }
    ;

javaClassMetaParameter[Environment env] returns [MetaParamDefinition metaParam]
    { Value v = null; }
    : #( EXPRESSION v=expr[env, false]
         {
             final String name;
             if (v.getInstanceName() == null)
                 name = null;
             else
                 name = v.getInstanceName().getAsString('.');
             metaParam = new MetaParamDefinition(
                     name,
                     ((MetaParamValueInterface) v).toMetaParam());
         }
       )
    ;

// returns Vector<JavaClassParameter>
javaClassParameters[Environment env, Vector params]
    {
        JavaClassParameter param = null;
    }
    : #( JAVA_CLASS_PARAMETERS
         ( param=javaClassParameter[env] { params.add(param); } )+
       )
    ;

javaClassParameter[Environment env] returns [JavaClassParameter param]
    { Value v = null; int[] timingParams = null; }
    : #( JAVA_CLASS_PARAMETER v=expr[env, false]
           ( timingParams=javaChannelTimingInfo[env] )?
         {
             if (v instanceof ArrayValue) {
                 final ArrayValue av = (ArrayValue) v;
                 final CoSimChannelNames names = av.getCoSimChannelNames();
                 // If there's only one element, unwrap it
                 if (names.hasOneElement()) {
                     param = new JavaClassParameter(names.getFirstElement());
                 } else {
                     param = new JavaClassParameter(names);
                 }
             } else {
                 // v is a channel? TODO: better error reporting,
                 // commenting
                 param = 
                    new JavaClassParameter
                    (v.getInstanceName().getAsString('.'));
             }
             if(timingParams != null) {
                 param.setTimingParams(timingParams);
             }
         }
       )
    ;

// STUB
javaClassInitializers[Environment env] returns [Vector rv]
    { rv = null; }
    : JAVA_CLASS_INITIALIZERS
    ;

/**
 * Adds the channel to the environment and the cosimInfo.
 **/
javaChannelStatement[Environment env, JavaCoSimInfo cosimInfo]
    {
        int[] timingParams = null;
        Pair arrayPair = null;
    }
    : #( CHANNEL
         id:IDENT
         ( arrayPair=arraySelector[env, null, false] )?
         ( timingParams=javaChannelTimingInfo[env] )?
       )
    {
        final String channelName = id.getText();
        final HierName channelHierName = HierName.makeHierName(channelName);
        final JavaChannelContainerValue channelValue;
        final SubscriptSpecInterface arraySpec;
        System.out.println("arrayPair is " + arrayPair);
        if (arrayPair == null) {
            arraySpec = null;
            channelValue = new JavaChannelContainerValue(channelHierName);
        } else {
            arraySpec = (SubscriptSpecInterface) arrayPair.getFirst();
            channelValue = new JavaChannelContainerValue(channelHierName,
                                                         arraySpec);
        }

        try {
            env.bind(Symbol.create(channelName), channelValue);
        } catch (SymbolRedeclaredException e) {
            throw semanticWrapperException("trouble making java channel", e, id);
        }
        cosimInfo.addInternalChannel(channelName, timingParams, arraySpec);
    }
    ;

javaChannelTimingInfo[Environment env]
    returns [int [] rv]
    {
        // There can't be array accesses in the antlr code, only in
        // the included java code, so this can't be an array
        Value slackValue, ffValue, bbValue, fbValue, bfValue;
    }
    : #( c:CHANNEL_TIMING_INFO
         slackValue=slack:expr[env, false]
         ( /* empty */
         {
             rv = new int[1];
             rv[0] = getInt(slackValue, slack);
         }
           | ffValue=ff:expr[env, false]
             bbValue=bb:expr[env, false]
             fbValue=fb:expr[env, false]
             bfValue=bf:expr[env, false]
         {
             rv = new int[5];
             rv[0] = getInt(slackValue, slack);
             rv[1] = getInt(ffValue, ff);
             rv[2] = getInt(bbValue, bb);
             rv[3] = getInt(fbValue, fb);
             rv[4] = getInt(bfValue, bf);
         }
         )
       )
    ;

//
// Old syntax Java block
//

oldJavaBlock[Environment env, CellImpl cell]
    : #( JAVA oldJavaStatements[env, cell] )
    ;

oldJavaStatements[Environment env, CellImpl cell]
    : #( BODY_STATEMENT_LIST ( oldJavaStatement[env, cell] )* )
    ;

oldJavaStatement[Environment env, CellImpl cell]
    : oldJavaClassStatement[env, cell]
    | inputChannelStatement[env, cell]
    | outputChannelStatement[env, cell]
    | internalChannelStatement[env, cell]
    ;

oldJavaClassStatement[Environment env, CellImpl cell]
    : #( JAVACLASS cls:QuotedString ) { cell.setJavaClass(cls.getText()); } 
    ;

inputChannelStatement[Environment env, CellImpl cell]
    : #( INPUTCHAN cls:QuotedString )
    { cell.setInputChannel(modulizeOldJavaChannel(cls.getText())); }
    ;

outputChannelStatement[Environment env, CellImpl cell]
    : #( OUTPUTCHAN cls:QuotedString )
    { cell.setOutputChannel(modulizeOldJavaChannel(cls.getText())); }
    ;

internalChannelStatement[Environment env, CellImpl cell]
    : #( INTERNALCHAN cls:QuotedString )
    { cell.setInternalChannel(modulizeOldJavaChannel(cls.getText())); }
    ;

//
// Verilog block
//

verilogBlock[Environment env, CellImpl cell]
    {
        final BlockIterator bi = cell.getBlockInterface()
                                     .iterator(BlockInterface.VERILOG);
        final VerilogBlock vb;
        if (bi.hasNext()) {
            vb = (VerilogBlock) bi.next();
        } else {
            vb = new VerilogBlock();
            bi.add(vb);
        }
    }
    : #( VERILOG ( verilogNamed[env, vb] )* )
    ;


verilogNamed[Environment env, VerilogBlock vb]
    {
        String n = null;
        VerilogBlock.NamedBlock nb = null;
    }
    : name:IDENT { n = name.getText();
                   nb = new VerilogBlock.SimpleNamedBlock(n); }
      verilogStatements[env, nb]
      {
          if (vb.getNamedBlock(n) != null) {
              syntaxError("Redefining named verilog block " + n, name);
          }
          vb.addNamedBlock(nb);
      }
    ;

verilogStatements[Environment env, VerilogBlock.NamedBlock nb]
    : #( BODY_STATEMENT_LIST ( verilogStatement[env, nb] )* )
    ;

verilogStatement[Environment env, VerilogBlock.NamedBlock nb]
    {
        final List<Value> implied = new ArrayList<>();
        final Map<String,Value> specified = new LinkedHashMap<>();
        final List<String> files = new ArrayList<>();
        TupleValue tuple = null;
        final List<Value> params = new ArrayList<>();
    }
    : module:IDENT
      ( tuple = expressionList[env, false] {
          for (int i = 0; i < tuple.getSize(); ++i) {
              final Value elem;
              try {
                  elem = tuple.accessTuple(i);
              } catch (InvalidOperationException e) {
                  throw new AssertionError("Cannot accessTuple on: " + tuple);
              }
              if (elem instanceof IntValue || elem instanceof FloatValue) {
                  params.add(elem);
              } else {
                  final SemanticException se = new SemanticException("Only int and float expressions allowed as module parameters", module.getFilename(), module.getLine(), module.getColumn());
                  throw se;
              }
          }
      } )?
      ( verilogParameters[env, implied, specified] )?
      ( verilogFiles[env, files] )? {
          nb.addInstance(new VerilogBlock.Instance(
              module.getText(), 
              params,
              implied.size() == 0 && specified.size() != 0 ? null : implied,
              specified.size() == 0 ? null : specified,
              files));
      }
      SEMI
    | verilogFiles[env, files] {
        nb.addFiles(files);
      }
      SEMI
    | directiveBlock[env, nb, BlockInterface.VERILOG]
    ;

verilogParameters[Environment env, List<Value> implied,
                  Map<String,Value> specified]
    : ( verilogParameter[env, implied, specified] )+
    ;

verilogParameter[Environment env, List<Value> implied,
                 Map<String,Value> specified]
    : verilogImplictParameter[env, implied]
    | verilogExplictParameter[env, specified]
    ;

verilogImplictParameter[Environment env, List<Value> implied]
    {
        Value v = null;
    }
    : v = expr[env, true] { implied.add(v); }
    ;

verilogExplictParameter[Environment env, Map<String,Value> specified]
    {
        Value v = null;
        ASTWithInfo p;
    }
    : ( p1:DOT_IDENT { p = p1; } | p2:ESCAPED_VERILOG_IDENT { p = p2; } )
      ( v = expr[env, true] )? RPAREN {
        final String port = p.getText();
        if (specified.containsKey(port)) {
            final SemanticException se
                = new SemanticException("Port '" + port + "' redeclared",
                    p.getFilename(), p.getLine(), p.getColumn());
            throw se;
        }
        specified.put(port, v);
      }
    ;

verilogFiles[Environment env, List<String> files]
    : ( s:QuotedString { files.add(s.getText()); } )+
    ;

partialExtract returns [Triplet p]
    {
        p = null;
        final Collection c = new ArrayList();
        String envName = null;
        TupleValue metas = null;
        String cellName;
        String special = "";
    }
    : type:IDENT { cellName = type.getText(); }
      ( metas = expressionList[new LocalEnvironment(), false] {
          cellName = UserDefinedValue.getTypeName(cellName, metas);
      })?
      ( { Boolean plus = null; }
        ( PLUS { plus = Boolean.TRUE; } | MINUS { plus = Boolean.FALSE; } )
        ( PLUS { special = "+"; } | MINUS { special = "-"; } )?
        { final StringBuffer sb = new StringBuffer(special); }
        ( partialExtractExpr[sb] ) { c.add(new Pair(sb.toString(), plus)); }
      )*
      ( env:IDENT { envName = env.getText(); }
        ( metas = expressionList[new LocalEnvironment(), false] {
            envName = UserDefinedValue.getTypeName(envName, metas);
        })?
      | times:TIMES { envName = "*"; } )?
      { p = new Triplet(cellName, envName, c); }
    ;

partialExtractExpr[StringBuffer sb]
    : #( FIELD_ACCESS partialExtractExpr[sb] fi:FIELD_IDENT
         { sb.append('.'); sb.append(fi.getText()); }
       )
    | #( ARRAY_ACCESS partialExtractExpr[sb] { sb.append('['); }
         partialExtractArraySelector[sb] { sb.append(']'); } )
    | id:IDENT { sb.append(id.getText()); }
    ;

partialExtractArraySelector[StringBuffer sb]
    : #( arrayExpr:ARRAY_SUB
         partialExtractRange[sb]
         ( { sb.append(','); } partialExtractRange[sb] )*
       )
    ;

partialExtractRange[StringBuffer sb]
    : #( RANGE #( EXPRESSION n:NUM_INT { sb.append(n.getText()); }) )
    ;

getHeaderAST returns [Pair p]
    : #( VAR_DECL IDENT ( INLINE )? ( FLATTEN )?
         #( TYPE
            #( USER_TYPE id:IDENT metas:EXPRESSION_LIST) ) ) {
        p = new Pair(id, metas);
    }
    ;

getCellData[ASTWithInfo errorAST, Environment paramEnv,
            CellInterface envContainer]
returns [CellData data]
    {
        data = null;
    }
    : cell:IDENT ( cellMeta:EXPRESSION_LIST )?
         ( env:IDENT ( envMeta:EXPRESSION_LIST )? )? {
        if (envContainer != null && env == null) {
            env = cell;
            envMeta = cellMeta;
            try {
                final AST ast =
                    cpe.getInstantiationAST(
                        envContainer.getFullyQualifiedType(),
                        HierName.makeHierName("__"));
                final Pair p = getHeaderAST(ast);
                cell = (ASTWithInfo) p.getFirst();
                cellMeta = (ASTWithInfo) p.getSecond();
            } catch (antlr.TokenStreamException e) { }
        }
        data = getCellFromAST(cell, cellMeta, env, envMeta, errorAST, paramEnv);
    }
    ;

alintFanin[Environment env] returns [Value v]
    { AlintFaninValue.State st = null; Value node = null; v = null; }
    : node = expr[env, false, true]
      ( s:NUM_INT 
        { st = s.getText().equals("0") ? AlintFaninValue.State.ZERO
                                       : AlintFaninValue.State.ONE;
        }
      | PLUS { st = AlintFaninValue.State.RISING; }
      | MINUS { st = AlintFaninValue.State.FALLING; }
      )
      { v = new AlintFaninValue(node, st); }
    ;
