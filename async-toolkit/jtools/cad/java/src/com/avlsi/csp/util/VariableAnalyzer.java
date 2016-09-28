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

package com.avlsi.csp.util;

import com.avlsi.cell.CellUtils;
import com.avlsi.csp.ast.*;
import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.csp.util.CSPCellInfo;

import com.avlsi.fast.metaparameters.*;

import com.avlsi.fast.ports.PortTypeInterface;
import com.avlsi.fast.ports.PortDefinition;

import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;

import java.math.BigInteger;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TreeMap;
import java.util.Vector;

import java.util.Enumeration;
import java.io.FileInputStream;
import java.io.IOException;
import com.avlsi.io.Printable;

/**
 * Class to analyze the use of variables.  Determines the implied set 
 * (of undeclared variables).  Also performs some type-checking.  
 * Associates types with channels and undeclared variables when these types
 * can be determined.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class VariableAnalyzer {
    /**
     * Special void type.
     **/
    private static final Type VOID = new Type() {
        public void accept(VisitorInterface v) throws VisitorException { }
        public int dimension() { return 0; }
        public String toString() { return "void"; }
    };

    /**
     * Special don't-care type.
     **/
    private static final Type ANY = new Type() {
        public void accept(VisitorInterface v) throws VisitorException { }
        public int dimension() { return 0; }
        public String toString() { return "unknown"; }
    };

    private static final ResourceBundle INVALID = new NullResourceBundle();
    private static ResourceBundle resourceBundle = null;
    public static ResourceBundle getResourceBundle() {
        if (resourceBundle == null) {
            try {
                resourceBundle = ResourceBundle.getBundle(
                        "com.avlsi.csp.resources.typechecker");
            } catch (MissingResourceException e) {
                System.err.println("Cannot load type checker error messages.");
                resourceBundle = INVALID;
            }
        }
        return resourceBundle;
    }

    private static class TypeError extends SimpleProblem {
        public TypeError(final String errorCode, final ParseRange pr,
                         final Object[] args) {
            super(errorCode, pr, args);
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

    /**
     * Metaparameter and ports information.  Use <code>null</code> to avoid
     * type checking channel expressions.
     **/
    CSPCellInfo cellInfo;

    public VariableAnalyzer(CSPCellInfo cellInfo) {
        this.cellInfo = cellInfo;
    }

    /** 
     * A nested class used for reporting results.  Sets contain objects of type
     * java.lang.String.  
     **/
    public static class Results {
        private Map<String,ParseRange> undeclaredReads; 
        private Map<String,ParseRange> undeclaredWrites;
        private Map<String,Type> undeclaredTypes;
        private Map/*<ExpressionInterface,Type>*/ expressionTypes;
        private Map/*<IdentifierExpression,String>*/ identUse;
        private Set<String> identRef;
        private Set<Type> identRefTypes;
        private Set<Type> initializerTokens;
        private boolean inFunctionInitializer;
        private boolean inInitializer;
        final RefinementResolver resolver;
        private Set declarationWarned = new HashSet();
        private Collection/*<VariableAnalyzerVisitorException>*/ errors;

        public Results(final RefinementResolver resolver) {
            this(resolver, Collections.EMPTY_MAP);
        }

        public Results(final RefinementResolver resolver, final Map identUse) {
            undeclaredReads = new HashMap<String,ParseRange>();
            undeclaredWrites = new HashMap<String,ParseRange>();
            undeclaredTypes = new HashMap<String,Type>();
            expressionTypes = new HashMap/*<ExpressionInterface,Type>*/();
            // XXX: We really want to used IdentityLinkedHashMap here
            this.identUse =
                new LinkedHashMap/*<IdentifierExpression,Type>*/(identUse);
            this.identRef = new HashSet<String>();
            this.identRefTypes = new HashSet<Type>();
            this.initializerTokens = new HashSet<Type>();
            this.inFunctionInitializer = false;
            this.inInitializer = false;
            this.resolver = resolver;
            this.errors = new ArrayList();
        }

        public void useIdent(final IdentifierExpression ident,
                             final Type type) {
            identUse.put(ident, type);
            if (inFunctionInitializer) initializerTokens.add(type);
            if (!inInitializer) {
                identRef.add(ident.getIdentifier());
                identRefTypes.add(type);
            }
        }

        /** 
         * Returns the set intersection(undeclaredReads, undeclaredWrites).  
         *
         * @return Set of String variable names
         **/
        public Set<String> getImpliedSet() {
            Set<String> results =
                new HashSet<String>(undeclaredWrites.keySet());
            results.retainAll (undeclaredReads.keySet());
            return results;
        }

        /** 
         * Returns the set undeclaredWrites \ undeclaredReads 
         * 
         * @return Set of String variable names
         **/
        public Set<String> getUnusedSet() {
            Set<String> results =
                new HashSet<String>(undeclaredWrites.keySet());
            results.removeAll (undeclaredReads.keySet());
            return results;
        }

        /** 
         * Returns the set undeclaredReads \ undeclaredWrites 
         * 
         * @return Set of String variable names
         **/
        public Set<String> getUninitializedSet() {
            Set<String> results =
                new HashSet<String>(undeclaredReads.keySet());
            results.removeAll (undeclaredWrites.keySet());
            return results;
        }

        /** 
         * Returns the map intersection(undeclaredReads, undeclaredWrites).  
         *
         * @return Map of String variable names
         **/
        public Map<String,ParseRange> getImpliedMap() {
            Map<String,ParseRange> results =
                new TreeMap<String,ParseRange>(undeclaredWrites);
            results.keySet().retainAll (undeclaredReads.keySet());
            return results;
        }

        /** 
         * Returns the map undeclaredWrites \ undeclaredReads 
         * 
         * @return Map of String variable names
         **/
        public Map<String,ParseRange> getUnusedMap() {
            Map<String,ParseRange> results =
                new TreeMap<String,ParseRange>(undeclaredWrites);
            results.keySet().removeAll (undeclaredReads.keySet());
            return results;
        }

        /** 
         * Returns the map undeclaredReads \ undeclaredWrites 
         * 
         * @return Map of String variable names
         **/
        public Map<String,ParseRange> getUninitializedMap() {
            Map<String,ParseRange> results =
                new TreeMap<String,ParseRange>(undeclaredReads);
            results.keySet().removeAll (undeclaredWrites.keySet());
            return results;
        }

        /** 
         * Returns the map union(undeclaredReads, undeclaredWrites).  
         *
         * @return Map of String variable names
         **/
        public Map<String,ParseRange> getUndeclaredMap() {
            Map<String,ParseRange> results =
                new TreeMap<String,ParseRange>(undeclaredReads);
            results.putAll (undeclaredWrites);
            return results;
        }

        /** 
         * Returns a map from the names of undeclared variables to the types of
         * undeclared variables, or maps to null if the type is unknown.
         *
         * @return Map from String to Type.
         **/
        public Map<String,Type> getUndeclaredTypes() {
            return undeclaredTypes;
        }

        public Type getUndeclaredType(final String s) {
            return undeclaredTypes.get(s);
        }

        public void setUndeclaredType(final String s, final Type t) {
            undeclaredTypes.put(s, t);
        }

        /** 
         * Returns a map from expressions to the types of expressions, or maps 
         * to null if the type is unknown.
         *
         * @return Map from ExpressionInterface to Type.
         **/
        public Map/*<ExpressionInterface,Type>*/ getExpressionTypes() {
            Map/*<ExpressionInterface,Type>*/ results =
                new HashMap/*<ExpressionInterface,Type>*/(expressionTypes);
            return results;
        }

        /**
         * Return the map of undeclared reads.  Only used internally.
         **/
        private Map<String,ParseRange> getUndeclaredWrites() {
            return undeclaredWrites;
        }

        /**
         * Return the map of undeclared writes.  Only used internally.
         **/
        private Map<String,ParseRange> getUndeclaredReads() {
            return undeclaredReads;
        }

        /** Adds an object to the specified undeclared map **/
        public void addUndeclared(Map<String,ParseRange> undeclaredMap,
                                  String o, ParseRange r) {
            if (!undeclaredMap.containsKey(o)) undeclaredMap.put(o,r);
        }

        /** Sets the type of an expression **/
        public void setType(ExpressionInterface e, Type t) {
            if (!(getType(e) instanceof NodeType))
                expressionTypes.put(e, t);
        }

        /** Gets the type of an expression **/
        public Type getType(ExpressionInterface e) {
            return (Type) expressionTypes.get(e);
        }
        
        // For this system of variable lifting to work, we require that
        // different variables must be represented by different instances of
        // IdentifierExpression (it is permissible to have different instances
        // of IdentifierExpression for the same variable) and each variable
        // must have different an unique instances of Type associated with it.
        // Reference equality (i.e., ==) is used, so care must be taken when
        // constructing or copying objects.

        public Object getIdentToken(final IdentifierExpression id) {
            assert identUse.containsKey(id);
            Object result = identUse.get(id);
            if (result == null) {
                result = getUndeclaredTypes().get(id.getIdentifier());
            }
            return result;
        }
        
        public Map/*<Object,String>*/ getTokenIdentMap() {
            // XXX: We really want to used IdentityLinkedHashMap here
            final LinkedHashMap/*<Object,String>*/ result =
                new LinkedHashMap/*<Object,String>*/();
            for (Iterator i = identUse.keySet().iterator(); i.hasNext(); ) {
                final IdentifierExpression id = (IdentifierExpression) i.next();
                final Object token = getIdentToken(id);
                if (result.containsKey(token)) {
                    assert result.get(token).equals(id.getIdentifier());
                } else {
                    result.put(token, id.getIdentifier());
                }
            }
            return result;
        }

        public Set<Type> getInitializerTokens() {
            return initializerTokens;
        }

        /**
         * Returns a set of identifiers (represented as Strings) referenced.
         **/
        public Set<String> getUsedIdentifiers() {
            return Collections.unmodifiableSet(identRef);
        }

        /**
         * Returns a set of types associated with identifiers referenced.
         **/
        public Set<Type> getUsedTokens() {
            return Collections.unmodifiableSet(identRefTypes);
        }

        public Collection getUndeclaredErrors(final boolean strictVars) {
            final Collection result = new ArrayList();
            if (strictVars) {
                Map undeclaredMap = getUndeclaredMap();
                for (final Iterator i = undeclaredMap.entrySet().iterator();
                     i.hasNext(); ) {
                    Map.Entry entry = (Map.Entry) i.next();
                    result.add(
                        new TypeError("type.checker.undeclared.var",
                                      (ParseRange) entry.getValue(),
                                      new Object[] { entry.getKey() }));
                }
            } else {
                final Map unusedMap = getUnusedMap();
                for (final Iterator i = unusedMap.entrySet().iterator();
                     i.hasNext(); ) {
                    Map.Entry entry = (Map.Entry) i.next();
                    result.add(
                        new TypeError("type.checker.undeclared.set.unused",
                                      (ParseRange) entry.getValue(),
                                      new Object[] { entry.getKey() }));
                }

                final Map uninitializedMap = getUninitializedMap();
                for (final Iterator i = uninitializedMap.entrySet().iterator();
                     i.hasNext(); ) {
                    Map.Entry entry = (Map.Entry) i.next();
                    result.add(
                        new TypeError("type.checker.undeclared.uninit",
                                      (ParseRange) entry.getValue(),
                                      new Object[] { entry.getKey() }));
                }
            }
            return result;
        }

        public Collection getErrors(final boolean strictVars) {
            final Collection result = new ArrayList();
            result.addAll(errors);
            result.addAll(getUndeclaredErrors(strictVars));
            return result;
        }
    }

    /**
     * Returns the results of analyzing the main body of a CSP program.
     *
     * @param p Program to analyze
     * @return Set of identifiers (as java.lang.String)
     **/
    public Results getResults (CSPProgram p, RefinementResolver resolver)
        throws VariableAnalysisException {
        Results results = new Results(resolver);
        Map<String,Type> predeclared = new HashMap<String,Type>();
        
        // Get metaparameters.
        
        predeclared.putAll(getMetaParamMap());

        // Get ports.

        predeclared.putAll(getPortMap());

        // Walk the syntax tree of the main program, if defined.  Otherwise,
        // return an empty set.

        SequentialStatement initSI = p.getInitializerStatement();
        StatementInterface si = p.getStatement();
        if (initSI != null || si != null)
            computeTypes (initSI, si, predeclared, Collections.EMPTY_MAP,
                          resolver, results, false);

        // return the results

        return results;
    }

    /**
     * Returns the results of analyzing the body of a function.
     *
     * @param f Function to analyze
     * @param initStmt initializer statement, containing declarations of
     *     and assignments to top level constants
     * @return Set of identifiers (as java.lang.String)
     **/
    public Results getResults (FunctionDeclaration f,
                               SequentialStatement initStmt,
                               RefinementResolver resolver)
        throws VariableAnalysisException {
        Map<String,Type> predeclared = new HashMap<String,Type>();
        // Get the set of metaparameters.
        
        predeclared.putAll (getMetaParamMap());

        // Get the set of ports.

        predeclared.putAll (getPortMap());

        // Get the set of formal arguments

        Map<String,Type> funcParams = new HashMap<String,Type>();

        // XXX: We really want to used IdentityLinkedHashMap here
        final LinkedHashMap identUse = new LinkedHashMap();
        for (Iterator i = f.getFormals().getDeclarations(); i.hasNext(); ) {
            final Declaration decl = (Declaration) i.next();
            funcParams.putAll (decl.getMap());
            for (Iterator j = decl.getDeclaratorList().getDeclarators();
                j.hasNext(); ) {
                final Declarator declarator = (Declarator) j.next();
                identUse.put(declarator.getIdentifier(),
                             declarator.getTypeFragment());
            }
        }

        // Add the function name to the list of funcParams variables

        if (f.getReturnType() != null) {
            funcParams.put (f.getName(), f.getReturnType());
            identUse.put(f.getNameIdentifier(), f.getReturnType());
        }

        // Walk the syntax tree of the function body

        final Results results = new Results(resolver, identUse);

        computeTypes(initStmt, f.getBodyStatement(), predeclared, funcParams,
                     resolver, results, true);

        // return the implied set

        return results;
    }

    private void computeTypes(SequentialStatement initStmt,
                              StatementInterface s,
                              Map<String,Type> declared,
                              Map<String,Type> funcParams,
                              RefinementResolver resolver,
                              Results results,
                              boolean isFunction)
        throws VariableAnalysisException {
        try {
            final HashMap<String,Type> withInit =
                new HashMap<String,Type>(declared);
            final VisitorImplementation visitorInit = 
                new VisitorImplementation (withInit, results);

            if (initStmt != null) {
                // do not use initStmt.accept(visitorInit), because then any
                // variable declaration in initStmt will be created in a
                // local scope, and won't be visible to the actual
                // statements
                results.inFunctionInitializer = isFunction;
                results.inInitializer = true;
                for (final Iterator i = initStmt.getStatements();
                     i.hasNext(); )
                    ((StatementInterface) i.next()).accept(visitorInit);
                results.inFunctionInitializer = false;
                results.inInitializer = false;
            }

            final Map<String,Type> withFuncParams =
                new HashMap<String,Type>(withInit);
            withFuncParams.putAll(funcParams);
            final VisitorImplementation visitor = 
                new VisitorImplementation (withFuncParams, results);

            if (s != null) s.accept(visitor);

            // validate the types of predeclared variables
            for (Type ty : declared.values()) ty.accept(visitor);

            // validate the types of function parameter and return value types
            for (Type ty : funcParams.values()) ty.accept(visitor);

        } catch (VisitorException e) {
            throw new VariableAnalysisException
                (s.getParseRange().fullString() +
                 ": Error in variable analysis. " + e.getMessage(), e);
        }
    }

    /**
     * Traverses an AST until known type information stabilizes.  Returns
     * results of final traversal.  (The traversal method should guarantee that
     * known type information is monotonically increasing.  Since there are a
     * finite number of expressions with unknown type, the method will
     * eventually return.)
     *
     * XXX: This method is dependent on the behavior of VisitorImplementation.
     * In particular, VisitorImplementation currently updates the undeclared
     * variable type information at the same time that the expression
     * information is updated.
     *
     * @param initStmt statements containing CAST constants
     * @param s Statement to traverse
     * @param declared Map of predeclared variable names to types
     * @param Results of traversal
     **/
    private void walkUntilStabilized(SequentialStatement initStmt,
                                     StatementInterface s,
                                     Map/*<String,Type>*/ declared,
                                     RefinementResolver resolver,
                                     Results results,
                                     boolean isFunction)
        throws VariableAnalysisException {
        int N = 0;      // N == number of currently known types
        int M = -1;     // M == number of previously known types

        try {
            while (N != M) {
                M = N;
                N = 0;
                HashMap/*<String,Type>*/ declaredIn =
                    new HashMap/*<String,Type>*/(declared);
                final VisitorImplementation visitor = 
                    new VisitorImplementation (declaredIn, results);

                if (initStmt != null) {
                    // do not use initStmt.accept(visitor), because then any
                    // variable declaration in initStmt will be created in a
                    // local scope, and won't be visible to the actual
                    // statements
                    results.inFunctionInitializer = isFunction;
                    results.inInitializer = true;
                    for (final Iterator i = initStmt.getStatements();
                         i.hasNext(); )
                        ((StatementInterface) i.next()).accept(visitor);
                    results.inFunctionInitializer = false;
                    results.inInitializer = false;
                }

                if (s != null) s.accept(visitor);
                Map/*<ExpressionInterface,Type>*/ types =
                    results.getExpressionTypes();
                for (Iterator i = types.keySet().iterator(); i.hasNext(); ) {
                    if (types.get(i.next()) != null)
                        N++;
                }
                if (N == M) {  // do this only once on the last iteration
                    // validate the types of predeclared variables
                    for (Iterator i = declared.values().iterator();
                         i.hasNext(); ) {
                        Type ty = (Type) i.next();
                        ty.accept(visitor);
                    }
                }
            }
        } catch (VisitorException e) {
            throw new VariableAnalysisException
                (s.getParseRange().fullString() +
                 ": Error in variable analysis. " + e.getMessage(), e);
        }
    }

    /** Get a Map of the ports of the current cell.  **/
    private /*@ non_null @*/ Map<String,Type> getPortMap() {
        Map<String,Type> results = new HashMap<String,Type>();
        for (Iterator i = cellInfo.getPortDefinitions(); i.hasNext(); ) {
            PortDefinition d = (PortDefinition) i.next();
            // TODO: We probably want to ignore Vdd, GND, and _RESET,
            // but eventually we will not want to ignore other nodes.
            results.put(d.getName(),
                        port2AST(d.getType(),
                                 PortDefinition.updateDirection(
                                     d.getDirection(),
                                     PortDefinition.FORWARD)));
        }
        return results;
    }

    /**
     * Maps a {@link PortTypeInterface} to a csp ast {@link Type}.
     **/
    private /*@ non_null @*/ Type port2AST(
            final /*@ non_null @*/ PortTypeInterface t,
            final int direction) {
        if (t instanceof com.avlsi.fast.ports.ArrayType) {
            final com.avlsi.fast.ports.ArrayType at =
                (com.avlsi.fast.ports.ArrayType) t;
            // XXX: what to do for parseRange of the nodes we create
            // here?
            return new ArrayType
                (new Range(new IntegerExpression(at.getMinIndex()),
                           new IntegerExpression(at.getMaxIndex())),
                 port2AST(at.getArrayedType(), direction));
        } else if (t instanceof com.avlsi.fast.ports.ChannelType) {
            final com.avlsi.fast.ports.ChannelType ct =
                (com.avlsi.fast.ports.ChannelType) t;
            return new ChannelType(computeChannelWidth(ct.getNumValues(),
                                                       ct.getWidth()),
                                   PortDirection.mapDirection(direction),
                                   ct.getTypeName());
        } else if (t instanceof com.avlsi.fast.ports.NodeType) {
            final com.avlsi.fast.ports.NodeType nt =
                (com.avlsi.fast.ports.NodeType) t;
            return new NodeType(nt.getWidth(),
                                PortDirection.mapDirection(direction),
                                nt.isArrayed());
        } else {
            assert t instanceof com.avlsi.fast.ports.StructureType;
            final com.avlsi.fast.ports.StructureType st =
                (com.avlsi.fast.ports.StructureType) t;
            final ChannelStructureType cst =
                new ChannelStructureType(st.getTag());
            for (Iterator i = st.iterator(); i.hasNext(); ) {
                final PortDefinition portDef = (PortDefinition) i.next();
                cst.addMember(portDef.getName(),
                              port2AST(portDef.getType(),
                                       PortDefinition.updateDirection(
                                           direction,
                                           portDef.getDirection())));
            }
            return cst;
        }
    }

    /**
     * Returns the base width of the channel, ie how many values
     * the unbundled channel can carry.
     **/
    private static int getChannelBase(final /*@ non_null @*/ String
                                          channelTypeName) {
        final int result = CellUtils.extractN(channelTypeName);
        assert result != -1;
        return result;
    }

    /**
     * Returns how many values a wide channel can carry.
     **/
    private static /*@ non_null @*/ BigInteger computeChannelWidth(
            final BigInteger narrow,
            final int width) {
        return narrow.pow(width);
    }

    /** Get a Map of the metaparameters of the current cell.  **/
    private Map<String,Type> getMetaParamMap() {
        Map<String,Type> results = new HashMap<String,Type>();
        for (Iterator i = cellInfo.getMetaParamDefinitions(); i.hasNext(); ) {
            MetaParamDefinition mp = (MetaParamDefinition) i.next();
            results.put (mp.getName(), meta2AST (mp.getType()));
        }
        return results;
    }

    /** Convert a metaparameter type into an AST type **/
    private Type meta2AST(MetaParamTypeInterface t) {
        if (t instanceof IntegerMetaParam) {
            return new IntegerType(true);
        } else if (t instanceof BooleanMetaParam) {
            return new BooleanType(true);
        } else if (t instanceof FloatMetaParam) {
            throw new AssertionError
                ((Object) "Float metaparameters unsupported.");
        } else if (t instanceof ArrayMetaParam) {
            ArrayMetaParam at = (ArrayMetaParam) t;
            // XXX: what to do for parseRange of the nodes we create
            // here?
            return new ArrayType
                (new Range(new IntegerExpression(at.getMinIndex()),
                           new IntegerExpression(at.getMaxIndex())),
                 meta2AST(at.getArrayedType()));
        } else {
            throw new AssertionError((Object) "Unknown metaparameter type.");
        }
    }

    private static boolean isBoolean(Type e) {
        return (e instanceof BooleanType) ||
               (e instanceof NodeType && !((NodeType) e).isArrayed()) ||
               e == ANY;
    }

    private static boolean isInteger(Type e) {
        return (e instanceof IntegerType) ||
               (e instanceof NodeType && ((NodeType) e).isArrayed()) ||
               e == ANY;
    }

    private static boolean isString(Type e) {
        return e instanceof StringType || e == ANY;
    }

    private static boolean isScalar(Type e) {
        return isBoolean(e) || isInteger(e) || isString(e);
    }

    /** 
     * Nested class used to walk the syntax tree.  Updates the results
     * structure and declared set.  Also performs some typechecking.
     **/
    private static class VisitorImplementation implements VisitorInterface {
        /** Declared variables in the current scope.  **/
        Map/*<String,Type>*/ declared;
        /** Results accumulated by walking the syntax tree.  **/
        Results results;

        /** Default interval used for integer with unspecified width. **/
        private static final int LARGE_WIDTH = 1024;
        private static final BigInteger LARGE_UPPER_BOUND =
            BigInteger.ONE.shiftLeft(LARGE_WIDTH).subtract(BigInteger.ONE);
        private static final BigInteger LARGE_LOWER_BOUND =
            LARGE_UPPER_BOUND.negate();
        private static final Interval LARGE_INTERVAL =
            new Interval(LARGE_LOWER_BOUND, LARGE_UPPER_BOUND);

        /** Both 'declared' and 'results' may be modified.  **/
        public VisitorImplementation (Map/*<String,Type>*/ declared,
                                      Results results) {
            this.declared = declared;
            this.results = results;
        }

        public void visitCSPProgram(CSPProgram p) throws VisitorException{
            throw new AssertionError((Object) "Can't get here.");
        }

        public void visitAddExpression(AddExpression e) 
            throws VisitorException {
            processBinary(e);

            final Type lty = getType(e.getLeft());
            final Type rty = getType(e.getRight());
            if (isString(lty)) {
                if (!isScalar(rty)) report("invalid.string.concat", e, rty);
                setString(e);
            } else if (isString(rty)) {
                if (!isScalar(lty)) report("invalid.string.concat", e, lty);
                setString(e);
            } else if (isInteger(lty) && isInteger(rty)) {
                setInteger(e);
                setInterval(getInterval(lty).add(getInterval(rty)), e);
            } else {
                report("invalid.add.operands", e, lty, rty);
                setInteger(e);
            }
        }

        public void visitAndExpression(AndExpression e) 
            throws VisitorException {
            processBitwiseBinary (e);

            final Type lty = getType(e.getLeft());
            final Type rty = getType(e.getRight());
            if (isInteger(lty) && isInteger(rty)) {
                setInterval(getInterval(lty).and(getInterval(rty)), e);
            }
        }

        public void visitArrayAccessExpression(ArrayAccessExpression e)
            throws VisitorException {
            e.getArrayExpression().accept(this);
            e.getIndexExpression().accept(this);
            expectInteger(e.getIndexExpression());

            Type t = getType(e.getArrayExpression());
            if (t instanceof ArrayType) {
                ArrayType at = (ArrayType) t;
                setType(e, at.getElementType());
            } else {
                e.checkParseRange();
                report("invalid.array", e, t);
                setInteger(e);
            }
        }

        public void visitBitRangeExpression(BitRangeExpression e) 
            throws VisitorException {
            e.getBitsExpression().accept(this);
            expectInteger(e.getBitsExpression());

            if (e.getMinExpression() != null) {
                e.getMinExpression().accept(this);
                expectInteger(e.getMinExpression());
            }

            e.getMaxExpression().accept(this);
            expectInteger(e.getMaxExpression());

            setInteger(e);
            if (e.getMinExpression() == null) {
                setInterval(new Interval(1), e);
            } else {
                setInterval(
                    getInterval(e.getBitsExpression())
                   .bitExtract(getInterval(e.getMinExpression()),
                               getInterval(e.getMaxExpression())), e);
            }
        }

        public void visitConditionalAndExpression(ConditionalAndExpression e)
            throws VisitorException {
            processBitwiseBinary(e);
            setBoolean(e);
        }

        public void visitConditionalOrExpression(ConditionalOrExpression e)
            throws VisitorException {
            processBitwiseBinary(e);
            setBoolean(e);
        }

        public void visitDivideExpression(DivideExpression e) 
            throws VisitorException {
            processArithmeticBinary(e);
            setInterval(
                getInterval(e.getLeft()).divide(getInterval(e.getRight())),
                e);
        }

        public void visitEqualityExpression(EqualityExpression e)
            throws VisitorException {
            processBitwiseComparison(e);
        }

        public void visitExponentialExpression(ExponentialExpression e) 
            throws VisitorException {
            processArithmeticBinary(e);
            setInterval(
                getInterval(e.getLeft()).pow(getInterval(e.getRight())),
                e);
        }

        private void processBuiltinString(final FunctionCallExpression e)
            throws VisitorException {
            int count = 0;
            for (Iterator i = e.getActuals(); i.hasNext(); ++count) {
                final ExpressionInterface arg = (ExpressionInterface) i.next();
                arg.accept(this);
                final Type ty = getType(arg);
                if (count == 0) {
                    if (!isInteger(ty) && !isBoolean(ty)) {
                        report("invalid.string.argument1", e, ty);
                    }
                } else if (count == 1) {
                    if (!isInteger(ty))
                        report("invalid.string.argument2", e, ty);
                } else if (count == 2) {
                    report("too.many.arguments", e, "string");
                }
            }
            if (count == 0) {
                report("too.few.arguments", e, "string");
            }
            setString(e);
        }

        private void processBuiltinPrint(final FunctionCallExpression e)
            throws VisitorException {
            int count = 0;
            for (Iterator i = e.getActuals(); i.hasNext(); ++count) {
                final ExpressionInterface arg = (ExpressionInterface) i.next();
                arg.accept(this);
                final Type ty = getType(arg);
                if (count == 0) {
                    if (i.hasNext()) {
                        if (!isInteger(ty)) {
                            report("invalid.print.tag", arg, ty);
                        }
                    } else {
                        if (!isScalar(ty)) {
                            report("invalid.print.message", arg, ty);
                        }
                    }
                } else if (count == 1) {
                    if (!isScalar(ty)) {
                        report("invalid.print.message", arg, ty);
                    }
                } else if (count == 2) {
                    report("too.many.arguments", arg, "print");
                }
            }
            if (count == 0) {
                report("too.few.arguments", e, "print");
            }
            setVoid(e);
        }

        private void processBuiltinAssert(final FunctionCallExpression e)
            throws VisitorException {
            int count = 0;
            for (Iterator i = e.getActuals(); i.hasNext(); ++count) {
                final ExpressionInterface arg = (ExpressionInterface) i.next();
                arg.accept(this);
                final Type ty = getType(arg);
                if (count == 0) {
                    if (!isBoolean(ty)) {
                        report("invalid.assert.argument1", arg, ty);
                    }
                } else if (count == 1) {
                    if (!isString(ty)) {
                        report("invalid.assert.argument2", arg, ty);
                    }
                } else if (count == 2) {
                    report("too.many.arguments", arg, "assert");
                }
            }
            if (count == 0) {
                report("too.few.arguments", e, "assert");
            }
            setVoid(e);
        }

        /**
         * Returns true if the argument is a packed structure.  A packed
         * structure is a structure that has a finite, declared width.  It
         * cannot contain strings.
         **/
        private boolean isPacked(final Type t) throws VisitorException {
            if (t instanceof BooleanType) {
                return true;
            } else if (t instanceof IntegerType) {
                final IntegerType it = (IntegerType) t;
                return it.getDeclaredWidth() != null;
            } else if (t instanceof ArrayType) {
                return isPacked(((ArrayType) t).getElementType());
            } else if (t instanceof StructureType) {
                final StructureDeclaration decl = getStructureDecl(null, t);
                if (decl == null) {
                    return false;
                } else {
                    return isPacked(decl);
                }
            } else {
                return false;
            }
        }

        private boolean isPacked(final StructureDeclaration decl)
            throws VisitorException {
            final boolean[] result = new boolean[] { true };
            (new DeclarationProcessor() {
                public void process(final Declarator d)
                    throws VisitorException {
                    final Type t = d.getTypeFragment();
                    result[0] &= isPacked(t);
                }
            }).process(decl.getDeclarations());
            return result[0];
        }

        private int getPackSize(final StructureDeclaration decl)
            throws VisitorException {
            final int[] result = new int[] { 0 };
            (new DeclarationProcessor() {
                public void process(final Declarator d)
                    throws VisitorException {
                    if (result[0] != -1) {
                        final int w = getPackSize(d.getTypeFragment());
                        if (w == -1) result[0] = -1;
                        else result[0] += w;
                    }
                }
            }).process(decl.getDeclarations());
            return result[0];
        }

        private int getPackSize(final Type t) throws VisitorException {
            int s = -1;
            if (t instanceof BooleanType) {
                s = 1;
            } else if (t instanceof IntegerType) {
                final IntegerType it = (IntegerType) t;
                final BigInteger w =
                    CspUtils.getIntegerConstant(it.getDeclaredWidth());
                s = w == null ? -1 : w.intValue();
            } else if (t instanceof ArrayType) {
                final ArrayType at = (ArrayType) t;
                final Range r = at.getRange();
                final BigInteger min =
                    CspUtils.getIntegerConstant(r.getMinExpression());
                final BigInteger max =
                    CspUtils.getIntegerConstant(r.getMaxExpression());
                if (min != null && max != null) {
                    final int w = getPackSize(at.getElementType());
                    if (w != -1) {
                        BigInteger num = max.subtract(min).add(BigInteger.ONE);
                        s = num.intValue() * w;
                    }
                }
            } else if (t instanceof StructureType) {
                final StructureDeclaration decl = getStructureDecl(null, t);
                if (decl != null) {
                    s = getPackSize(decl);
                }
            }
            return s;
        }

        private StructureDeclaration getStructureDecl(
                final AbstractASTNodeInterface e,
                final Type t) {
            StructureDeclaration decl = null;
            if (t instanceof StructureType) {
                StructureType st = (StructureType) t;
                final Pair p = (Pair)
                    results.resolver.getResolvedStructures().get(st);
                if (p == null) {
                    if (e != null) {
                        e.checkParseRange();
                        report("undefined.structure", e, st.getName());
                    }
                } else {
                    decl = (StructureDeclaration) p.getSecond();
                }
            } else {
                if (e != null) {
                    e.checkParseRange();
                    report("not.a.structure", e);
                }
            }
            return decl;
        }

        private void processBuiltinPack(final FunctionCallExpression e)
            throws VisitorException {
            int count = 0;
            int size = -1;
            for (Iterator i = e.getActuals(); i.hasNext(); ++count) {
                final ExpressionInterface arg = (ExpressionInterface) i.next();
                arg.accept(this);
                final Type ty = getType(arg);
                if (count == 0) {
                    processPacked(arg, ty, "packed.expected");
                    size = getPackSize(ty);
                } else if (count == 1) {
                    report("too.many.arguments", arg, "pack");
                }
            }
            if (count == 0) {
                report("too.few.arguments", e, "pack");
            }
            setInteger(e);
            if (size > 0) {
                setInterval(new Interval(size), e);
            }
        }

        private void processBuiltinUnpack(final FunctionCallExpression e)
            throws VisitorException {
            int count = 0;
            for (Iterator i = e.getActuals(); i.hasNext(); ++count) {
                final ExpressionInterface arg = (ExpressionInterface) i.next();
                arg.accept(this);
                final Type ty = getType(arg);
                if (count == 0) {
                    processPacked(arg, ty, "packed.expected");
                } else if (count == 1) {
                    if (!isInteger(ty) && !isBoolean(ty)) {
                        report("invalid.unpack.argument", arg, ty);
                    }
                } else if (count == 2) {
                    report("too.many.arguments", arg, "unpack");
                }
            }
            if (count < 2) {
                report("too.few.arguments", e, "unpack");
            }
            setVoid(e);
        }

        private String getName(final FunctionCallExpression e) {
            final ExpressionInterface func = e.getFunctionExpression();
            if (func instanceof IdentifierExpression) {
                return ((IdentifierExpression) func).getIdentifier();
            } else {
                return null;
            }
        }

        public void visitFunctionCallExpression(FunctionCallExpression e)
            throws VisitorException {
            // Find function call declaration using refinement resolver
            // to determine types of arguments and return value
            final Pair p = (Pair) results.resolver.getResolvedFunctions()
                                                  .get(e);

            final String name;
            final Object resolved = p == null ? null : p.getSecond();
            if (p == null) {
                name = getName(e);
                if ("print".equals(name)) {
                    processBuiltinPrint(e);
                } else if ("string".equals(name)) {
                    processBuiltinString(e);
                } else if ("assert".equals(name)) {
                    processBuiltinAssert(e);
                } else if ("pack".equals(name)) {
                    processBuiltinPack(e);
                } else if ("unpack".equals(name)) {
                    processBuiltinUnpack(e);
                } else if (name != null) {
                    report("unknown.function", e, name);
                    setAny(e);
                } else {
                    report("invalid.function", e);
                    setAny(e);
                }
                return;
            }

            final DeclarationList list;
            final Type rtype;
            if (resolved instanceof FunctionDeclaration) {
                final FunctionDeclaration decl = (FunctionDeclaration) resolved;
                list = decl.getFormals();
                name = decl.getName();
                rtype = decl.getReturnType() == null ? VOID
                                                     : decl.getReturnType();
            } else {
                final StructureDeclaration decl =
                    (StructureDeclaration) resolved;
                list = decl.getDeclarations();
                name = decl.getName();
                rtype = new StructureType(false, name);
            }
            setType(e, rtype);

            final Collection<Declarator> c = new ArrayList<Declarator>();
            (new DeclarationProcessor() {
                public void process(final Declarator d)
                    throws VisitorException {
                    c.add(d);
                }
            }).process(list);
            final Iterator<Declarator> decls = c.iterator();

            boolean argsNumWarned = false;
            int count = 0;
            for (Iterator i = e.getActuals(); i.hasNext(); ++count) {
                final ExpressionInterface arg = (ExpressionInterface) i.next();
                arg.accept(this);

                final Declarator decl;
                if (decls.hasNext()) {
                    decl = decls.next();
                } else {
                    if (!argsNumWarned) report("too.many.arguments", e, name);
                    argsNumWarned = true;
                    decl = null;
                }

                if (decl != null) {
                    final Type actualType = getType(arg);
                    final Type formalType = decl.getTypeFragment();
                    final int direction = decl.getDirection();

                    if (!isAssignable(formalType, actualType)) {
                        report("incompatible.parameter.passing", arg,
                               formalType, actualType, name);
                    } else if (direction != Declarator.IN &&
                               arg instanceof BitRangeExpression) {
                        report("output.bitrange.unsupported", arg,
                               decl.getIdentifier().getIdentifier());
                    }
                }
            }

            if (decls.hasNext()) {
                report("too.few.arguments", e, name);
            }
        }

        public void visitGreaterEqualExpression(GreaterEqualExpression e)
            throws VisitorException {
            processArithmeticComparison (e);
        }

        public void visitGreaterThanExpression(GreaterThanExpression e)
            throws VisitorException {
            processArithmeticComparison(e);
        }

        public void visitIdentifierExpression(IdentifierExpression e)
            throws VisitorException {

            // This method handles reading of variables.  Writing is handled 
            // explicitly in:
            //      visitAssignmentStatement() 
            //      visitReceiveStatement()

            processRead(e);
        }

        public void visitInequalityExpression(InequalityExpression e)
            throws VisitorException {
            processBitwiseComparison(e);
        }

        public void visitIntegerExpression(IntegerExpression e) 
            throws VisitorException {
            if (e instanceof BooleanExpression) {
                setBoolean(e);
            } else {
                setInteger(e);
                final IntegerType ity = (IntegerType) getType(e);
                ity.setInterval(
                    new Interval(new BigInteger(e.getValue(), e.getRadix())));
            }
        }

        public void visitLeftShiftExpression(LeftShiftExpression e)
            throws VisitorException {
            processArithmeticBinary(e);
            setInterval(
                getInterval(e.getLeft()).shiftLeft(getInterval(e.getRight())),
                e);
        }

        public void visitLessEqualExpression(LessEqualExpression e)
            throws VisitorException {
            processArithmeticComparison(e);
        }

        public void visitLessThanExpression(LessThanExpression e)
            throws VisitorException {
            processArithmeticComparison(e);
        }

        public void visitLoopExpression(LoopExpression e) 
            throws VisitorException {

            // Visit the min/max expression with the current scope

            e.getRange().getMinExpression().accept(this);
            expectInteger(e.getRange().getMinExpression());

            e.getRange().getMaxExpression().accept(this);
            expectInteger(e.getRange().getMaxExpression());

            // Visit the expression being looped with an augmented 
            // scope which includes the index variable.  

            Map/*<String,Type>*/ loopDeclared =
                new HashMap/*<String,Type>*/(declared);
            loopDeclared.put (e.getIndexVar(), new IntegerType());
            e.getExpression().accept(
                    new VisitorImplementation (loopDeclared, results));
            final Type ty = getType(e.getExpression());
            if (ty instanceof StringType &&
                e.getSeparator() != LoopExpression.PLUS) {
                report("nonstring.plus", e, e.getSeparatorString());
            }
            setType(e, ty);
        }

        public void visitMultiplyExpression(MultiplyExpression e)
            throws VisitorException {
            processArithmeticBinary(e);
            setInterval(
                getInterval(e.getLeft()).multiply(getInterval(e.getRight())),
                e);
        }

        public void visitNegateExpression(NegateExpression e) 
            throws VisitorException {
            processUnary(e);

            final Type ty = getType(e.getExpression());
            if (isInteger(ty)) {
                setInterval(getInterval(ty).negate(), e);
            }
        }

        public void visitNotExpression(NotExpression e) 
            throws VisitorException {
            processUnary(e);
            final Type ty = getType(e.getExpression());
            if (isInteger(ty)) {
                setInterval(getInterval(ty).not(), e);
            }
        }

        public void visitOrExpression(OrExpression e) 
            throws VisitorException {
            processBitwiseBinary(e);

            final Type lty = getType(e.getLeft());
            final Type rty = getType(e.getRight());
            if (isInteger(lty) && isInteger(rty)) {
                setInterval(getInterval(lty).or(getInterval(rty)), e);
            }
        }

        public void visitXorExpression(XorExpression e) 
            throws VisitorException {
            processBitwiseBinary(e);

            final Type lty = getType(e.getLeft());
            final Type rty = getType(e.getRight());
            if (isInteger(lty) && isInteger(rty)) {
                setInterval(getInterval(lty).xor(getInterval(rty)), e);
            }
        }

        public void visitPeekExpression(PeekExpression e) 
            throws VisitorException {
            e.getChannelExpression().accept(this); 
            expectChannel(e.getChannelExpression(), PortDirection.IN);
            setInteger(e);
            setInterval(getInterval(e.getChannelExpression()), e);
        }

        public void visitProbeExpression(ProbeExpression e) 
            throws VisitorException {
            e.getChannelExpression().accept(this); 
            expectChannel(e.getChannelExpression(), null);
            setBoolean(e);
        }

        public void visitReceiveExpression(ReceiveExpression e) 
            throws VisitorException {
            e.getChannelExpression().accept(this); 
            expectChannel(e.getChannelExpression(), PortDirection.IN);
            setInteger(e);
            setInterval(getInterval(e.getChannelExpression()), e);
        }

        public void visitRemainderExpression(RemainderExpression e)
            throws VisitorException {
            processArithmeticBinary(e);
            setInterval(
                getInterval(e.getLeft()).remainder(getInterval(e.getRight())),
                e);
        }

        public void visitRightShiftExpression(RightShiftExpression e)
            throws VisitorException {
            processArithmeticBinary(e);
            setInterval(
                getInterval(e.getLeft()).shiftRight(getInterval(e.getRight())),
                e);
        }

        public void visitStringExpression(StringExpression e)
            throws VisitorException {
            setString(e);
        }

        public void visitStructureAccessExpression(StructureAccessExpression e)
            throws VisitorException { 
            e.getStructureExpression().accept(this); 
            final Type t = getType(e.getStructureExpression());
            if (t instanceof ChannelStructureType) {
                final ChannelStructureType cst = (ChannelStructureType) t;
                final Type memberType = cst.getMemberType(e.getFieldName());
                if (memberType == null) {
                    report("invalid.field", e, e.getFieldName(), cst.getName());
                    setAny(e);
                } else {
                    setType(e, memberType);
                }
            } else {
                e.checkParseRange();
                report("not.a.defchan", e);
                setAny(e);
            }
        }

        public void visitMemberAccessExpression(MemberAccessExpression e)
            throws VisitorException {
            e.getStructureExpression().accept(this); 
            final Type t = getType(e.getStructureExpression());
            final StructureDeclaration sd = getStructureDecl(e, t);
            if (sd == null) {
                setAny(e);
            } else {
                final Map/*<String,Type>*/ m = sd.getMap();
                final Type mt = (Type) m.get(e.getMemberName());
                if (mt == null) {
                    e.checkParseRange();
                    report("invalid.member", e, e.getMemberName(),
                           sd.getName());
                    setAny(e);
                } else {
                    setType(e, mt);
                }
            }
        }

        public void visitSubtractExpression(SubtractExpression e)
            throws VisitorException {
            processArithmeticBinary(e);
            setInterval(
                getInterval(e.getLeft()).subtract(getInterval(e.getRight())),
                e);
        }

        public void visitAssignmentStatement(AssignmentStatement s)
            throws VisitorException {
            s.getRightHandSide().accept(this);
            processLvalue(s.getLeftHandSide());
            final Type lty = getType(s.getLeftHandSide());
            final Type rty = getType(s.getRightHandSide());
            switch (s.getKind()) {
              case AssignmentStatement.EQUAL:
                if (!isAssignable(lty, rty)) {
                    report("incompatible.assignment", s, lty, rty);
                }
                if (lty instanceof TemporaryIntegerType) {
                    setInterval(getInterval(rty), lty);
                }
                break;
              case AssignmentStatement.ADD:
                if (isString(lty)) {
                    if (!isScalar(rty)) report("invalid.string.concat", s, rty);
                } else if (!(isInteger(lty) && isInteger(rty))) {
                    report("invalid.add.operands", s, lty, rty);
                }
                break;
              case AssignmentStatement.SUBTRACT:
              case AssignmentStatement.MULTIPLY:
              case AssignmentStatement.DIVIDE:
              case AssignmentStatement.REMAINDER:
              case AssignmentStatement.LEFTSHIFT:
              case AssignmentStatement.RIGHTSHIFT:
                expectInteger(s.getLeftHandSide());
                expectInteger(s.getRightHandSide());
                break;
              case AssignmentStatement.AND:
              case AssignmentStatement.OR:
              case AssignmentStatement.XOR:
                if (!(isBoolean(lty) && isBoolean(rty)) &&
                    !(isInteger(lty) && isInteger(rty))) {
                    report("invalid.bitwise.operand", s, lty, rty);
                }
                break;
              default:
                throw new AssertionError("Unknown assignment kind: " +
                                         s.getKind());
            }
        }

        public void visitIncDecStatement(IncDecStatement s)
            throws VisitorException {
            s.getExpression().accept(this);
            expectInteger(s.getExpression());
        }

        public void visitDeterministicRepetitionStatement(
                DeterministicRepetitionStatement s)
            throws VisitorException {
            processGuardedStatement(s);
        }

        public void visitDeterministicSelectionStatement(
                DeterministicSelectionStatement s)
            throws VisitorException {
            processGuardedStatement(s);
        }

        public void visitExpressionStatement(ExpressionStatement s)
            throws VisitorException {
            s.getExpression().accept(this);
        }

        public void visitLoopStatement(LoopStatement s)
            throws VisitorException {
            // Visit the min/max expression with the current scope
            s.getRange().getMinExpression().accept(this);
            expectInteger(s.getRange().getMinExpression());

            s.getRange().getMaxExpression().accept(this);
            expectInteger(s.getRange().getMaxExpression());

            // Visit the statement being looped with an augmented 
            // scope which includes the index variable.  

            Map/*<String,Type>*/ loopDeclared =
                new HashMap/*<String,Type>*/(declared);
            final IntegerType t = new IntegerType();
            final Interval min = getInterval(s.getRange().getMinExpression());
            final Interval max = getInterval(s.getRange().getMaxExpression());
            if (min != null && max != null) t.setInterval(min.union(max));

            loopDeclared.put (s.getIndexVar(), t);
            results.useIdent(s.getIndexVarExpression(), t);
            s.getStatement().accept(
                    new VisitorImplementation (loopDeclared, results));
        }

        public void visitNonDeterministicRepetitionStatement(
                NonDeterministicRepetitionStatement s)
            throws VisitorException {
            processGuardedStatement(s);
            processLinkageTerms(s.getNeutralState());
        }

        public void visitNonDeterministicSelectionStatement(
                NonDeterministicSelectionStatement s)
            throws VisitorException {
            processGuardedStatement(s);
            processLinkageTerms(s.getNeutralState());
        }

        public void visitParallelStatement(ParallelStatement s)
            throws VisitorException {
            for (final Iterator i = s.getStatements(); i.hasNext(); ) 
                ((StatementInterface) i.next()).accept(this);
        }

        private void processPacked(final AbstractASTNodeInterface rhs,
                                   final Type ty,
                                   final String err)
            throws VisitorException {
            if (ty instanceof StructureType) {
                final StructureDeclaration sd = getStructureDecl(rhs, ty);
                if (sd != null && !isPacked(sd)) {
                    report("not.a.packed.structure", rhs, sd.getName());
                }
            } else if (ty instanceof ArrayType) {
                if (!isPacked(ty)) {
                    report("not.a.packed.array", rhs);
                }
            } else {
                report(err, rhs, ty);
            }
        }

        public void visitReceiveStatement(ReceiveStatement s)
            throws VisitorException {
            s.getChannelExpression().accept(this);
            expectChannel(s.getChannelExpression(), PortDirection.IN);
            if (s.getRightHandSide() != null) {
                processLvalue(s.getRightHandSide());
                final Type ty = getType(s.getRightHandSide());
                if (!isInteger(ty) && !isBoolean(ty)) {
                    processPacked(s.getRightHandSide(), ty,
                                  "int.bool.packed.expected");
                }
            }
        }

        public void visitSendStatement(SendStatement s)
            throws VisitorException {
            s.getChannelExpression().accept(this);
            expectChannel(s.getChannelExpression(), PortDirection.OUT);
            s.getRightHandSide().accept(this);
            final Type ty = getType(s.getRightHandSide());
            if (!isInteger(ty) && !isBoolean(ty)) {
                processPacked(s.getRightHandSide(), ty,
                              "int.bool.packed.expected");
            }
        }

        public void visitSequentialStatement(SequentialStatement s)
            throws VisitorException{

            // A sequential statement is the only place in which a variable
            // declaration should occur, and any variable declaration is 
            // limited to the scope of the sequential statement which is its
            // immediate parent.  Hence, we make a new scope here.

            Map/*<String,Type>*/ seqDeclared =
                new HashMap/*<String,Type>*/(declared);
            VisitorImplementation vi = 
                    new VisitorImplementation (seqDeclared, results);

            for (final Iterator i = s.getStatements(); i.hasNext(); )
                ((StatementInterface) i.next()).accept(vi);
        }

        public void visitErrorStatement(ErrorStatement s) 
            throws VisitorException { /* Do nothing.  */ }

        public void visitSkipStatement(SkipStatement s) 
            throws VisitorException { /* Do nothing.  */ }

        public void visitVarStatement(VarStatement s) throws VisitorException {
            if (s.getStatement() != null)
                throw new VisitorException ("Old-style CSP unsupported.");

            // For each declarator, visit the initializer and type specifier,
            // and add the identifier to the list of variables declared in the
            // current scope.

            for (final Iterator i = s.getDeclarationList().getDeclarations();
                    i.hasNext(); ) {
                final Declaration d = (Declaration) i.next();
                for (final Iterator j = d.getDeclaratorList().getDeclarators();
                        j.hasNext(); ) {
                    final Declarator dr = (Declarator) j.next();
                    dr.getTypeFragment().accept(this);
                    declared.put(dr.getIdentifier().getIdentifier(),
                                 dr.getTypeFragment());
                    setType(dr.getIdentifier(), dr.getTypeFragment());
                    results.useIdent(dr.getIdentifier(), dr.getTypeFragment());
                    // Handle initializer as if it had been an assignment
                    // statement.  This should be less bug-prone than the
                    // old way of handling the initializer explicity.
                    if (dr.getInitializer() != null) {
                        final AssignmentStatement fake =
                            new AssignmentStatement(dr.getIdentifier(),
                                                    dr.getInitializer());
                        fake.epr(dr);
                        fake.accept(this);
                    }
                }
            }
        }

        public void visitArrayType(ArrayType t) throws VisitorException{
            if (t.getElementType() != null) 
                t.getElementType().accept(this);

            t.getRange().getMinExpression().accept(this);
            expectInteger(t.getRange().getMinExpression());

            t.getRange().getMaxExpression().accept(this);
            expectInteger(t.getRange().getMaxExpression());
        }

        public void visitChannelType(com.avlsi.csp.ast.ChannelType t) 
            throws VisitorException { /* Do nothing.  */ }

        public void visitChannelStructureType(ChannelStructureType t) 
            throws VisitorException { /* Do nothing.  */ }

        public void visitIntegerType(IntegerType t) 
            throws VisitorException {
            if (t.getDeclaredWidth() != null) {
                t.getDeclaredWidth().accept(this);
            }
        }

        public void visitBooleanType(BooleanType t) 
            throws VisitorException { /* Do nothing.  */ }

        public void visitNodeType(NodeType t) 
            throws VisitorException { /* Do nothing.  */ }

        private void processStructureDeclaration(final StructureDeclaration sd)
            throws VisitorException {
            // visit each member's type and initializer if any
            for (final Iterator i = sd.getDeclarations().getDeclarations();
                 i.hasNext(); ) {
                final Declaration d = (Declaration) i.next();
                for (final Iterator j = d.getDeclaratorList().getDeclarators();
                     j.hasNext(); ) {
                    final Declarator dr = (Declarator) j.next();
                    dr.getTypeFragment().accept(this);
                    if (dr.getInitializer() != null) {
                        processLvalue(dr.getInitializer());
                        final Type lty = dr.getTypeFragment();
                        final Type rty = getType(dr.getInitializer());
                        if (!isAssignable(lty, rty)) {
                            report("incompatibe.structure.init", dr,
                                   lty, rty, dr.getIdentifier()
                                               .getIdentifier());
                        }
                    }
                }
            }
        }

        public void visitStringType(StringType t) throws VisitorException {}

        public void visitStructureType(StructureType t)
            throws VisitorException {
            final Pair p = (Pair)
                results.resolver.getResolvedStructures().get(t);
            if (p == null) {
                t.checkParseRange();
                report("undefined.structure", t, t.getName());
            } else {
                final StructureDeclaration sd =
                    (StructureDeclaration) p.getSecond();
                if (results.declarationWarned.add(sd)) {
                    processStructureDeclaration(sd);
                }
            }
        }

        public void visitIdentifierList(IdentifierList il)  {
            throw new AssertionError((Object) "Can't get here");
        }

        public void visitLinkageLoopTerm(LinkageLoopTerm term)
            throws VisitorException {
            // Visit the min/max expression with the current scope
            term.getRange().getMinExpression().accept(this);
            expectInteger(term.getRange().getMinExpression());

            term.getRange().getMaxExpression().accept(this);
            expectInteger(term.getRange().getMaxExpression());

            // Visit the linkage term being looped with an augmented
            // scope which includes the index variable.

            Map/*<String,Type>*/ loopDeclared =
                new HashMap/*<String,Type>*/(declared);
            loopDeclared.put (term.getIndexVar(), new IntegerType());
            Map/*<String,Type>*/ oldDeclared = declared;
            declared = loopDeclared;
            term.getTerm().accept(this);
            declared = oldDeclared;
        }
        public void visitLinkageExpressionTerm(LinkageExpressionTerm term)
            throws VisitorException {
            term.getExpression().accept(this);
        }
        public void visitLinkageArrayAccessExpression(
                LinkageArrayAccessExpression e) throws VisitorException {
            e.getArrayExpression().accept(this);
            e.getIndexExpression().accept(this);
        }
        public void visitLinkageIdentifierExpression(
                LinkageIdentifierExpression e) {
        }
        public void visitLinkageStructureAccessExpression(
                LinkageStructureAccessExpression e) {
        } 

        public void visitLoopGuard(LoopGuard s) throws VisitorException {
            throw new AssertionError((Object) "Can't get here");
        }   

        private void processUnary(final AbstractUnaryExpression e)
            throws VisitorException {
            e.getExpression().accept(this);
            setType(e, getType(e.getExpression()));
        }

        private void processBinary(final AbstractBinaryExpression e) 
            throws VisitorException {
            e.getLeft().accept(this);
            e.getRight().accept(this);
        }

        private void processBitwiseBinary(final AbstractBinaryExpression e)
            throws VisitorException {
            processBinary(e);
            final Type lty = getType(e.getLeft());
            final Type rty = getType(e.getRight());
            if (isBoolean(lty) && isBoolean(rty)) setBoolean(e);
            else if (isInteger(lty) && isInteger(rty)) setInteger(e);
            else {
                setAny(e);
                report("invalid.bitwise.operand", e, lty, rty);
            }
        }

        private void processArithmeticBinary(final AbstractBinaryExpression e)
            throws VisitorException {
            processBinary(e);
            expectInteger(e.getLeft());
            expectInteger(e.getRight());
            setInteger(e);
        }

        private void processArithmeticComparison(
                final AbstractBinaryExpression e) throws VisitorException {
            processBinary(e);
            expectInteger(e.getLeft());
            expectInteger(e.getRight());
            setBoolean(e);
        }

        private void processBitwiseComparison(final AbstractBinaryExpression e)
            throws VisitorException {
            processBinary(e);

            final Type lty = getType(e.getLeft());
            final Type rty = getType(e.getRight());

            if (!(isInteger(lty) && isInteger(rty)) &&
                !(isBoolean(lty) && isBoolean(rty))) {
                report("invalid.comparison", e, lty, rty);
            }

            setBoolean(e);
        }

        private void processLinkageTerms(final LinkageTerms terms)
            throws VisitorException {
            if (terms != null)
                for (Iterator i = terms.getTerms(); i.hasNext(); ) {
                    LinkageTermInterface term = (LinkageTermInterface) i.next();
                    term.accept(this);
                }
        }

        private void processGuards(Iterator guards) throws VisitorException {
            for (final Iterator i = guards; i.hasNext(); ) {
                final GuardedCommandInterface gci =
                    (GuardedCommandInterface) i.next();

                if (gci instanceof LoopGuard) {
                    final LoopGuard lg = (LoopGuard) gci;
                    // Visit the min/max expression with the current scope

                    lg.getRange().getMinExpression().accept(this);
                    expectInteger(lg.getRange().getMinExpression());

                    lg.getRange().getMaxExpression().accept(this);
                    expectInteger(lg.getRange().getMaxExpression());

                    // Visit the statement being looped with an augmented
                    // scope which includes the index variable.

                    Map/*<String,Type>*/ loopDeclared =
                        new HashMap/*<String,Type>*/(declared);
                    loopDeclared.put (lg.getIndexVar(), new IntegerType());
                    Map/*<String,Type>*/ oldDeclared = declared;
                    declared = loopDeclared;
                    processGuards(lg.getGuards().iterator());
                    declared = oldDeclared;
                } else {
                    final GuardedCommand gc = (GuardedCommand) gci;
                    if (gc instanceof GuardedCommandWithStatement) {
                        final GuardedCommandWithStatement gcws =
                            (GuardedCommandWithStatement) gc;
                        gcws.getGuardStatement().accept(this);
                    }
                    gc.getGuard().accept(this);
                    final Type gty = getType(gc.getGuard());
                    if (!isBoolean(gty)) {
                        report("invalid.guard", gc.getGuard(), gty);
                    }
                    gc.getCommand().accept(this);
                    processLinkageTerms(gc.getLinkageTerms());
                }
            }
        }
        private void processGuardedStatement(final AbstractGuardedStatement s)
            throws VisitorException {
            if (s.getElseStatement() != null)
                s.getElseStatement().accept(this);
            processGuards(s.getGuardedCommands());
        }
        private void processLvalue(final ExpressionInterface e)
            throws VisitorException {

            // If the lhs is an identifier, or an identifier qualified by a
            // bitfield specifier, then consider this identifier a candidate
            // for addition to the UndeclaredWrite set, and any bitfield
            // specifiers for additions to the UndeclaredRead set.  Otherwise,
            // consider the entire expression for additions to the
            // UndeclaredRead set.

            // XXX: note that this will not handle the following expression
            // properly:
            //
            //      x[15:0][3:2] = 5;
            //
            // This could be fixed by treating each bitrange expression in
            // turn.  Ignore this for now, since it will probably occur 
            // rarely, and should produce obvious error messages if it does
            // present a problem.

            if (e instanceof BitRangeExpression) {
                BitRangeExpression bre = (BitRangeExpression) e;
                ExpressionInterface bits = bre.getBitsExpression();
                if (bits instanceof IdentifierExpression) {
                    IdentifierExpression id = (IdentifierExpression) bits;
                    processWrite (id);
                }
                visitBitRangeExpression(bre);
            } else if (e instanceof IdentifierExpression) {
                IdentifierExpression id = (IdentifierExpression) e;
                processWrite (id);
            } else if (e != null) {
                e.accept(this);
            }
        }

        private void processIdent(Map<String,ParseRange> undeclared,
                                  IdentifierExpression expr)
            throws VisitorException {
            final String id = expr.getIdentifier();
            Type ty;
            if (!declared.containsKey(id)) {
                results.addUndeclared(undeclared, id, expr.getParseRange());
                ty = results.getUndeclaredType(id);
                if (ty == null) {
                    ty = new IntegerType();
                    results.setUndeclaredType(id, ty);
                }
            } else {
                ty = (Type) declared.get(id);
            }
            setType (expr, ty);
            results.useIdent(expr, ty);
        }
        private void processWrite(IdentifierExpression expr)
            throws VisitorException {
            processIdent(results.getUndeclaredWrites(), expr);
        }
        private void processRead(IdentifierExpression expr)
            throws VisitorException {
            processIdent(results.getUndeclaredReads(), expr);
        }

        private void setVoid(ExpressionInterface e) {
            setType(e, VOID);
        }
        private void setAny(ExpressionInterface e) {
            setType(e, ANY);
        }
        private void setInteger(ExpressionInterface e) {
            setType(e, new IntegerType());
        }
        private void setBoolean(ExpressionInterface e) {
            setType(e, new BooleanType());
        }
        private void setString(ExpressionInterface e) {
            setType(e, new StringType());
        }
        private void setType(ExpressionInterface e, Type t) {
            assert e != null && t != null;

            // If t already has a parse range (for example, if it is the
            // type of a member in a structure), it makes no sense to
            // extend it.
            if (t.getParseRange() == ParseRange.EMPTY) t.epr(e);
            t.checkParseRange();
            results.setType(e, t);
        }
        private Type getType(ExpressionInterface e) {
            return results.getType(e);
        }
        private void sameType(ExpressionInterface e, 
                              ExpressionInterface f)
            throws VisitorException {
            setType(e, getType(f));
            setType(f, getType(e));
        }
        private void expectInteger(ExpressionInterface e)
            throws VisitorException {
            final Type ty = getType(e);
            if (!isInteger(ty)) report("int.expected", e, ty);
        }
        private void expectChannel(ExpressionInterface e,
                                   PortDirection dir)
            throws VisitorException {
            final Type t = getType(e);
            if (t instanceof ChannelType) {
                if (dir != null) {
                    final ChannelType ct = (ChannelType) t;
                    if (ct.getDirection() != dir) {
                        report("channel.wrong.direction", e, ct.getDirection(),
                               dir);
                    }
                }
            } else {
                report("channel.expected", e, t);
            }
        }
        // Because there could be multiple structures with the same name, for
        // any StructureType, add additional information on the declaration
        // location, if available
        private Object clarify(Object o) {
            if (o instanceof StructureType) {
                final StructureDeclaration decl =
                    getStructureDecl(null, (StructureType) o);
                o = o.toString() + " (" +
                    (decl == null ? "undeclared"
                                  : decl.getParseRange().fullString()) +
                    ")";
            }
            return o;
        }
        private void report(final String s, final AbstractASTNodeInterface a) {
            report(s, a, null);
        }
        private void report(final String s, final AbstractASTNodeInterface a,
                            final Object o1) {
            report(s, a, o1, null);
        }
        private void report(final String s, final AbstractASTNodeInterface a,
                            final Object o1, final Object o2) {
            report(s, a, o1, o2, null);
        }
        private void report(final String s, final AbstractASTNodeInterface a,
                            final Object o1, final Object o2, final Object o3) {
            results.errors.add(new TypeError("type.checker." + s,
                                             a.getParseRange(),
                                             new Object[] { clarify(o1),
                                                            clarify(o2),
                                                            clarify(o3) }));
        }

        private Interval getInterval(final Type ty) {
            final Interval result;
            if (ty instanceof IntegerType) {
                final IntegerType ity = (IntegerType) ty;
                result = ity.getInterval() == null ? LARGE_INTERVAL
                                                   : ity.getInterval();
            } else if (ty instanceof NodeType) {
                final NodeType nty = (NodeType) ty;
                result = new Interval(nty.getWidth());
            } else if (ty instanceof ChannelType) {
                final ChannelType cty = (ChannelType) ty;
                final BigInteger biggest =
                    cty.getNumValues().subtract(BigInteger.ONE);
                if (biggest.signum() == -1) {
                    result = LARGE_INTERVAL;
                } else {
                    result = new Interval(BigInteger.ZERO, biggest);
                }
            } else {
                result = LARGE_INTERVAL;
            }
            return result;
        }

        private Interval getInterval(final ExpressionInterface ei) {
            final Interval result = getInterval(getType(ei));
            return result;
        }

        private void setInterval(Interval interval, final Type ty) {
            // Saturate to LARGE_INTERVAL so that intervals that became too big
            // due to operations on infinite integers don't propagate
            if (interval == Interval.EXCEPTION) {
                interval = LARGE_INTERVAL;
            } else if (interval != null) {
                interval = new Interval(
                        LARGE_LOWER_BOUND.max(interval.getLeftBound()),
                        LARGE_UPPER_BOUND.min(interval.getRightBound()));
            }

            if (ty instanceof IntegerType) {
                final IntegerType ity = (IntegerType) ty;
                ity.setInterval(interval);
            }
        }

        private void setInterval(final Interval interval,
                                 final ExpressionInterface ei) {
            setInterval(interval, getType(ei));
        }

        /** Assert that two types are compatible.  **/
        private void assertCompatible(Type s, Type t)
            throws VariableAnalyzerVisitorException {
            if(!isCompatible(s,t)) {
                s.checkParseRange();
                t.checkParseRange();
                final ParseRange spr = s.getParseRange();
                final ParseRange tpr = t.getParseRange();
                throw new VariableAnalyzerVisitorException(spr.fullString() + ", " +
                     (spr.start.filename == tpr.start.filename ? tpr.toString()
                                                               : tpr.fullString()) +
                     ": CSP type error: expected "+ s + ", found " + t, t);
            }
        }

        private boolean sameType(StructureType s, StructureType t) {
            final StructureDeclaration sd = getStructureDecl(null, s);
            final StructureDeclaration td = getStructureDecl(null, t);
            if (sd != null && td != null) {
                return sd.getParseRange().equals(td.getParseRange());
            } else {
                return sd == td;
            }
        }

        /** Return a value indicating whether two types are compatible
         * (called by assertCompatible) **/
        private boolean isCompatible(Type s, Type t) {
            if (t != null && s != null) {
                if ((t instanceof BooleanType && !isBoolean(s))
                    || (t instanceof IntegerType && !isInteger(s))
                    || (t instanceof StructureType && !(s instanceof StructureType))
                    || (t instanceof ArrayType && !(s instanceof ArrayType)))
                    return false;
                if (t instanceof ArrayType && s instanceof ArrayType)
                    return isCompatible(((ArrayType) s).getElementType(), 
                                        ((ArrayType) t).getElementType());
                if (t instanceof StructureType && s instanceof StructureType) {
                    return sameType((StructureType) t, (StructureType) s);
                }

            }
            return true;
        }

        /**
         * Return <code>true</code> if <code>rhs</code> is a type that can be
         * assigned to <code>lhs</code>, and <code>false</code> otherwise.
         **/
        private boolean isAssignable(Type lhs, Type rhs) {
            if ((isBoolean(lhs) && isBoolean(rhs)) ||
                (isInteger(lhs) && isInteger(rhs)) ||
                (isString(lhs) && isString(rhs)) ||
                (lhs == ANY) ||
                (rhs == ANY)) {
                return true;
            }
            if (lhs instanceof StructureType && rhs instanceof StructureType) {
                return sameType((StructureType) lhs, (StructureType) rhs);
            }
            if (lhs instanceof ArrayType && rhs instanceof ArrayType) {
                return isAssignable(((ArrayType) lhs).getElementType(), 
                                    ((ArrayType) rhs).getElementType());
            }
            return false;
        }
    }

    private static final class VariableAnalyzerVisitorException
        extends VisitorExceptionWithLocation {
        private VariableAnalyzerVisitorException(String s,
                                                 AbstractASTNodeInterface a) {
            super(s, a);
        }
    }

    private static String getWidthString(final Type ty) {
        final IntegerType it = (IntegerType) ty;
        final BigInteger w = CspUtils.getIntegerConstant(it.getDeclaredWidth());
        return w == null ? "int" : ("int(" + w + ")");
    }
}
