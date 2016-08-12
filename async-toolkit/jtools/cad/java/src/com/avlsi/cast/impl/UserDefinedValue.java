/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast.impl;

import java.util.HashMap;
import java.util.Map;

import antlr.collections.AST;

import com.avlsi.cell.CellImpl;
import com.avlsi.util.container.Pair;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.debug.Debug;
import com.avlsi.file.common.HierName;
import com.avlsi.util.text.StringUtil;

/**
 * This class contains information <code>CastTwoTree</code> needs to
 * generate a <code>CellImpl</code> for types defined in CAST files by
 * <code>define</code> or <code>defchan</code>.  This information is:
 * <ul>
 *   <li> The environment in place before the type definition was parsed
 *   <li> The AST for the meta parameter list
 *   <li> The AST for the port parameter list
 *   <li> The AST for the implied port parameter list
 *   <li> The AST for the refinement parent
 *   <li> The AST for the body of the type definition
 *   <li> The module in which the type was declared
 *   <li> A flag indicating which of the following keywords was used
 *        to define the type: <code>define</code> or <code>defchan</code>
 * </ul>
 *
 * <p> For efficency, this class caches (in the {@link #cellMap}) the
 * <code>CellImpl</code> corresponding to each combination of
 * metaparameters.  Similiar maps are kept for the tree parser's
 * use ({@link aliasesMap}, {@link cellConstantsMap}, {@link prsEnvMap}
 * and {@link subcellsEnvMap}).  To explain that would be to explain
 * <code>CastTwoTree.g</code>, and that is beyond the scope of this comment.
 * All these maps map from a {@link TupleValue} of the metaparameter values
 * to whatever the map is mapping to.
 *
 * <p> Note that this class extends {@link Value}.  This was done
 * so the CAST implementation could treat types somewhat like first
 * class objects, even though types are not first class objects in
 * the CAST language.  This may have been a good decision.  This may
 * have been a bad decision.  Pointless arguments about this decision
 * may ensue.
 *
 * <p> This class is called <code>UserDefinedValue</code> and
 * represents the type definitions of user defined types (those
 * defined with <code>define</code> or <code>defchan</code>).  Do not 
 * confuse it with {@link InstanceValue}, which represents instances
 * of these types.  <code>UserDefinedValue</code> was chosen over
 * something like <code>CellValue</code> because in the future
 * additional user defined types may be added, perhaps functions.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class UserDefinedValue extends Value {

    private final Environment env;
    private final AST metaParamList;
    private       AST portParamList;
    private       AST impliedPortParamList;
    private       AST envExtraParamList;
    private final AST inheritanceList;
    private final AST refinementParent;
    private       AST body;
    private final String moduleName;
    /** Whether it's a cell, attributes cell, alias cell, or a channel **/
    private final int structureType;
    /** Whether it's a named environment **/
    private final boolean isEnv;

    // cellMap, aliasesMap, cellConstantsMap, prsEnvMap and subcellsEnvMap
    // are conceptually very similar, but accessed at different points
    // in the parsing.

    /**
     * Precomputed cells for this UDV and specific metaparameters.
     * TupleValue (metaparameters) -&gt; CellImpl.
     **/
    private final Map cellMap;

    /**
     * Precomputed aliases this channel should export.  TupleValue
     * (metaparameters) -&gt; LocalEnvironment
     **/
    private final Map aliasesMap;

    /**
     * Precomputed top-level environments for this UDV and specific
     * metaparameters.  These contain the ints/bools/floats defined at
     * the top-level of the cell.  This map exists to allow these
     * constants to be passed along via refinement.
     *
     * TupleValue (metaparameters) -&gt; LocalEnvironment.
     **/
    private final Map cellConstantsMap;

    /**
     * Environment containing nodes and cells declared in the prs block.
     * This map exists to allow these constants to be passed along via
     * refinement.
     *
     * TupleValue (metaparameters) -&gt; Environment.
     **/
    private final Map prsEnvMap;

    /**
     * Environment containing nodes and cells declared in the subcells
     * or subtypes block.  This map exists to allow these constants to
     * be passed along via refinement.
     *
     * TupleValue (metaparameters) -&gt; Environment.
     **/
    private final Map subcellsEnvMap;

    /**
     * Environment containing metaparameters, and imported cells. This map
     * exists to allow refinement children to omit the port list.
     *
     * TupleValue (metaparameters) -&gt; Environment.
     **/
    private final Map portEnvMap;

    private boolean portListInherited;

    /** Symbol for the cell type. **/
    private final Symbol cellTypeSymbol;

    /**
     * The cell containing this cell, if this cell is an environment; otherwise
     * <code>null</code>.
     **/
    private UserDefinedValue environmentContainer;

    /** Available structureTypes **/
    public static final int CELL = 0, ATTRIBUTES_CELL = 1, CHANNEL = 2,
                            ALIAS_CELL = 3;

    public UserDefinedValue(
            final Environment env,
            final Symbol cellTypeSymbol,
            final AST metaParamList,
            final AST portParamList,
            final AST impliedPortParamList,
            final AST body)
    {
        this(env, cellTypeSymbol, metaParamList, portParamList, impliedPortParamList, null, null, null, body, null, CELL, false);
    }

    public UserDefinedValue(
            final Environment env,
            final Symbol cellTypeSymbol,
            final AST metaParamList,
            final AST portParamList,
            final AST impliedPortParamList,
            final AST envExtraParamList,
            final AST inheritanceList,
            final AST refinementParent,
            final AST body,
            final String moduleName,
            final int structureType,
            final boolean isEnv)
    {
        super(true);

        this.env = env;
        this.cellTypeSymbol = cellTypeSymbol;
        this.metaParamList = metaParamList;
        this.portParamList = portParamList;
        this.impliedPortParamList = impliedPortParamList;
        this.envExtraParamList = envExtraParamList;
        this.inheritanceList = inheritanceList;
        this.refinementParent = refinementParent;
        this.body = body;
        this.moduleName = moduleName;
        this.cellMap = new HashMap();
        this.aliasesMap = new HashMap();
        this.cellConstantsMap = new HashMap();
        this.prsEnvMap = new HashMap();
        this.subcellsEnvMap = new HashMap();
        this.portEnvMap = new HashMap();
        this.portListInherited = false;
        Debug.assertTrue((structureType == CELL) ||
                         (structureType == ATTRIBUTES_CELL) || 
                         (structureType == CHANNEL) ||
                         (structureType == ALIAS_CELL),
                     "UserDefinedValue given bad structure type: " + structureType);
        this.structureType = structureType;
        this.isEnv = isEnv;
    }

    public Value assign(final Value v, final CellImpl cell)
        throws InvalidOperationException {
        throw new InvalidOperationException("can't assign types!");
    }

    public Value duplicate() {
        throw new AssertionFailure("can't dup types!");
    }

    //
    // accessors 
    //

    public Environment getEnvironment() {
        return env;
    }

    public AST getMetaParamList() {
        return metaParamList;
    }

    public boolean hasMetaParams() {
        return metaParamList != null && metaParamList.getFirstChild() != null;
    }

    public AST getPortParamList() {
        return portParamList;
    }

    /* The port list of a refinement child may be empty to mean it has the same
     * ports as the parent.  This lets us update the port list accordinly. */
    public void updatePortParamList(AST inherited) {
        portListInherited = true;
        portParamList = inherited;
    }

    public boolean isPortListInherited() {
        return portListInherited;
    }

    public AST getImpliedPortParamList() {
        return impliedPortParamList;
    }

    /** To let refinement change the implied ports. **/
    public void updateImpliedPortParamList(AST inherited) {
        impliedPortParamList = inherited;
    }

    public AST getEnvExtraParamList() {
        return envExtraParamList;
    }

    /** To let refinement change the extra ports. **/
    public void updateEnvExtraParamList(AST inherited) {
        envExtraParamList = inherited;
    }

    public AST getInheritanceList() {
        return inheritanceList;
    }

    public AST getRefinementParent() {
        return refinementParent;
    }

    public AST getBody() {
        return body;
    }

    public void clearBody() {
        body = null;
    }

    public String getModuleName() {
        return moduleName;
    }

    public String getCellTypeName() {
        return cellTypeSymbol.getString();
    }

    public String getFullyQualifiedType() {
        if (moduleName == null)
            return getCellTypeName();
        else
            return moduleName + '.' + getCellTypeName();
    }

    public int getStructureType() {
        return structureType;
    }

    public Type getType() throws InvalidOperationException {
        throw new InvalidOperationException ("not yet implemented");
    }

    public String toString() {
        return "UserDefinedValue()";
    }

    //
    // Cell stuff
    //

    /**
     * Add an instantiation of the cell for the given meta-parameters.
     **/
    public void putCell(final TupleValue metaParams, final CellImpl cell) {
        cellMap.put(metaParams, cell);
    }

    /**
     * Get the instantiation of the cell for the given meta-parameters.
     **/
    public CellImpl getCell(final TupleValue metaParams) {
        return (CellImpl) cellMap.get(metaParams);
    }

    /**
     * Add aliases for the given metaparameters (only relevant to channels)
     **/
    public void putAliases(final TupleValue metaParams,
                           final LocalEnvironment aliases) {
        aliasesMap.put(metaParams, aliases);
    }

    /**
     * Get aliases for the given metaparameters (only relevant to channels)
     **/
    public LocalEnvironment getAliases(final TupleValue metaParams) {
        return (LocalEnvironment) aliasesMap.get(metaParams);
    }

    /**
     * Take ints/bools/floats from the top-level of the cell for the
     * given metaparameters and add them to the internal cache.
     **/
    public void putCellConstants(final TupleValue metaParams,
                                 final Environment cellConstants) {
        cellConstantsMap.put(metaParams, cellConstants);
    }

    /**
     * Return cell-top-level ints/bools/floats for the given
     * metaparameters.
     **/
    public Environment getCellConstants(final TupleValue metaParams) {
        return (Environment) cellConstantsMap.get(metaParams);
    }

    /**
     * Take nodes/cells defined in the prs block and add them to the
     * internal cache for use in refining cells.
     **/
    public void putPrsEnvironment(final TupleValue metaParams,
                                  final Environment prsEnv) {
        prsEnvMap.put(metaParams, prsEnv);
    }

    /**
     * Return nodes/cells defined in the prs block.  Needed for refining
     * cells.
     **/
    public Environment getPrsEnvironment(final TupleValue metaParams) {
        return (Environment) prsEnvMap.get(metaParams);
    }

    /**
     * Take nodes/cells defined in the subcells or subtypes block and add
     * them to the internal cache for use in refining cells.
     **/
    public void putSubcellsEnvironment(final TupleValue metaParams,
                                       final Environment subcellsEnv) {
        subcellsEnvMap.put(metaParams, subcellsEnv);
    }

    /**
     * Return nodes/cells defined in the subcells or subtypes block.  
     * Needed for refining cells.
     **/
    public Environment getSubcellsEnvironment(final TupleValue metaParams) {
        return (Environment) subcellsEnvMap.get(metaParams);
    }

    public void putPortEnvironment(final TupleValue metaParams,
                                   final Environment portListEnv) {
        portEnvMap.put(metaParams, portListEnv);
    }

    public Environment getPortEnvironment(final TupleValue metaParams) {
        return (Environment) portEnvMap.get(metaParams);
    }

    /**
     * Returns whether it's a named environment.
     **/
    public boolean isNamedEnvironment() {
        return isEnv;
    }

    /**
     * Return the cell where this cell is an environment, or <code>null</code>
     * if this cell is not an environment.
     **/
    public UserDefinedValue getEnvironmentContainer() {
        return environmentContainer;
    }

    /**
     * Set the cell where this cell is an environment.
     **/
    public void updateEnvironmentContainer(final UserDefinedValue cell) {
        assert isEnv : "setting " + cell.getFullyQualifiedType() +
                       " as environment container of non-named environment " +
                       getFullyQualifiedType();
        environmentContainer = cell;
    }

    /**
     * Returns a string representation of the name with meta parameters.
     * This is what we should expect the .mag file to be called.
     * Ie "SLACK_1of2(2)"
     **/
    public static String getTypeName(final String baseName,
                                     final TupleValue metaParams) {
        if (metaParams.getSize() == 0)
            return baseName;
        else {
            final StringBuffer sb = new StringBuffer(baseName);

            sb.append('(');
            try {
                for (int i = 0; i < metaParams.getSize(); ++i) {
                    final Value v = metaParams.accessTuple(i);

                    if (i > 0)
                        sb.append(",");

                    // code similiar to ArrayValue.getMetaParamString
                    if (v instanceof IntValue)
                        sb.append(((IntValue) v).getValue());
                    else if (v instanceof BoolValue)
                        sb.append(((BoolValue) v).getValue());
                    else if (v instanceof ArrayValue)
                        sb.append(((ArrayValue) v).getMetaParamString());
                    else
                        throw new AssertionFailure("bad meta-param type");
                }
            } catch (InvalidOperationException e) {
                throw (AssertionFailure)
                    new AssertionFailure("invalid operation?"
                        + " can't happen").initCause(e);
            }
            sb.append(')');

            return sb.toString();
        }
    }
}
