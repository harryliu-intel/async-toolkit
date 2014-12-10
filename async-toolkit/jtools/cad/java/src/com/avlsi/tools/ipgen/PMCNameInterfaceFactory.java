/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.ipgen;


import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;

import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.jauto.CastStat;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;

public class PMCNameInterfaceFactory implements CDLNameInterfaceFactory {

    /* Interface to allow user of this class to seed the set of existing names
       used in each cell to make sure that no generated names in that cells
       collides with existing names in that cell.*/
    public interface ExistingNamesInterface {
        Set getExistingTranslatedNamesForCell( final String cellName );
    }

    private static class NameInterface implements CDLNameInterface {
        private final Map mTransformTransistorModel;
        
        private final Set mExistingTranslatedNames;

        private final Map mGeneratedNames;
        
        private final Set mDynamicNodeNames;
        
        public NameInterface( final Set dynamicNodeNames,
                              final Set existingTranslatedNames ) {
            mTransformTransistorModel = new HashMap();
            
            mTransformTransistorModel.put( "P", "p" );
            mTransformTransistorModel.put( "N", "n" );
            
            mExistingTranslatedNames = new HashSet( existingTranslatedNames );

            mGeneratedNames = new HashMap();

            mDynamicNodeNames = dynamicNodeNames;
            
            mGeneratedNames.put( "GND", "gnd" );
            mGeneratedNames.put( "Vdd", "vdd" );

        
        }
        
        public String translateName(String name ) 
            throws CDLRenameException 
        {
            final String existingGeneratedName = ( String ) mGeneratedNames.get( name );

            if ( existingGeneratedName == null ) {

                final StringBuffer sb = new StringBuffer();
            
                final int nameLength = name.length();
            
                int i = 0;
                while ( i < nameLength ) {
                    final char c = name.charAt(i);
                    
                    switch (c) {
                    case '.':
                    case ',':
                    case '(':
                    case ')':
                    case '{':
                    case '}':
                    case '-':
                    case '_': 
                        sb.append('_'); 
                        break;
                    default:
                        if (Character.isLetterOrDigit(c)) {
                            if ( Character.isLetter( c ) ) {
                                sb.append( Character.toLowerCase( c ) );
                            }
                            else {
                                sb.append(c);
                            }
                        }
                        else {
                            sb.append( '_' );
                        }
                    }
                    ++i;
                }
                
                final String translatedString = sb.toString();
                String translatedStringWithCounter = translatedString;
                int counter = 0;
                
                while ( mExistingTranslatedNames.contains( translatedStringWithCounter ) ) {
                    translatedStringWithCounter = translatedString + "_" + Integer.toString( counter );
                    ++counter;
                }
                
                final String ret = "f" + translatedStringWithCounter + "m" ;
                mExistingTranslatedNames.add( translatedStringWithCounter );
                
                mGeneratedNames.put( name, ret );
                
                return ret ;
            }
            else {
                return existingGeneratedName ;
            }
        }
        
        public String renameCell(String name ) 
            throws CDLRenameException 
        {
            return translateName( name );
        }
        
        public String renameNode( final String oldNodeName ) 
            throws CDLRenameException
        {
            
            final String translatedNodeName = translateName( oldNodeName );
            
            if ( mDynamicNodeNames.contains( oldNodeName ) ) {
                return "zz" + translatedNodeName;
            }
            else {
                return translatedNodeName;
            }
        }
        
        public String renameDevice( final String oldDeviceName ) 
            throws CDLRenameException 
        {
            return translateName( oldDeviceName );
        }
        
        public String renameSubCellInstance( final String oldInstanceName ) 
            throws CDLRenameException 
        {
            return translateName( oldInstanceName );
        }
        
        public String renameTransistorModel( final String oldTransistorModel )
        {
            
            final String translatedModel = ( String ) mTransformTransistorModel.get( oldTransistorModel );
            
            if ( translatedModel == null ) {
                return oldTransistorModel;
            }
            else {
                return translatedModel;
            }
        }   
    }


    private final CastFileParser mCastParser;
    private final Cadencize mCadencizer;
    private final ExistingNamesInterface mExistingNamesInterface;
    private final Map mDynamicNodesMap;
    private final Map mDynamicPortNodesMap;

    public PMCNameInterfaceFactory( final CastFileParser castParser,
                                    final Cadencize cadencizer,
                                    final ExistingNamesInterface existingNamesInterface ) {
        mCastParser = castParser;
        mCadencizer = cadencizer;
        mExistingNamesInterface = existingNamesInterface;
        mDynamicNodesMap = new HashMap();
        mDynamicPortNodesMap = new HashMap();
    }
    
    private Set getDynamicPortNodes( final CellInterface cell ) throws InvalidHierNameException {
        return CastStat.getDynamicPortNodes(cell, mCadencizer, mCastParser,
                                            mDynamicPortNodesMap,
                                            mDynamicNodesMap);
    }

    private Set getDynamicNodes( final CellInterface cell ) throws InvalidHierNameException {
        return CastStat.getDynamicNodes(cell, mCadencizer, mCastParser,
                                        mDynamicPortNodesMap, mDynamicNodesMap);
    }

    public CDLNameInterface getNameInterface( final String cellName ) 
        throws CDLRenameException
    {
        try {
            final CellInterface cell = mCastParser.getFullyQualifiedCell( cellName );
            final Set dynamicNodes = ((Boolean) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.NETLIST_PRIMITIVE)).booleanValue() ?
                Collections.EMPTY_SET :
                getDynamicNodes( cell );
            /*
              Get all the names that have already been used in the current cell.
             */
            final Set existingNames = 
                mExistingNamesInterface.getExistingTranslatedNamesForCell( cellName );
            return new NameInterface( dynamicNodes, existingNames );
        }
        catch ( CastSemanticException e ) {
            throw new CDLRenameException( "Unable to get \"" + cellName + "\".", e );
        }
        catch ( InvalidHierNameException e ) {
            throw new CDLRenameException( "Unable to get \"" + cellName + "\".", e );
        }
    }
}
