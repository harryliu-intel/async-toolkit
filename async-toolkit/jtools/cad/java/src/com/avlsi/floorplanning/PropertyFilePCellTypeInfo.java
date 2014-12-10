/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.floorplanning;

import com.avlsi.floorplanning.PCellTypeInfo;

import java.io.InputStream;
import java.io.IOException;

import java.util.Properties;

class PropertyFilePCellTypeInfo implements PCellTypeInfo {

    private final Properties info;

    public PropertyFilePCellTypeInfo( final Properties p ) {
        info = p;
    }

    public PropertyFilePCellTypeInfo( final InputStream input ) 
        throws IOException
    {
        info = new Properties();
        info.load( input );
    }

    protected final String get( final String key ) {
        return info.getProperty( key );
    }

    public String getLibName( ) {
        return get( "library" );
    }

    public String getCellName( ) {
        return get( "cell" );
    }

    public String getLayoutViewName( ) {
        return get( "layout" );
    }

    public String getSchematicViewName( ) {
        return get( "schematic" );
    }

    public String getParamType( final String name ) {
        return get( name );
    }

}
