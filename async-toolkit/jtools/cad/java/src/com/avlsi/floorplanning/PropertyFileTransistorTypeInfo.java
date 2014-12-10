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

class PropertyFileTransistorTypeInfo extends PropertyFilePCellTypeInfo
    implements TransistorTypeInfo
    
{

    
    public PropertyFileTransistorTypeInfo( final Properties p ) {
        super( p );
    }

    public PropertyFileTransistorTypeInfo( final InputStream input ) 
        throws IOException
    {
        super( input );
    }

    public String getModelName() {
        return get( "model" );
    }
    
    public String getGateTerminalName() {
        return get( "gate" );
    }
    
    public String getSourceTerminalName() {
        return get( "source" );
    }
    
    public String getDrainTerminalName() {
        return get( "drain" );
    }

    public String getBulkTerminalName() {
        return get( "bulk" );
    }

    public static boolean isTransistorInfo( Properties info ) {
        return info.getProperty( "model" ) != null;
    }

}
