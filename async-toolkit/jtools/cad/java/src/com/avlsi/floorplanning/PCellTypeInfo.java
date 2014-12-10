/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.floorplanning;


interface PCellTypeInfo {
    
    String getLibName( );
    String getCellName( );
    String getLayoutViewName( );
    String getSchematicViewName();

    String getParamType( final String name );

}
