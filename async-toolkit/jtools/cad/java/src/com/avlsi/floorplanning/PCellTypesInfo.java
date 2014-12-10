/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.floorplanning;

import java.util.Iterator;

import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.floorplanning.PCellTypeInfo;
import com.avlsi.floorplanning.TransistorTypeInfo;


interface PCellTypesInfo {

    PCellTypeInfo getPCellTypeInfo( final String cellName );

    TransistorTypeInfo getTransistorInfo( final String modelName );

    StringContainerIterator getTransistorModels();

    Iterator getAllPCellTypeInfos();

    Iterator getNonTransistorPCellTypeInfos();

}
