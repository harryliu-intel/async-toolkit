/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

/**
 * CoSimChannelArrays are (possibly multi-dimensional) arrays of
 * ChannelInputs or ChannelOutputs produced by the cosim framework for
 * javablock classes.  People writing javablocks should always look
 * for a CoSimChannelInputArray or a CoSimChannelOutputArray, for
 * better typing.  This interface is for the use of the framework.
 **/
interface CoSimChannelArrayInterface { }
