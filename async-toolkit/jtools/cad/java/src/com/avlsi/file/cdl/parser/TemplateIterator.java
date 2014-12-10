/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.parser;


import java.util.NoSuchElementException;

import com.avlsi.file.cdl.parser.Template;

public interface TemplateIterator {

    boolean hasNext();

    Template next() throws NoSuchElementException;

}
