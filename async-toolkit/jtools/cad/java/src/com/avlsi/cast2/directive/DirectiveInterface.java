/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive;

import java.util.Map;
import java.util.Iterator;

import com.avlsi.cast2.directive.UnknownDirectiveException;

/**
   API between directives blocks and their consumers.
 */
public interface DirectiveInterface {
    /**
       Get the default value for a parameterized directive.
       @param key Name of a directive
       @param memberType Type of parameter
       @return The default value for the specified directive, or
       <code>null</code> if there is no default value.
       @throws UnknownDirectiveException if the key is not a valid directive
     */
    Object getDefaultValue(String key, String memberType) throws UnknownDirectiveException;

    /**
       Get non default values specified for a parameterized directive.
       @param key Name of a directive
       @param memberType Type of parameter
       @return A map, possibly empty, from <code>String</code> to
       <code>Value</code>.  The keys of the map are the parameters.
       @throws UnknownDirectiveException if the key is not a valid directive
     */
    Map getValues(String key, String memberType) throws UnknownDirectiveException;

    /**
       Get the value for a non-parameterized directive.
       @param key Name of a directive
       @return Value for the specified directive.
       @throws UnknownDirectiveException if the key is not a valid directive
     */
    Object lookup(String key) throws UnknownDirectiveException;

    /**
       Returns whether a non-parameterized directive has been assigned a value.
       This information is not available from lookup, which always returns a
       value, regardless if a directive is actually present.  The only user of
       this call should be MergeDirective, which is called on refinement.
       @param key Name of a directive
       @return True if a directive statement assigns a value to this directive;
       false otherwise.
       @throws UnknownDirectiveException if the key is not a valid directive
     */
    boolean containsDirective(String key) throws UnknownDirectiveException;

    /**
       Get the value for a parameterized directive.  Returns the default value
       if no value specified for the parameter.
       @param key Name of a directive
       @param memberType Type of the parameter
       @param parameter The parameter as a <code>String</code>
       @return Value for the specified parameterized directive.
       @throws UnknownDirectiveException if the key is not a valid directive
     */
    Object lookup(String key, String memberType, Object parameter) throws UnknownDirectiveException;

    /**
       Returns whether the specified key is valid.
       @param key Name of a directive
       @return <code>true</code> if key is a valid directive,
       <code>false</code> otherwise.
     */
    boolean isKey(String key);

    /**
       Returns whether the specified key/parameter type combination is valid.
       @param key Name of a directive
       @param memberType Type of the parameter
       @return <code>true</code> if key/parameter type is a valid directive,
       <code>false</code> otherwise.
     */
    boolean isKey(String key, String memberType);

    /**
       Returns an Iterator over entries of parametrized directives.
       The values in the entries are maps from parameter strings to values
     */
    Iterator paramEntryIterator();

    /**
       Returns an Iterator over entries of nonParametrized directives.    
    */
    Iterator noparamEntryIterator();
}
