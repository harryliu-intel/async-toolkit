/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast;

import java.util.HashMap;
import java.util.Map;
import java.io.IOException;

import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.io.SearchPath;

/**
 * FREDERIK-UNDOCUMENTED
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class CastCacheManager {
    private static CastCacheManager def = null;
    private CastFileParser appointed = null;

    /** cache can be a HashMap for the nonce, but we will want to use
     * a specialized class for this data if it gets more
     * complicated. **/
    HashMap cache;

    HashMap getData() {
        if(cache == null)
            cache = new HashMap();
        return cache;
    }

    public static CastCacheManager getDefault() {
        if (def == null) def = new CastCacheManager();
        return def;
    }
    
    /**
     * Return a cast file parser, using our common cache (if it
     * exists). 
     *
     * We could add more methods corresponding to other CastFileParser
     * constructors, or to create CastParserEnvironment, but this is
     * all that is currently needed.
     **/
    public CastFileParser getCastFileParser(final SearchPath castPath,
                                            String castVersion,
                                            boolean verbose) 
            throws CastSemanticException, CastSyntaxException,
            IOException {
        return appointed == null ?
            new CastFileParser(castPath, castVersion, verbose, getData())
          : appointed;
    }
    
    public CastFileParser getCastFileParser(final SearchPath castPath,
                                            String castVersion,
                                            CastParsingOption opt) 
            throws CastSemanticException, CastSyntaxException,
            IOException {
        return getCastFileParser(castPath, castVersion, false, opt);
    }

    public CastFileParser getCastFileParser(final SearchPath castPath,
                                            String castVersion,
                                            boolean verbose,
                                            CastParsingOption opt) 
            throws CastSemanticException, CastSyntaxException,
            IOException {
        return appointed == null ?
            new CastFileParser(castPath, castVersion, verbose, getData(), opt)
          : appointed;
    }

    public void setCastFileParser(final CastFileParser appointed) {
        this.appointed = appointed;
    }
    
    boolean neverClearCache = false;

    
    /** 
     * Clear any caches. Used to save memory after parsing is done.
     *
     * @see com.avlsi.tools.jauto.Jauto
     **/
    public void clearCache() {
        if(!neverClearCache && cache != null) {
            synchronized(cache) {
                cache.clear();
            }
        }
    }

    /**
     * Overrides the effects of clearCache. This is used when a tool
     * which would normally clear the cast cache once or more in a
     * single run, is being called multiple times from some meta-tool
     * which desires to keep the cache intact for efficiency. This is
     * not normally called by most tools. See JobServer for an
     * example usage.
     **/
    public void setNeverClear(boolean b) {
        neverClearCache = b;
    }
}
