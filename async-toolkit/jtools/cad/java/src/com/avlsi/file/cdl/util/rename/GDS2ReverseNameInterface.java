/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

/**
 * Implements the reverse of GDS2NameInterface.  However, due to the way
 * GDS2NameInterface is defined, it is impossible to completely reverse the
 * translation.  However, restrictions on CAST will not allow (hopefully) those
 * inconsistencies to be triggered.
 **/
public class GDS2ReverseNameInterface implements CDLNameInterface {

    //If on, then foo_1 -> foo_1 instead of failing
    //and _XXX_ -> _XXX_ if it fails other translations
    private final boolean bLenient;

    public GDS2ReverseNameInterface() {
        this(true);
    }

    public GDS2ReverseNameInterface(final boolean bLenient) {
        this.bLenient = bLenient;
    }

    public String renameCell(String name ) 
    throws CDLRenameException 
    {
        return translate( name );
    }

    public String renameNode( final String oldNodeName ) 
        throws CDLRenameException
    {
        return translate( oldNodeName );
    }

    public String renameDevice( final String oldDeviceName ) 
        throws CDLRenameException 
    {
        return translate( oldDeviceName );
    }

    public String renameSubCellInstance( final String oldInstanceName ) 
        throws CDLRenameException 
    {
        return translate( oldInstanceName );
    }

    public String renameTransistorModel( final String oldTransistorModel )
    {
        return oldTransistorModel;
    }

    private char translateEscaped( final String str )
        throws CDLRenameException, NumberFormatException  {
        switch ( str.length() ) {
        case 1:            
            final char c = str.charAt(0);
            switch( c ) {
            case 'D':
                return '.';
            case 'C':
                return ',';
            case 'l':
                return '[';
            case 'r':
                return ']';
            case 'L':
                return '(';
            case 'R':
                return ')';
            case 'M':
                return '-';
            case 'U':
                return '_';
            case 'H':
                return '#';
            default:
                throw new CDLRenameException( "_" + c + "_ is and invalid escape construct" );
            }
        default:
            int charValue = Integer.parseInt( str, 16 );
            return (char) charValue;
        }
    }
    

    private String translate( final String s ) 
    throws CDLRenameException 
    {
        final StringBuffer sb = new StringBuffer();
        for (int i = 0; i < s.length(); i++) {
            final char c = s.charAt(i);
            if(c == '_' ) {
                final StringBuffer usb = new StringBuffer();
                boolean closed = false;
                while( ++i < s.length()) {
                    final char uc;                                    
                    uc = s.charAt(i);
                    if( uc == '_' ) {
                        closed = true;
                        break;
                    }
                    usb.append(uc);
                }
                if(closed || !bLenient ) {
                    final String untranslated = new String(usb);
                    try {
                        final char translated = translateEscaped( untranslated );
                        sb.append( translated );
                    }
                    catch(CDLRenameException e ) {
                        if(!bLenient) { throw e; } else { sb.append( "_" + untranslated ); i--; };
                    }
                    catch(NumberFormatException e ) {
                        if(!bLenient) { throw e; } else { sb.append( "_" + untranslated ); i--; };
                    }
                }
                else {
                    sb.append('_');
                    sb.append(usb);
                }
            }
            else
                sb.append(c);
        }
        return sb.toString();
    }
}
