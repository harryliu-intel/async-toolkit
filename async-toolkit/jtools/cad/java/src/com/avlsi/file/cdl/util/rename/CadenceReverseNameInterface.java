/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

/**
 * Implements the reverse of CadenceNameInterface.  However, due to the way
 * CadenceNameInterface is defined, it is impossible to completely reverse the
 * translation.  However, restrictions on CAST will not allow (hopefully) those
 * inconsistencies to be triggered.
 **/
public class CadenceReverseNameInterface implements CDLNameInterface {

    int curlyCount = 0;

    public CadenceReverseNameInterface() {
    }

    private char left() {
        char result;
        if (curlyCount == 0) {
            result = '(';
        } else {
            result = '{';
        }
        ++curlyCount;
        return result;
    }

    private char right() throws CDLRenameException {
        char result;
        if (curlyCount <= 0) {
            throw new CDLRenameException( "Unmatched parenthesis!" );
        } else if (curlyCount == 1) {
            result = ')';
        } else {
            result = '}';
        }
        --curlyCount;
        return result;
    }

    public String renameCell(String name ) 
    throws CDLRenameException 
    {
        boolean badName = false;
        final StringBuffer sb = new StringBuffer();
        curlyCount = 0;

        final int nameLength = name.length();

        int i = 0;
        while ( ( i < nameLength ) &&
                ( ! badName ) ) {
            final char c = name.charAt(i);
              
            switch (c) {
            case '$':
                sb.append('$'); 
                break;
            case '.':
                badName = curlyCount != 0;
                sb.append('.'); 
                break;
            case '-':
                if (i + 1 < nameLength) {
                    char d = name.charAt(i + 1);
                    if (d == 'L') {
                        ++i;
                        sb.append(left());
                    } else if (d == 'R') {
                        ++i;
                        sb.append(right());
                    } else {
                        sb.append('-');
                    }
                } else {
                    sb.append('-');
                }
                break;
            case '_': 
                if (curlyCount > 0) {
                    sb.append(',');
                } else {
                    sb.append('_'); 
                }
                break;
            default:
                if (Character.isLetterOrDigit(c)) {
                    sb.append(c);
                }
                else {
                    final String errorMessage =
                        "Unable to translate '" +
                        Character.toString(c) +
                        "' (0x" +
                        Integer.toString( ( int ) c, 16 ) +
                        ")";
                    throw new CDLRenameException( errorMessage );
                }
            }
            ++i;
        }
        if ( badName || curlyCount != 0 ) {
            final String errorMessage =
                "\"" + name + "\" is an invalid cell name.";
            throw new CDLRenameException( errorMessage );
        }
        return sb.toString();
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

    private String translate( final String s ) 
    throws CDLRenameException 
    {
        final StringBuffer sb = new StringBuffer();
        for (int i = 0; i < s.length(); i++) {
            final char c = s.charAt(i);
            switch (c) {
            case '!':
                sb.append('!');
                break;
            case '.':
                sb.append('.'); 
                break;
            case '[':
                sb.append('['); 
                break;
            case ']':
                if (i + 1 < s.length() && s.charAt(i + 1) == '[') {
                    sb.append(',');
                    i++;
                } else {
                    sb.append(']');
                }
                break;
            case '(':
                sb.append('(');
                break;
            case ')':
                sb.append(')');
                break;
            case '-':
                if (i + 1 < s.length() && s.charAt(i + 1) == 'H') {
                    sb.append('#');
                    i++;
                } else {
                    sb.append('-'); 
                }
                break;
            case '_': 
                sb.append('_'); 
                break;
            default:
                if (Character.isLetterOrDigit(c)) {
                    sb.append(c);
                } 
                else {
                    final String errorMessage =
                        "Unable to translate '" +
                        Character.toString(c) +
                        "' (0x" +
                        Integer.toString( ( int ) c, 16 ) +
                        ")";
                    throw new CDLRenameException( errorMessage );
                }
            }
        }
        return sb.toString();
    }
}
