/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;
import java.util.HashSet;
import java.util.Set;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;


public class CadenceNameInterface implements CDLNameInterface {
    private final Set transformTransistorModel;
    //If true '$' will be translated to '-D', otherwise throw
    //when we are asked to translate a name containing a '$'.
    private final boolean mTranslateDollar;

    public CadenceNameInterface() {
        mTranslateDollar=false;
        transformTransistorModel = new HashSet();
        transformTransistorModel.add("p");
        transformTransistorModel.add("P");
        transformTransistorModel.add("n");
        transformTransistorModel.add("N");
    }

    public CadenceNameInterface( final boolean translateDollar ) {
        mTranslateDollar = translateDollar;
        transformTransistorModel = new HashSet();
        transformTransistorModel.add("p");
        transformTransistorModel.add("P");
        transformTransistorModel.add("n");
        transformTransistorModel.add("N");
    }

    public CadenceNameInterface(final Set transformTransistorModel) {
        this.transformTransistorModel = transformTransistorModel;
        mTranslateDollar=false;
    }

    public CadenceNameInterface(final Set transformTransistorModel,
                                final boolean translateDollar ) {
        mTranslateDollar = translateDollar;
        this.transformTransistorModel = transformTransistorModel;
    }

    public String renameCell(String name ) 
        throws CDLRenameException 
    {
        boolean badName = false;
        final StringBuffer sb = new StringBuffer();
        boolean inParens = false;    
        int curlyCount = 0;

        final int nameLength = name.length();

        int i = 0;
        while ( ( i < nameLength ) &&
                ( ! badName ) ) {
            final char c = name.charAt(i);
              
            switch (c) {
            case '.':
                badName = inParens;
                sb.append('.'); 
                break;
            case ',':
                badName = !inParens;
                sb.append("_"); 
                break;
            case '(':
                inParens = true;
                sb.append("-L");
                break;
           case ')':
                badName = !inParens;
                inParens = false;
                sb.append("-R");
                break;
            case '{':
                badName = !inParens;
                ++curlyCount;
                sb.append("-L");
                break;
            case '}':
                badName = ! ( inParens && ( curlyCount > 0 ) );
                --curlyCount;
                sb.append("-R");
                break;
            case '-':
                sb.append('-');
                badName = ( ( ! inParens ) ||
                            ( i >= ( nameLength - 1 ) ) ||
                            ( ! ( Character.isDigit( name.charAt( i + 1 ) ) ) ) );
                break;
            case '_': 
                sb.append('_'); 
                break;
            default:
                if (Character.isLetterOrDigit(c)) {
                    sb.append(c);
                }
                else {
                    if ( ( mTranslateDollar ) && ( c =='$' ) ) {
                        sb.append("-D");
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
            ++i;
        }
        badName = badName || inParens || ( curlyCount != 0 ) ;
        if ( badName ) {
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


        boolean badName = false;
        final StringBuffer sb = new StringBuffer();
        boolean inParens = false;
        boolean inBrackets = false;

        final int stringLength = s.length();

        int i = 0;
        while ( ( i < stringLength ) &&
                ( ! badName ) ) {
            final char c = s.charAt(i);
              
            switch (c) {
            case '!':
                sb.append('!');
                break;
            case '.':
                sb.append('.'); 
                break;
            case ',':
                badName = ( ! inBrackets ) || inParens ;
                sb.append("][");
                break;
            case '[':
                badName = inBrackets | inParens ;
                inBrackets = true;
                sb.append('['); 
                break;
            case ']':
                badName = ( ! inBrackets ) || inParens ;
                inBrackets = false;
                sb.append(']');
                break;
                /* case '(':
                badName = inBrackets || inParens ;
                inParens = true;
                sb.append('(');
                break;
            case ')':
                badName = inBrackets || ( ! inParens ) ;
                inParens = false;
                sb.append(')');
                break; */
            case '-':
                badName = ! ( inBrackets || inParens );
                sb.append('-'); 
                break;
            case '_': 
                sb.append('_'); 
                break;
            case '#':
                badName = inBrackets || inParens ;
                sb.append("-H");
                break;
            case '+':
                sb.append( '+' );
                break;
            default:
                if (Character.isLetterOrDigit(c)) {
                    sb.append(c);
                }
                else {
                    if ( ( mTranslateDollar ) && ( c =='$' ) ) {
                        sb.append("-D");
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
            ++i;
        }
        badName = badName || inParens || inBrackets ;
        if ( badName ) {
            final String errorMessage =
                "\"" + s + "\" is an invalid instance or net name.";
            throw new CDLRenameException( errorMessage );
        }
        return sb.toString();
    }

}
