/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class GDS2NameInterface implements CDLNameInterface {

    public GDS2NameInterface() {
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

    private String translate( final String s ) 
        throws CDLRenameException
    {
        final StringBuffer sb = new StringBuffer();
        for (int i = 0; i < s.length(); i++) {
            final char c = s.charAt(i);
            switch (c) {
            case '.':
                sb.append("_D_"); 
                break;
            case ',':
                sb.append("_C_"); 
                break;
            case '[':
                sb.append("_l_"); 
                break;
            case ']':
                sb.append("_r_");
                break;
            case '(':
                sb.append("_L_");
                break;
            case ')':
                sb.append("_R_");
                break;
            case '-':
                sb.append("_M_"); 
                break;
            case '_': 
                sb.append("_U_"); 
                break;
            case '#':
                sb.append("_H_");
                break;
            default:
                if (Character.isLetterOrDigit(c)) {
                    sb.append(c);
                }
                else {
                    sb.append("_"); 
                    final String intStr = Integer.toHexString( c );
                    switch ( intStr.length() ) {
                    case 1:
                        sb.append( "0" );
                        break;
                    case 2:
                        break;
                    default:
                        final String errorMessage =
                            "Unable to translate '" +
                            Character.toString( c ) +
                            "' (0x" +
                            intStr +
                            ")";
                        throw new CDLRenameException( errorMessage );
                    }
                    sb.append( intStr );
                    sb.append("_");
                }
            }
        }
        return sb.toString();
    }
    
}
