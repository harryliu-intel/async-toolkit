/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadencize;


import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Set ;
import java.util.Iterator ;
import java.text.MessageFormat ;

import com.avlsi.tools.cadencize.CadenceDataInterface;
/**
   A class containing static methods to write out data in an object that
   implements CadenceDataInterface to a skill file.
*/

public final class CadencizeOutputToSkill {
    /**
     * This class should not be instantiated.
     **/
    private CadencizeOutputToSkill() { }

    private static String GetSubCellConnectionFunctionString( CadenceDataInterface cadData, 
							      String SubCellName,
							      long CurrSubCellNum ) {
	
	Set SubCellNets = cadData.GetSubCellNets( SubCellName );
	long InternalFuncCount = 0;
	long LineCount = 0;

	String SubCellFuncName = MakeSubCellConnectionsFuncName( CurrSubCellNum );
	StringBuffer result = new StringBuffer();

	result.append( "( defun " );
	result.append( SubCellFuncName );
	result.append( " ( )\n" );
	if ( SubCellNets != null ) {
	    
	    result.append( "( append\n( list\n" );

	    Iterator i;
	    
	    i = SubCellNets.iterator();

	    while ( i.hasNext() ) {
		if ( LineCount == 500 ) {
		    StringBuffer NewFuncName = new StringBuffer( MakeSubCellConnectionsFuncName( CurrSubCellNum ) );
		    NewFuncName.append( "_" );
		    NewFuncName.append( InternalFuncCount );
		    ++InternalFuncCount;
		    LineCount = 0;
		    
		    result.append( " ) \n ( " );
		    result.append( NewFuncName ) ;
		    result.append( " )\n )\n )\n" );
		    
		    result.append( "( defun " );
		    result.append( NewFuncName );
		    result.append( " ( )\n" );
		    result.append( "( append\n( list\n" );
		}
		    

		    
		    
		String CurrSubCellNet = ( String ) i.next() ;
		result.append( "   ( list \"" );
		result.append( CurrSubCellNet );
		result.append( "\" \"" );
		result.append( cadData.GetSubCellConnection( SubCellName, 
							     CurrSubCellNet ) ) ;
		result.append( "\" )\n" );
		++LineCount;
	    }
	    result.append( "  )\n nil )\n" );
	    
	    
	}
	else {

	    result.append( "nil\n" );
	}
	result.append( ")\n" );
	return result.toString() ;
    }


    private static String GetNetAliasessString( CadenceDataInterface cadData, 
						String NetName,
						long CurrNetNum ) {
	
	Iterator i = cadData.GetNetAliases( NetName );
	long InternalFuncCount = 0;
	long LineCount = 0;

	String NetAliasesFuncName = MakeNetAliasesFuncName( CurrNetNum );
	StringBuffer result = new StringBuffer();

	result.append( "( defun " );
	result.append( NetAliasesFuncName );
	result.append( " ( )\n" );
	if ( i.hasNext() ) {
	    
	    result.append( "( append\n( list\n" );

	    while ( i.hasNext() ) {
		if ( LineCount == 500 ) {
		    StringBuffer NewFuncName = new StringBuffer( MakeNetAliasesFuncName( CurrNetNum ) );
		    NewFuncName.append( "_" );
		    NewFuncName.append( InternalFuncCount );
		    ++InternalFuncCount;
		    LineCount = 0;
		    
		    result.append( " ) \n ( " );
		    result.append( NewFuncName ) ;
		    result.append( " )\n )\n )\n" );
		    
		    result.append( "( defun " );
		    result.append( NewFuncName );
		    result.append( " ( )\n" );
		    result.append( "( append\n( list\n" );
		}
		    

		    
		    
		String CurrAlias = ( String ) i.next() ;
		result.append( "\"");
		result.append(CurrAlias);
		result.append("\"\n" );
		++LineCount;
	    }
	    result.append( "  )\n nil )\n" );
	    
	    
	}
	else {

	    result.append( "nil\n" );
	}
	result.append( ")\n" );
	return result.toString() ;
    }



    private static String GetSubCellConnectionsString( long CurrSubCellNum ) {
	StringBuffer result = new StringBuffer();
	
	result.append( "( " );
	result.append( MakeSubCellConnectionsFuncName( CurrSubCellNum ) );
	result.append( " )" );
	return result.toString();
    }

    private static String MakeSubCellConnectionsFuncName( long SubCellNum ) {
	
	return "CastInfo_SubCellConnectionFunc" + SubCellNum ;
    }

    private static String MakeNetAliasesFuncName( long SubCellNum ) {
	return  "CastInfo_NetAliasesFunc" + SubCellNum;
    }

    public static void WriteSkill( PrintWriter out, CadenceDataInterface cadData  ) {

	long CurrSubCellNum = 0;
	long CurrNetNum = 0;
	Iterator i = cadData.GetInstanceNames().iterator();

	while( i.hasNext() ) {
	    String CurrSubCellInst = ( String ) i.next() ;
	    
	    out.println( GetSubCellConnectionFunctionString( cadData,
							     CurrSubCellInst,
							     CurrSubCellNum ) );
	    ++CurrSubCellNum;
	}


	i = cadData.GetNetNames().iterator() ;
	while ( i.hasNext() ) {
	    final String CurrNetName = ( String ) i.next() ;
	    
	    out.println( GetNetAliasessString( cadData, CurrNetName, CurrNetNum ) ) ;
	    ++CurrNetNum;
        }


	out.println( ";CurrSubCellInst\n( defun ImportCurrentCell ( BlockLibHandle )" );
	out.println( "  ( let (" );
	

	

	i = cadData.GetInstanceNames().iterator();
	CurrSubCellNum = 0;

	out.println( "    ( SubCellsConnections" );
	if ( i.hasNext() ) {
	    
	    out.println( "      ( list" );

	    MessageFormat subcellfmt = new MessageFormat( "        ( list \"{0}\" {1} )" );
	    while( i.hasNext() ) {
		String CurrSubCellInst = ( String ) i.next();
		String[] subcellstrs = { CurrSubCellInst,
					 GetSubCellConnectionsString( CurrSubCellNum ) }; 
		out.println( subcellfmt.format( subcellstrs ) ) ;
		++CurrSubCellNum;
	    }
	    
	    out.println( "      )" );
	    
	}
	else {
	    out.println( "nil" );
	}

	out.println( "    )" ) ;

	i = cadData.GetExportedNets().iterator() ;
	out.println( "    ( ExportedNets");
	if ( i.hasNext() ) {
	    
	    out.println( "      ( list" );

	    while ( i.hasNext() ) {

		String CurrPortNet ;
		CurrPortNet = ( String ) i.next();
		
		out.print( "\"");
		out.print( CurrPortNet );
		out.println( "\"" );
	    }
	   

	    out.println( "     )" );
	   
	}
	else{
	    out.println( "nil" );
	}

	out.println( "   )" ) ;

	out.println( "  )" ) ;

	out.println( "( list" );
	out.println( "nil" );
	out.println( "( list" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "nil" );
	out.println( "SubCellsConnections" ) ;
	out.println( "ExportedNets" );
	out.println( "nil" );
	
	

	/*out.println( "( list" );
	i = cadData.GetNetNames().iterator() ;
	CurrNetNum = 0;
	while ( i.hasNext() ) {
	    final String CurrNetName = ( String ) i.next() ;
	    out.println( "  ( list" );
	    out.print( "    \"");
	    out.print( CurrNetName );
            out.println( "\"" );
	    out.print( "    ( ");
	    out.print( MakeNetAliasesFuncName( CurrNetNum ) );
	    out.println(" )" );
	    out.println("    )" );
	    ++CurrNetNum;
        }
	out.println("  )" );*/
	out.println( "nil" );

	out.println( ")" );
	out.println( ")" );
	out.println( ")" );


	out.println( ")" );
	out.flush();
	
    }

}
