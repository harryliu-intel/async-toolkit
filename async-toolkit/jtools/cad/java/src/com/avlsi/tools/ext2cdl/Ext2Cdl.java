/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.ext2cdl;

import java.util.Iterator;

import java.io.PrintWriter;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.text.MessageFormat;


import com.avlsi.util.debug.Debug ;

import com.avlsi.file.ext.ExtCell;
import com.avlsi.file.ext.FET;
import com.avlsi.file.ext.parse.ExtParser;
import com.avlsi.io.FileSearchPath;
import com.avlsi.file.common.DeviceTypes;
import java.util.Map;
import java.util.Iterator;
import com.avlsi.tools.cadencize.CadenceDataInterface;
import java.util.TreeSet;
import java.util.Set;
import java.util.SortedSet;


/**
 * Extracts list of fets from a .ext file to be used for cadence's lvs.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Ext2Cdl {
    /**
     * This class should not be instantiated.
     **/
    private Ext2Cdl() { }

    static final double fuckermetersinlambda = 100000000.0 / 5 ; 
    private static void usage(final int exitStatus) {
        System.err.println("Usage: " +
                Ext2Cdl.class.getName() + " cell outputfilename");
        System.exit(exitStatus);
    }

    private static double converttolambda( double d ) {
	return ( d * fuckermetersinlambda ) ;
    }


    private static String makeSafeCellName( String cellName ) {
	StringBuffer ret = new StringBuffer();

	int j = 0;

	while( j < cellName.length() ){
	    if ( cellName.charAt( j ) == '(' ) {
		ret.append( "%40" );
	    }
	    else if ( cellName.charAt( j ) == ')' ) {
		ret.append( "%41" );
	    }
	    else if ( cellName.charAt( j ) == ',' ) {
		ret.append( "%44" );
	    }
	    else {
		ret.append( cellName.charAt( j ) );
	    }
	    ++j ;
	}
	return ret.toString();
    }


    public static void WriteCDL( final ExtCell ext, 
				  PrintWriter out, 
				  MessageFormat FETFormat,
				  CadenceDataInterface cadData ) {
	int FETCount ;
	FETCount = 0 ;

	SortedSet SortedNetNames = cadData.GetExportedNets();
	  

	Iterator pCurrNetName = SortedNetNames.iterator();
	out.println( "*.GLOBAL Vdd!:p GND!:g _SReset!:p _PReset!:p" ) ;
	out.print( ".SUBCKT " );
	out.print( cadData.GetCellUnitName() );
	out.print( "-" );
	out.print( makeSafeCellName(cadData.GetCellSubType()) );
	out.print( " / " );

	while ( pCurrNetName.hasNext() ) {
	    String CurrNetName = ( String ) pCurrNetName.next();
	    out.print( CurrNetName );
	    if ( pCurrNetName.hasNext() ) {
		out.println();
		out.print( "+ " );
	    }
	}

	out.println( " " );
	    
	if ( ext != null ) {

	    for (final Iterator iFET = ext.getFETs(); iFET.hasNext(); ) {
		final FET fet = (FET) iFET.next();
		
		final String gateNet =
		    ext.getCanonicalName(fet.getGate().getConnectingNode())
		    .getCadenceString();
		final String sourceNet =
		    ext.getCanonicalName(fet.getSource().getConnectingNode())
		    .getCadenceString();
		final String drainNet =
		    ext.getCanonicalName(fet.getDrain().getConnectingNode())
		    .getCadenceString();

		String BulkNet ;
		String ModelNameRef ;

		Debug.assertTrue( ( fet.getType() == DeviceTypes.N_TYPE ) ||
			      ( fet.getType() == DeviceTypes.P_TYPE ) ) ;

		switch ( fet.getType() ) {
		case DeviceTypes.N_TYPE:
		    BulkNet = new String( "GND!" ) ;
		    ModelNameRef = new String( "N" ) ;
		    break;
		case DeviceTypes.P_TYPE:
		    BulkNet = new String( "Vdd!" ) ;
		    ModelNameRef = new String ( "P" ) ;
		    break;
		default:
		    BulkNet = null;
		    ModelNameRef = null;
		    break;
		}
		
		Object[] FETArguments = {
		    new Integer( FETCount ),
		    drainNet,
		    gateNet,
		    sourceNet,
		    BulkNet,
		    ModelNameRef,
		    new Double( converttolambda(fet.getDrain().getLength()) ),
		    new Double( converttolambda(fet.getGate().getLength()) / 2.0 )
			};
		
		++FETCount ;
		
		out.println( FETFormat.format( FETArguments ) ) ;
		
	    }
	}

	Iterator pCurrInstanceName = cadData.GetInstanceNames().iterator();

	//Print out subcircuit calls for all subcells in the cell.
	while ( pCurrInstanceName.hasNext() ) {
	    String CurrInstanceName = ( String ) pCurrInstanceName.next();
	    //print out instance name with X prepended.
	    out.print( "X" );
	    out.print( CurrInstanceName );
	    
	    out.print( " " );

	    //Get a list of nets in the subcell we are connecting to.
	    //We want them sorted so we can output them in the same order as
	    //the were in the sub circuit definition.
	    SortedSet CurrSubCellNets = cadData.GetSubCellNets( CurrInstanceName );
	    
	    //If the sub cell has connections...
	    if ( CurrSubCellNets != null ) {

		//Output all the nets in this cell that connect to the subcell.
		Iterator pCurrSubCellNet = CurrSubCellNets.iterator();

		while ( pCurrSubCellNet.hasNext() ){
		    String CurrSubCellNet = ( String ) pCurrSubCellNet.next() ;
		    String CurrConnectedNet = 
			cadData.GetSubCellConnection( CurrInstanceName, 
							    CurrSubCellNet ) ;
		    if ( CurrConnectedNet != null ) {
			out.print( CurrConnectedNet ) ;
			if ( pCurrSubCellNet.hasNext() ) {
			    out.println();
			    out.print( "+ " );
			}
		    }
		}
	    }
	    out.print( " / " );
	    out.println( makeSafeCellName( cadData.GetInstanceSubType( CurrInstanceName ) ) );
	}

	out.println( ".ENDS" );
		
	out.flush();

    }
}
