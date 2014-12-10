/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadencize;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.Map;
import java.util.TreeMap;

final class SubCellFile {
    private BufferedReader m_BufferedReader;
    private InputStreamReader m_Reader;
    private InputStream m_Stream;
    private TreeMap m_InstTypes;
    private boolean m_StreamDone;

    private int GetChar( ) throws java.io.IOException {
	int c;
	c = m_BufferedReader.read();
	if ( c == -1 ) {
	    m_StreamDone = true;
	}
	return c;
    }

    private void EatWhiteSpace( ) throws java.io.IOException  {
	int c;
	StringBuffer ret = new StringBuffer();
	
	do {
	    m_BufferedReader.mark( 1 );
	    c = GetChar();
	} while ( ( c != -1 ) && ( Character.isWhitespace( ( char ) c ) ) ) ;
	
	if ( c != -1 ) {
	    m_BufferedReader.reset() ;
	}
    }

    private String GetNextWord( ) throws java.io.IOException  {
	int c;
	boolean bIsSpace;
	StringBuffer ret = new StringBuffer();
	
	EatWhiteSpace();
    
	do {
	    c = GetChar();
	    bIsSpace = Character.isWhitespace( (char) c);
	    if ( ( c != -1 ) && ( ! ( bIsSpace ) ) ) {
		ret.append( ( char ) c );
	    }
	} while ( ( c != -1 ) && ( ! ( bIsSpace ) ) ) ;

	return ret.toString();
    }

    private static String MungeCellTypeName( String Src ) {
	
	int Index = 0;
	StringBuffer ret = new StringBuffer();

	while ( Index < Src.length() ) {
	    if ( Src.charAt( Index ) == '/' ) {
		ret.append( '-' ) ;
	    }
	    else{
		ret.append( Src.charAt( Index ) );
	    }
	    ++Index;
	}

	return ret.toString();
    }

    static String CononicalizeSubCellInstanceName( String Src ) {
	int Index = 0 ;
	StringBuffer ret = new StringBuffer();
	
	while ( Index < Src.length() ) {
	    while ( ( Index < Src.length() ) && 
		    ( Src.charAt( Index ) != '[' ) &&
		    ( Src.charAt( Index ) != '(' ) ) {
		if ( ( Index < ( Src.length() - 1 ) ) || Src.charAt( Index ) != '.' ) {
		    ret.append( Src.charAt( Index ) );
		}
		++Index;
	    }

	    if ( Index < Src.length() ){
		
		switch ( Src.charAt( Index ) ) {
		case '[':
		case '(':
		    ret.append( '[' );
		    ++Index;
		    break;
		default:
		    Index = Src.length();
		    break;
		}

		while ( ( Index < Src.length() ) &&
			( Src.charAt( Index ) != ']' ) &&
			( Src.charAt( Index ) != ')' ) ) {
		    if ( Src.charAt( Index ) == ',' ) {
			ret.append( "][" );
		    }
		    else {
			if ( ( Index < ( Src.length() - 1 ) ) || Src.charAt( Index ) != '.' ) {
			    ret.append( Src.charAt( Index ) );
			}
		    }
		    ++Index;
		}

		if ( Index < Src.length() ) {
		    switch ( Src.charAt( Index ) ) {
		    case ']':
		    case ')':
			ret.append( ']' );
			++Index;
			break;
		    default:
			Index = Src.length();
			break;
		    }
		}
	    }
	}
	return ret.toString();
    }
		    

    static String MungeArrayEntry( String Src ) {

	return CononicalizeSubCellInstanceName( Src );
	
    }
   

    public SubCellFile( InputStream Stream )  throws java.io.IOException  {
	m_Stream = Stream;
	m_Reader = new InputStreamReader( Stream, "UTF-8" );
	m_BufferedReader = new BufferedReader( m_Reader ) ;
	m_StreamDone = false;
	m_InstTypes = new TreeMap();
	parse();
    }

    private void parse( )  throws java.io.IOException  {
	while ( ! m_StreamDone ) {
	    String SubType = MungeCellTypeName( GetNextWord() ) ;
	    String InstName = MungeArrayEntry( GetNextWord() ) ;
	    if ( ! ( m_StreamDone ) ) {
//  		System.out.print( "\"" ) ;
//  		System.out.print( SubType );
//  		System.out.print( "\" \"" );
//  		System.out.print( InstName );
//  		System.out.println( "\"");
		m_InstTypes.put( InstName, SubType );
	    }
	}
    }

    Map GetInstanceTypeMap( ) {
	return m_InstTypes;
    }
    

}
