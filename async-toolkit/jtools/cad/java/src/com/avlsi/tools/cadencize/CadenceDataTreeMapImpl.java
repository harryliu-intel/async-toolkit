/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadencize;

import java.util.Map;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;
import java.util.SortedSet;
import java.util.TreeSet;

import com.avlsi.tools.cadencize.CadenceDataInterface;
import com.avlsi.util.debug.Debug;

public class CadenceDataTreeMapImpl implements CadenceDataInterface {

    public String m_CellName ;
    public String m_SubTypeName ;
    public String m_CellUnitName ;
    /**
       Maps portname to netname
    */
    private TreeMap m_Ports ;
    /**
       Maps subcell name to subcell type name
    */
    private TreeMap m_SubCellTypes ;
    /**
       Maps subcell name to subcell sub-type name
    */
    private TreeMap m_SubCellSubTypes ;
    /**
       Maps subcell name to subcell connections map.
    */
    private TreeMap m_SubCellConnections ;
    
    /**
       The set all nets in the cell.
    */
    private TreeSet m_Nets ;
    
    /**
       Maps net name to iterator of aliases.
    */
    private TreeMap m_NetAliases ;

    private boolean IsANet( final String NetName ) {
	return m_Nets.contains( NetName );
    }
    
    /**
       Default constructor.
    */
    public CadenceDataTreeMapImpl( ) {
	m_Ports = new TreeMap();
	m_SubCellTypes = new TreeMap();
	m_SubCellSubTypes = new TreeMap();
	m_SubCellConnections = new TreeMap();
	m_CellName = null ;
	m_Nets = new TreeSet();
	m_NetAliases = new TreeMap();
    }
    
    /**
       Sets the cell's unit name.
       @param UnitName The new unit name of the cell.
    */
    public final void SetCellUnitName( final String UnitName ) {
	m_CellUnitName = UnitName;
    }
    
    /**
       Sets the cell name.
       @param CellName The new cell name.
    */
    public final void SetCellName( final String CellName ) {
	m_CellName = CellName ;
    }
    
    /**
       Sets the cell's subtype name.
       @param SubType The new subtype name.
    */
    public final void SetCellSubType( final String SubTypeName ) {
	m_SubTypeName = SubTypeName ;
    }
    
    public final void AddPort( final String netName, final String portName ) {
	//Debug.assertTrue( IsANet( netName ) );
	m_Ports.put( portName, netName );
    }
    
    public final void AddNet( final String netName ) {
	m_Nets.add( netName );
    }
    
    public final void AddNetAliases( final String CononicalName, final Iterator aliases ) {
	Debug.assertTrue( IsANet( CononicalName ) ) ;
	m_NetAliases.put( CononicalName, aliases );
    }
    
    public final void AddSubCell( String subcellName, String subcellType, String subcellSubType ){
	m_SubCellTypes.put( subcellName, subcellType );
	m_SubCellSubTypes.put( subcellName, subcellSubType );
    }
    
    public final void AddSubCellConnection( String subcellName, 
					    String subcellNetName,
					    String netName ){
	TreeMap PortMap;
	Debug.assertTrue( IsANet( netName ) );
	PortMap = ( TreeMap ) m_SubCellConnections.get( subcellName );
	
	if ( PortMap == null ) {
	    PortMap = new TreeMap() ;
	    m_SubCellConnections.put( subcellName, PortMap ) ;
	}
	
	PortMap.put( subcellNetName, netName ) ;
    }
    
    public String GetCellUnitName( ) {
	return m_CellUnitName;
    }
    
    public String GetCellName( ) {
	return m_CellName;
    }
    
    public String GetCellSubType( ) {
	if ( m_SubTypeName != null ) {
	    return m_SubTypeName ;
	}
	else{
	    return m_CellName;
	}
    }
    
    public String GetPortNetName( String PortName ) {
	return ( String ) m_Ports.get( PortName ) ;
    }
    
    public SortedSet GetExportedNets( ) {
	TreeSet ret = new TreeSet();
	Iterator pCurrNetName = m_Ports.values().iterator();
	
	while ( pCurrNetName.hasNext() ){
	    String CurrNetName = ( String ) pCurrNetName.next();
	    ret.add( CurrNetName );
	}
	
	return ret;
    }
    
    public String GetInstanceType( String InstanceName ){
	return ( String ) m_SubCellTypes.get( InstanceName );
    }
    
    public String GetInstanceSubType( String InstanceName ) {
	return ( String ) m_SubCellSubTypes.get( InstanceName );
    }
    
    public Map GetSubCellConnections( String InstanceName ) {
	return ( Map ) m_SubCellConnections.get( InstanceName ) ;
    }
    
    public SortedSet GetSubCellNets( String InstanceName ) {
	TreeSet ret = null;
	Map SubCellConnections = GetSubCellConnections( InstanceName );
	
	if ( SubCellConnections != null ) {
	    ret = new TreeSet( SubCellConnections.keySet() );
	}
	
	return ret;
    }
    
    public String GetSubCellConnection( String InstanceName, String SubCellNet ) {
	String ret = null ;
	Map SubCellConnections = GetSubCellConnections( InstanceName );
	if ( SubCellConnections != null ) {
	    ret = ( String ) SubCellConnections.get( SubCellNet ) ;
	}
	return ret;
    }
    
    public Set GetPortNames( ) {
	return m_Ports.keySet();
    }
    
    public SortedSet GetNetNames( ) {
	return m_Nets;
    }

    public Iterator GetNetAliases( final String CononicalNetName ) {
	return ( Iterator ) m_NetAliases.get( CononicalNetName );
    }
    
    public Set GetInstanceNames( ) {
	return m_SubCellTypes.keySet();
    }

}
