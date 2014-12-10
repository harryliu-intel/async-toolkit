/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadencize;
import java.util.Map;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.Set;

/**
   Interface specification for data needed to output files for cadence.
*/
public interface CadenceDataInterface {
    /**
       Gets the name of the unit in which the cell resides.
       @return The name of the cell's unit.
    */
    String GetCellUnitName();

    /**
       Gets the name of the cell.
       @return The name of the cell.
    */
    String GetCellName();

    /**
       Gets the name of the cell's subtype.
       @return The name of the cell's subtype.  If the cell is not subtyped,
       the cell name will be returned.
    */
    String GetCellSubType();

    /**
       Gets the net a given port is conencted to.
       @param PortName The name of the port to lookup.
       @return 
       The name of the net connected to the port on success.
       null on failure.
    */
    String GetPortNetName( String PortName );


    /**
       Gets the set of nets that are connected to ports.
       @return A Set containing all the nets that are connected to ports.
    */
    SortedSet GetExportedNets( ) ;
    
    /**
       Gets the type of a given instance as specified in the cast.
       @param InstanceName The name of the instance to lookup.
       @return The type of the instance as specified in the cast on success.
       null on failure.
    */
    String GetInstanceType( String InstanceName );

    /**
       Gets the subtype of a given instance as specified in the auto file or mag file.
       The subtype of an instance is often the same as the type (ie the cell has not been
       subtyped)
       @param InstanceName The name of the instance to lookup.
       @return The subtype of a given instance as specified in the auto file or mag file
       on success.  null on failure.
    */
    String GetInstanceSubType( String InstanceName );

    /**
       Gets the set of nets in the subcell that connect to nets in the cell.
       @param InstanceName The name of the instance to lookup.
       @return The set of subcell nets that connecto to nets in this cell on success,
       or null on failure.
    */
    SortedSet GetSubCellNets( String InstanceName );

    /**
       Gets the name of the net that is connected to a specified net in a specified
       subcell.
       @param InstanceName The name of the subcell to lookup up connection to.
       @param SubCellNet The name of the net in the specified subcell.
       @return The name of the net the net in the subcell is connected to on success,
       null on failure.
    */
    String GetSubCellConnection( String InstanceName, String SubCellNet ) ;

    /**
       Gets the set of all port names in the cell.
       @return A set containing all the port names.
    */
    Set GetPortNames( );

    /**
       Gets the set of all nets in the cell.
       @return A sorted set containing all the net names in the cell.
    */
    SortedSet GetNetNames();


    /**
       Gets an iterator that will iterate over all the aliases for a given net.
       @param CononicalNetName The canonical name for the net for which you want
       the aliases.
    */
    Iterator GetNetAliases( final String CononicalNetName ) ;


    /**
       Gets the set of all instance names in the cell.
       @return A set containing the names of all the instances in the cell.
    */
    Set GetInstanceNames( );
}
