package com.avlsi.layout;
import com.avlsi.file.common.HierName;
import java.util.Vector;
import com.avlsi.cell.CellInterface;
import com.avlsi.tools.cadencize.CadenceInfo;

public interface CellInterfaceActionInterface {
    void doPort(HierarchicalDirectiveInterface hdi, HierName netName, HierName portName, int direction);	    
    void doLocalSubCell(CellInterface cell, CadenceInfo cellInfo, HierName cellName);		
    void doChannel(HierarchicalDirectiveInterface hdi, 
                   CellInterface subCell,
                   CadenceInfo cellInfo, 
                   HierName portName,
                   int direction );
}
    
