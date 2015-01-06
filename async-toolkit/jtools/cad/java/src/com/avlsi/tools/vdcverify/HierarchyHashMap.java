/*
*
*/

package com.avlsi.tools.vdcverify;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.common.HierName;
import java.io.Reader;
import java.io.FileReader;
import java.io.BufferedReader;
import java.util.Map;
import java.util.HashMap;
import java.util.Collection;
import java.util.ArrayList;
import java.util.Set;
import java.util.List;

public class HierarchyHashMap extends CDLFactoryAdaptor {
  
    private HashMap<String, HashMap<String, ArrayList<String>>> cells;
    private HashMap<String, ArrayList<String>> subcells;
//    private ArrayList<String> subcellPorts;                  //for each X-type instance in a subcircuit it holds the port names of the subcircuit connected to X-type instance

    public HierarchyHashMap() throws Exception {
        cells = new HashMap<String, HashMap<String, ArrayList<String>>>();
    }
    
    public void beginSubcircuit(String subName, String[] in, String[] out,
                         Map parameters, Environment env) {
        this.subcells = new HashMap<String, ArrayList<String>>(); 
    }
    
    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        ArrayList<String> portCons = new ArrayList<String>();
        //treaming the portCons
        for (int i=0; i<args.length; i++) {
            portCons.add(args[i].getAsString("."));
        }
        portCons.add(0, subName);
        subcells.put(name.getAsString("."), portCons);
    }
    
    public void endSubcircuit(String subName, Environment env) {
         cells.put(subName, subcells);
          
    }
    //returns true if there is no X-type instances under the SUBCKT subName
    public boolean isSubEmpty(String subName) {
        return this.cells.get(subName).isEmpty();
    }
    //returns the keys (instance names) of the hashmap corresponding to the SUBCKT subName
    public Set<String> getInstanceSet(String subName) {
        return this.cells.get(subName).keySet();
    }
    //returns the SUBCKT name (fqcn) of the instance instName which is instantiated under SUBCKT subName 
    public String getInstanceSubName(String subName, String instName) {
        return this.cells.get(subName).get(instName).get(0);
    }
    public List<String> getPortCons(String subName, String instName) {
        return this.cells.get(subName).get(instName).subList(1, this.cells.get(subName).get(instName).size());
    }
       
    //returns the index (sequence in the port list) of the port portName in the connectivity list of the instance instName instantiated under SUBCKT subName
    public int getPortConIndex(String subName, String instName, String portName) {
        return this.cells.get(subName).get(instName).indexOf(portName) - 1;
    }
    public int getPortConLastIndex(String subName, String instName, String portName) {
        return this.cells.get(subName).get(instName).lastIndexOf(portName) - 1;
    }
    //returns the port connectivity name of the port with index of portIndex of the instance instName instantiated in subName 
    public String getPortConAtIndex(String subName, String instName, int portIndex) {
        return this.cells.get(subName).get(instName).get(portIndex+1);
    }
    public HashMap<String, HashMap<String, ArrayList<String>>> getMap() {
        return this.cells;
    }
    public void printMap() {
        System.out.println(this.cells.toString());
    }
    public boolean isLeaf(String subName) {
        for (String c : this.getInstanceSet(subName)) {
            if (!this.getInstanceSubName(subName, c).startsWith("gate.") && !this.getInstanceSubName(subName, c).startsWith("stack.")) {
                return false;
            }
        }
        return true;
    }
}