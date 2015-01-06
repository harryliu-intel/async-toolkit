/*
*
*/

package com.avlsi.tools.vdcverify;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.common.HierName;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.Reader;
import java.util.Collection;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;

public class PortListHashMap extends CDLFactoryAdaptor {
  
    //private HashMap<String, Collection<String>> cells;
    private HashMap<String, HashMap<String, ArrayList<String>>> cells;
  
    public PortListHashMap () throws Exception {
        this.cells = new HashMap<String, HashMap<String, ArrayList<String>>>();
    }
    
    public void beginSubcircuit(String subName, String[] in, String[] out,
                         Map parameters, Environment env) {
        HashMap<String, ArrayList<String>> tempPortHash = new HashMap<String, ArrayList<String>>(2);
        ArrayList<String> portList = new ArrayList<String>(); 
        ArrayList<String> vddList  = new ArrayList<String>(); 
        ArrayList<String> vddCons  = new ArrayList<String>();
        ArrayList<String> status   = new ArrayList<String>(1);
        status.add("variable");
        ArrayList<String> wiring   = new ArrayList<String>(1);
        wiring.add("false");

        for (String s:in) {
            portList.add(s);
        }
        tempPortHash.put("Port List", portList);
        tempPortHash.put("Vdd List", vddList);
        tempPortHash.put("Port Domain", vddCons);
        tempPortHash.put("Status", status);
        tempPortHash.put("Wiring", wiring);
        cells.put(subName, tempPortHash);
/*        System.out.print(subName + ":");
        for (int i=0; i<portList.size(); i++) {
            System.out.println(portList.get(i));
        }
*/
    }
    public boolean isVddListEmpty(String subName) {
        if (!cells.containsKey(subName)) {
            System.err.println("Error: Cannot find " + subName + " in the data structure");
            return false;
        }
        else if (cells.containsKey(subName) && cells.get(subName).get("Vdd List").isEmpty()) {
            return true;
        } else {
            return false;
        }
    }
    public boolean isPortDomainEmpty(String subName) {
        if (!cells.containsKey(subName)) {
            System.err.println("Error: Cannot find " + subName + " in the data structure");
            return false;
        }
        else if (cells.get(subName).get("Port Domain").isEmpty()) {
            return true;
        } else {
            return false;
        }
    }
    public ArrayList<String> getPortList(String subName) {
        return this.cells.get(subName).get("Port List");
    }
    
    public int getPortListSize(String subName) {
        return this.cells.get(subName).get("Port List").size();
    }
    
    public ArrayList<String> getVddList(String subName) {
        return this.cells.get(subName).get("Vdd List");
    }

    public Set<String> getVddSet(String subName) {
        Set<String> vddSet = new HashSet<String>(this.cells.get(subName).get("Vdd List"));
        return vddSet;
    }

    public ArrayList<String> getPortDomain(String subName) {
        return this.cells.get(subName).get("Port Domain");
    }
    public void setLeafVdd(String subName) {
        this.cells.get(subName).get("Vdd List").clear();
        this.cells.get(subName).get("Vdd List").add(VDCVerify.LEAF_VDD_NAME);
        }
    public void setVddList(String subName, ArrayList<String> vddList) {
        //System.out.println(subName + ": " + vddList);
        //System.out.println(subName);
        this.cells.get(subName).get("Vdd List").clear();
        this.cells.get(subName).get("Vdd List").addAll(vddList);
        }
    public void setSingleDomain(String subName, String vddDomain) {
        for (String s : this.getPortList(subName)) {
            if (!VDCVerify.GROUND_LIST.contains(s)) {
                this.cells.get(subName).get("Port Domain").add(vddDomain);
            }
            else {
                this.cells.get(subName).get("Port Domain").add(s);
            }
        }
    }
    public boolean isPortDomainFixed(String subName, String portName) {
        if (this.cells.get(subName).get("Port Domain").get(this.getPortIndex(subName, portName)).equals(VDCVerify.NO_PORT_DOMAIN)) {
            return false;
        } else {
            return true;
        }
    }
    public boolean containsCell(final String subName) {
        if (this.cells.containsKey(subName)) {
           return true;
        } else {
            return false;
        }
    }
    public void initPortDomain(String subName) {
        //this.setSingleDomain(subName, this.getVddList(subName).get(0));
        this.setSingleDomain(subName, VDCVerify.NO_PORT_DOMAIN);
    }
    public void setPortDomain(String subName, String portName, String vddDomain) {
        //System.out.println(this.getPortIndex(subName, portName));
        this.cells.get(subName).get("Port Domain").set(this.getPortIndex(subName, portName), vddDomain);
    }
    public int getPortIndex(String subName, String portName) {
        return this.getPortList(subName).indexOf(portName);
    }      
    public HashMap<String, HashMap<String, ArrayList<String>>> getMap() {
        return cells;
    }
    public int getNumVdds(String subName) {
        return this.cells.get(subName).get("Vdd List").size();
    }
    //returns the index of the Vdd in the port list of SubName that is connected to (index)th port of subName
    public int getVddIndex(String subName, int portIndex) {
        //System.out.println("SUBCKT : " +subName+"******port index:"+portIndex+"******returned value:"+this.cells.get(subName).get("Port Domain").get(0));
        if (!this.cells.get(subName).get("Port Domain").isEmpty()) {
            return this.cells.get(subName).get("Port List").indexOf(this.cells.get(subName).get("Port Domain").get(portIndex));
        } else {
           System.err.println("Error:Trying to access Port Domain list of SUBCKT "+subName+" before it is created!");
           //System.err.println("SUBCKT: "+subName+"****portIndex: "+portIndex+"****Port Domain:" + this.cells.get(subName).get("Port Domain"));
           return -1;
        }
    }
    public String getStatus(String subName) {
        return this.cells.get(subName).get("Status").get(0);
    }
    public void setStatus(String subName, String status) {
        this.cells.get(subName).get("Status").set(0, status);
    }
    public boolean isWiringTrue(String subName) {
        if (this.cells.get(subName).get("Wiring").get(0).equals("true")) {
            return true;
        } else {
            return false;
        }
    }
    public void setWiringTrue(String subName) {
        this.cells.get(subName).get("Wiring").set(0, "true");
    }
    public Set<String> getCellSet() {
        return this.cells.keySet();
    }
    public void initVDC() throws Exception {
        String domain;
        for (String subName : VDCVerify.getVDCCellSet()) {
            if (this.containsCell(subName)) {
                for (String portName : this.getPortList(subName)) {
                    domain = VDCVerify.getVDCCellPortDomain(subName, portName);
                    if (this.cells.containsKey(subName) && !this.getVddList(subName).contains(domain) && !VDCVerify.GROUND_LIST.contains(domain)) {
                        this.getVddList(subName).add(domain);
                    }
                }
                this.setStatus(subName, "fixed");
            }
        }
    }
    //debugging methods
    
    //prints out the cells that have more than vddCount Vdd ports
    public void printCells(int vddCount) { 
        System.out.println("Cells with at least " + vddCount + " supply port(s) associated to them are:");
        for (String c : cells.keySet()) {
            if (this.getNumVdds(c) >= vddCount) {
                System.out.println(c);
            }
        }
    }
          
    public void printMap () {
        System.out.println(this.cells.toString());
    }
    public void printEmptyVddLists() {
        for (String c : cells.keySet()) {
            if (isVddListEmpty(c)) {
                System.out.println(c);
            }
        }
    }
/*       public static void main(String[] args) throws Exception {
        if (args.length == 1) {
           PortListHashMap myTable = new PortListHashMap();
           myTable.printMap();
        } else {
            System.err.println("Invalid number of input arguments");
        }
    }*/
}
      
  
