/*
*
*/
package com.avlsi.tools.vdcverify;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import java.io.Reader;
import java.io.FileReader;
import java.io.BufferedReader;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.TreeSet;
import java.util.HashSet;
import java.util.Set;
import java.util.Scanner;

public class SplittingHashMap implements CDLFactoryInterface {
  
    private PortListHashMap  mPortHash;
    private HierarchyHashMap mHierHash;
    private static String topCellName;
    private static List<String> topVddList;
  
    public SplittingHashMap(PortListHashMap portHash, HierarchyHashMap hierHash, 
                            String topCellName, List<String> topVddList,
                            final Reader r ) throws Exception {
        mPortHash = portHash;
        mHierHash = hierHash;
        this.topCellName = topCellName;
        this.topVddList = new ArrayList<String>(topVddList);
        ReadCDLIntoFactory.readCDLSimple(r, this);
    }
    
    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters, Environment env) {
        mPortHash.makeResistor( name, n1, n2, val, parameters, env );
        mHierHash.makeResistor( name, n1, n2, val, parameters, env );
    }

    
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters, Environment env) {
        mPortHash.makeCapacitor( name, npos, nneg, val, parameters, env );
        mHierHash.makeCapacitor( name, npos, nneg, val, parameters, env );
    }

    
    public void makeTransistor(HierName name, String type, HierName ns, HierName nd,
                               HierName ng, HierName nb, CDLLexer.InfoToken w,
                               CDLLexer.InfoToken l, Map parameters, Environment env) {
        mPortHash.makeTransistor( name, type, ns, nd, ng, nb, w, l, parameters, env );
        mHierHash.makeTransistor( name, type, ns, nd, ng, nb, w, l, parameters, env );
    }

    
    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val, Map parameters,
                          Environment env) {
        mPortHash.makeDiode( name, type, npos, nneg, val, parameters, env );
        mHierHash.makeDiode( name, type, npos, nneg, val, parameters, env );
    }

    
    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters, Environment env) { 
        mPortHash.makeInductor( name, npos, nneg, val, parameters, env );
        mHierHash.makeInductor( name, npos, nneg, val, parameters, env );
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne,
                            CDLLexer.InfoToken val, Map parameters,
                            Environment env) {
        mPortHash.makeBipolar( name, type, nc, nb, ne, val, parameters, env );
        mHierHash.makeBipolar( name, type, nc, nb, ne, val, parameters, env );
    }
    
    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        mPortHash.makeCall( name, subName, args, parameters, env );
        mHierHash.makeCall( name, subName, args, parameters, env );
    }

    
    public  void beginSubcircuit(String subName, String[] in, String[] out,
                                 Map parameters, Environment env) {
        mPortHash.beginSubcircuit( subName, in, out, parameters, env );
        mHierHash.beginSubcircuit( subName, in, out, parameters, env ); 
    }

   
    public void endSubcircuit(String subName, Environment env) {
        mPortHash.endSubcircuit( subName, env );
        mHierHash.endSubcircuit( subName, env );
    }
    
    public Set<String> getCellSet() {
        return this.mPortHash.getCellSet();
    }
    
    public ArrayList<String> getPortList(String subName) {
        return this.mPortHash.getPortList(subName);
    }
    
    //a recursive method that finds the Vdd port names of each SUBCKT definition
    //in a top-down fashion and sets the corresponding data structure
   public Set<String> findVddPorts(String subName) {
        Set<String> vddSet = new HashSet<String>();
        Set<String> portSet = new HashSet<String>();
        portSet.addAll(this.mPortHash.getPortList(subName));
        if (this.mHierHash.isLeaf(subName)) {
            //leaf cells cannot have more than one Vdd
           vddSet.add(VDCVerify.LEAF_VDD_NAME);
           this.mPortHash.setLeafVdd(subName);
        } else if (VDCVerify.isVDCCell(subName)) {
            vddSet.addAll(this.mPortHash.getVddList(subName));
        } else {
            for (String c : this.mHierHash.getInstanceSet(subName)) { //c would be the instance name in the HierHash of cellName
                Set<String> xVddSet = new HashSet<String>();
                String xSubName = this.mHierHash.getInstanceSubName(subName, c); //keeping the SUBCKT name of the instance name c in xCellName variable for simplicity  
                xVddSet = findVddPorts(xSubName);
                for (String v : xVddSet) {
                    int index = this.mPortHash.getPortIndex(xSubName, v);
                  //report an error message if any of the detected Vdd's in the instances is not in the portlist of the subcell
                    if (index == -1) {
                        System.out.println("Warning: node '" + v + "' is connected to a supply port of an instance, but it is not in the port list of the SUBCKT '" + xSubName +"'!");
                    } else {
                        vddSet.add(this.mHierHash.getPortConAtIndex(subName, c, index));
                    }
                }
                for (String s : this.mHierHash.getPortCons(subName, c)) {
                    if (portSet.contains(s)) {
                        portSet.remove(s);
                    }
                }
            }
            if (!portSet.isEmpty()) 
                this.mPortHash.setWiringTrue(subName);
        }
        this.mPortHash.setVddList(subName, new ArrayList<String>(vddSet));
        if (this.topCellName.equals(subName)) {
            Set<String> diffVddSet = new HashSet<String>(vddSet);
            diffVddSet.removeAll(this.topVddList);
            if (!diffVddSet.isEmpty())          
                System.out.println("Error: node(s) " + diffVddSet + " are detected as supply ports of the top cell '" + 
                                   subName +"', but cannot be found in the user-provided Vdd list!");
            diffVddSet = new HashSet<String>(this.topVddList);
            diffVddSet.removeAll(vddSet);
            if (!diffVddSet.isEmpty())          
                System.out.println("Warning: port(s)  '" + diffVddSet + "' are specified in the user-provided Vdd list, but are not connected to the Vdd port(s) of any leaf cell!");
        }
        return vddSet;
    }
/*    public boolean checkVddConnectivity(String subName, Set<String> vddSet) {
        final Set<String> Leaf_Vdd = new HashSet<String>();
        Leaf_Vdd.add(VDCVerify.LEAF_VDD_NAME);
        boolean cflag = false;
        if (this.mHierHash.isLeaf(subName)) {
            if (!this.mPortHash.getVddSet(subName).equals(Leaf_Vdd) {
                System.out.println("Error: checking Vdd connectivity faild! The assigned supply port of the leaf cell '" + subName + "' is not 'Vdd'!");
                cflag = false;
            } else {
                cflag = true;
            }
        } else {
            for (String c : this.mHierHash.getInstanceSet(subName)) {
                Set<String> diffVddSet = new HashSet<String>();
                String xSubName = this.mHierHash.getInstanceSubName(subName, c);
                for (String v : this.mPortHash.getVddSet(xSubName)) {
                    int index = this.mPortHash.getPortIndex(xSubName, v);
                    if (index == -1) {
                        System.out.println("Error: checkign the Vdd connectivity failed for the cell '" + xSubName + "'! Node '" + v + 
                         "' is assigned as a supply port, but it is not listed as a valid port for this cell!");
                        cflag = false;
                    } else {
                        diffVddSet.add(this.mHierHash.getPortConAtIndex(subName, c, index));
                    }
                }
                if (!diffVddSet.removeAll(vddSet).isEmpty()) {
                    System.out.println("Error: checking the Vdd connectivity failed for the cell '" + subName + "'! Node(s) " + diffVddSet + 
                     " are connected to the supply port(s) of the instance '" + c + "', but they are not in the Vdd set of the SUBCKT '" + subName +"'!");
                    cflag = false;
                } else {
                    cflag = ture;
                }
                Set<String> xVddSet = new HashSet<String>(this.mHierHash.getPortCons(xSubName));
                xVddSet.retainAll(vddSet);
            }
                    
                
        Set<String> xVddSet = new HashSet<String>();
        */
    public void findDomainCrossing(String subName, ArrayList<String> vddList) throws Exception {
        Set<String> vddSet = new HashSet<String> (vddList);
        if (vddSet.isEmpty()) {
            System.err.println("Warning: The vddList parameter for the cell " + subName + " includes no element!");
        } else if (vddSet.size() == 1 && !this.mPortHash.isWiringTrue(subName)) {
            this.mPortHash.setSingleDomain(subName, vddList.get(0));
            //there should not be any problem under this SUBCKT as it only uses a singe Vdd
            //thus, there is no need to create vddDomainMap in this case
            //Also there is no need to go deeper to this cell as the cells under this cell may never be called
        } else if (VDCVerify.isVDCCell(subName)) {
            //System.out.println(subName);
            this.mPortHash.initPortDomain(subName);
            if (this.mPortHash.containsCell(subName)) {
                for (String s : this.mPortHash.getPortList(subName)) {
                    this.mPortHash.setPortDomain(subName, s, VDCVerify.getVDCCellPortDomain(subName, s));
                }
            }
        } else {
             HashMap<String, ArrayList<String>> vddDomainMap = new HashMap<String, ArrayList<String>>();
             //initializing vddDomainMap
             vddSet.add(VDCVerify.NO_PORT_DOMAIN);
             for (String s : vddSet) {
                 ArrayList<String> temp = new ArrayList<String> ();
                 vddDomainMap.put(s, temp);
             }
             for (String c : this.mHierHash.getInstanceSet(subName)) {//c would be the instance name in the HierHash of cellName
                 String xSubName = this.mHierHash.getInstanceSubName(subName, c);
                 if (this.mPortHash.isPortDomainEmpty(xSubName)) {
                     ArrayList<String> xVddList = this.mPortHash.getVddList(xSubName);
                     findDomainCrossing(xSubName, xVddList);
                 }
                 //this seems to be the right place to create the VddDomainMap
                 
                 int portIndex = 0;
                 for (String s : this.mHierHash.getPortCons(subName, c)) {
                     
                     if (!VDCVerify.GROUND_LIST.contains(s)) {
                         String vddCon;
						 //System.out.println("SUBCKT: " + xSubName + "*** index: " + portIndex);
						 if (!this.mPortHash.getPortDomain(xSubName).isEmpty()) { 
                             if (!this.mPortHash.getPortDomain(xSubName).get(portIndex).equals(VDCVerify.NO_PORT_DOMAIN)) {
                                 vddCon =  this.getInstPortVddCon(subName, c, portIndex);
                             } else {
                                 vddCon = VDCVerify.NO_PORT_DOMAIN;
                             }
						 } else {
                             vddCon = VDCVerify.NO_PORT_DOMAIN;
                         }
                         //System.out.println("SUBCKT: " + subName + "*** instance: " + c+ "*** Port: " + s + "*** Vdd Connection: " + vddCon + "*** portIndex: " + portIndex);
                         //System.out.println("Vdd Connection is ****" + vddCon + " *****Port is ******" + s);
                         if (!vddDomainMap.get(vddCon).contains(s)) {
                             vddDomainMap.get(vddCon).add(s);
                         }
                     } 
                     //else if (vddList.contains(s)) {
                        // String vddCon = s;
                        // if (!vddDomainMap.get(vddCon).contains(s)) {
                        //     vddDomainMap.get(vddCon).add(s);
                        // }
                     //}
                    portIndex++;
                 }
             }
           boolean foundDomain = false;  
           for (String portName : this.mPortHash.getPortList(subName)) {
               foundDomain = false;
               for (String domain : vddDomainMap.keySet()) {
                   if (vddDomainMap.get(domain).contains(portName)) {
                       foundDomain = true;
                   }
               }
               if (!foundDomain) {
                   vddDomainMap.get(VDCVerify.NO_PORT_DOMAIN).add(portName);
               }
            }
           //this part updates the "Port Domain" for the subName
           //before updating the port domain, remove the pass-thru wires (aka, mappings to VDCVerify.NO_PORT_DOMAIN)
           updatePortDomain(subName, vddDomainMap);
           //THis seems to be the place for reporting Error message at the subName level by checking vddDomainMap
           vddDomainMap.remove(VDCVerify.NO_PORT_DOMAIN);
           reportDomainCrossing(subName, vddDomainMap);
         }
    }
    public String getInstPortVddCon(String subName, String instName, int portIndex) {
        String xSubName = this.mHierHash.getInstanceSubName(subName, instName);
        //int index = this.mHierHash.getPortConIndex(subName, instName, portName); //sequence index of the portName in the instName instantiation under subName
        
        if (portIndex >= 0) {
            int xVddIndex = this.mPortHash.getVddIndex(xSubName, portIndex); 
            return this.mHierHash.getPortConAtIndex(subName, instName, xVddIndex);
        } else {
            //System.err.println("Error: Port index of " + portName + " was not found among the connectivity list of instance " + instName +" in subcircuit " + subName);
            System.err.println("Error: Negative port index is passed when processing instance " + instName +" in subcircuit " + subName);
            return null;          
        }
    }
            
    //returns the Vdd's of the SUBCKT subName that are connected to the instance instName
    public ArrayList<String> getInstVdd(String subName, String instName) {
        ArrayList<String> intersect = new ArrayList<String>(this.mPortHash.getVddList(subName));
        intersect.retainAll(this.mHierHash.getPortCons(subName, instName));
        return intersect;
    }
    //returns a list of all non-Vdd ports in the connectivity list of the instName in subName
    public ArrayList<String> getNonVddPortCons(String subName, String instName) {
        ArrayList<String> portCons = new ArrayList<String>(this.mHierHash.getPortCons(subName, instName));
        portCons.removeAll(this.mPortHash.getVddList(subName));
        portCons.removeAll(VDCVerify.GROUND_LIST);
        return portCons;
    }
    public void updatePortDomain(String subName, HashMap<String, ArrayList<String>> domainMap) {
        boolean foundDomain = false;
        this.mPortHash.initPortDomain(subName);
        for (String portName : this.mPortHash.getPortList(subName)) {
            foundDomain = false;
            if (domainMap.keySet().contains(portName) || VDCVerify.GROUND_LIST.contains(portName)) {
                this.mPortHash.setPortDomain(subName, portName, portName);
                foundDomain = true;
            } else {
                for (String domain : domainMap.keySet()) {
                    if (domainMap.get(domain).contains(portName) && !this.mPortHash.isPortDomainFixed(subName, portName)) {
                    //if (domainMap.get(domain).contains(portName)) {
                        this.mPortHash.setPortDomain(subName, portName, domain);
                        foundDomain = true;
                    }
                }
            }
            if (!foundDomain) 
                System.out.println("Error: Unable to assign a Vdd domain for the port '" + portName + "' in '" + subName + "'!");
        }
    }
/*
    public void updatePortDomain(String subName, ArrayList<String> vddList) {
//        System.out.println(subName + ": " + vddList);
        String domain;
        boolean flag = false;
        this.mPortHash.initPortDomain(subName);
        for (String s : this.mPortHash.getPortList(subName)) {
            flag = false;
            if (vddList.contains(s) || VDCVerify.GROUND_LIST.contains(s)) {
                this.mPortHash.setPortDomain(subName, s, s);
            } else {
                for (String c : this.mHierHash.getInstanceSet(subName)) {
                    if (this.mHierHash.getPortCons(subName, c).contains(s)) {
                        int portIndex = this.mHierHash.getPortConIndex(subName, c, s);
                        String xSubName = this.mHierHash.getInstanceSubName(subName, c);
                        int xVddIndex = this.mPortHash.getVddIndex(xSubName, portIndex);  //index of the vdd in the port list of xSubName
                        domain = this.mHierHash.getPortConAtIndex(subName, c, xVddIndex);
                        this.mPortHash.setPortDomain(subName, s, domain);
                        flag = true;
                        break;
                    }
                }
                if (!flag) {
                    System.err.println("Error: unable to find a matching domain for port " + s + " in SUBCKT " + subName);
                }
            }
        }
//        System.out.println(this.mPortHash.getPortDomain(subName));
    }
*/
    public void reportDomainCrossing(String subName, HashMap<String, ArrayList<String>> domainMap) {
        boolean inclusive = false;
        TreeSet<String> keys = new TreeSet<String>(domainMap.keySet());
        for (String d1 : keys) {
            for (String d2 : keys.tailSet(d1, inclusive)) {
                //System.out.println("Hello!!!");
                ArrayList<String> intersect = new ArrayList<String>(domainMap.get(d1));
                intersect.retainAll(domainMap.get(d2));
                for (String s : intersect) {
                    System.out.println("Error: Found illegal Domain Crossing in " + subName + " between '" + d1 + "' and '" + d2 + "' at node '" + s + "'!");
                }
            }
        }
    }
/*    public int readUserVddIndex() {
        Scanner sc = new Scanner(System.in);
        int index = sc.nextInt();
        return index;
    }
*/
    public void initVDC() throws Exception {
        this.mPortHash.initVDC();
    }
    //methods used for debugging
    public void printVddCons() {
        System.out.println("Printing the Vdd connection report:");
        for (String subName : this.mHierHash.getMap().keySet()) {
            System.out.println(subName);
            for (String instName : this.mHierHash.getMap().get(subName).keySet()) {
                System.out.print("|___" + instName + ": ");
                //System.out.println(this.getInstVdd(subName, instName));
                System.out.println(this.getNonVddPortCons(subName, instName));
            }
        }
    }
    //for each SUBCKT prints its port names and the Vdd they are connected to 
    public void printVddPortCons() {
        System.out.println("Printing SUBCKT's with their Port/Vdd connection:");
        for (String subName : this.mPortHash.getMap().keySet()) {
            System.out.println(subName);
            System.out.println("Port List: " + this.mPortHash.getPortList(subName));
            System.out.println("Port Domain: " + this.mPortHash.getPortDomain(subName));
        }
    }
          
            
/*            
    public static void main(String[] args) throws Exception {
        if (args.length == 1) {
            System.out.println("Parsing cdl...");
            PortListHashMap portHash = new PortListHashMap();
            HierarchyHashMap hierHash = new HierarchyHashMap();
            SplittingHashMap myMaps = new SplittingHashMap(portHash, hierHash, new BufferedReader(new FileReader(args[0])));
            System.out.println("Finished parsing cdl successfully.");
            myMaps.mPortHash.initVDC();
            System.out.println("Starting the VDCVerify first pass: finding the supply ports for all the cells...");
            myMaps.findVddPorts(VDCVerify.topCell);
            System.out.println("VDCVerify first pass finished successfully.");
            System.out.println("Starting the VDCVerify second pass: finding the illegal domain crossings...");
            myMaps.findDomainCrossing(VDCVerify.topCell, VDCVerify.topVddList);
            System.out.println("VDCVerify second pass finished successfully.");
//            System.out.println("The printout of the HierarchyHashMap is:");
//            myMaps.mHierHash.printMap();
//            System.out.println("The printout of the PortListHashMap is:");
//            myMaps.mPortHash.printMap();
//            System.out.println("The cells with empty Vdd list are:");
//            myMaps.mPortHash.printEmptyVddLists();
//            myMaps.mPortHash.printCells(2);
//            myMaps.printVddCons();
//            myMaps.printVddPortCons();
//            if (!myMaps.mHierHash.isLeaf("lib.sram.fifo.FIFO_DEMUX.1250")) {
//                System.out.println("Hello!");
//            }
        } else {
            System.err.println("Invalid number of the input arguments");
        }
    } */
}
