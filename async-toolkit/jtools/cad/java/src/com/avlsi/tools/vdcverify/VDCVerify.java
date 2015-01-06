/*
*
*/
package com.avlsi.tools.vdcverify;

import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2ReverseNameInterface;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.io.FileSearchPath;
import com.avlsi.file.common.HierName;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.Reader;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;


public class VDCVerify {
    public static final String LEAF_VDD_NAME = "Vdd";
    public static final ArrayList<String> GROUND_LIST = new ArrayList<String>();
    static {
        GROUND_LIST.add("GND");
		GROUND_LIST.add("AGND");
		//GROUND_LIST.add("AGND_1");
    }
    public static final String NO_PORT_DOMAIN = "No Domain Assigned!";
    private String cdlFile;
	private static String cdlStyle;
    private String cellName;
    private ArrayList<String> vddList;
    private SplittingHashMap splitHashMap;  
    public static HashMap<String, HashMap<String, String>> vdcCellMap = 
                  new HashMap<String, HashMap<String, String>>();
  /**
  * Constructor
  **/
    VDCVerify(String cdlFile, String cdlStyle, String cellName, 
            ArrayList<String> vddList) throws Exception { 

        this.cdlFile = cdlFile;
		this.cdlStyle = cdlStyle;
        this.cellName = cellName;
        this.vddList = new ArrayList<String> (vddList);
        PortListHashMap portHash = new PortListHashMap();
        HierarchyHashMap hierHash = new HierarchyHashMap();
        this.splitHashMap = new SplittingHashMap(portHash, hierHash, 
                                                 cellName, vddList,
                                                 new BufferedReader(new FileReader(cdlFile)));
    }
      
    private void firstPass() throws Exception {
    
        this.splitHashMap.initVDC();
        this.splitHashMap.findVddPorts(this.cellName);
    
    }

    private void secondPass() throws Exception {

        this.splitHashMap.findDomainCrossing(this.cellName, this.vddList);
    }
  
    private String getCellName() {
        return this.cellName;
    }
  
    public String canonicalize(final String portName, 
                               final AliasedSet localNodes) throws Exception {
                                                    
        HierName canon = (HierName) localNodes.getCanonicalKey(HierName.makeHierName(portName, '.'));
        return canon.getAsString(".");
    }
  
    public HashMap<String, String> canonicalize(final HashMap portDomain, 
                                                final AliasedSet localNodes) throws Exception {
        HashMap<String, String> portDomainCanon = new HashMap<String, String> ();
        HierName canon;
        for (Object obj : portDomain.keySet()) {
            String portName = obj.toString();
            canon = (HierName) localNodes.getCanonicalKey(HierName.makeHierName(portName, '.'));
            String portNameCanon = canon.getAsString(".");
            canon = (HierName) localNodes.getCanonicalKey(HierName.makeHierName(portDomain.get(obj).toString(), '.'));
            String domainCanon = canon.getAsString(".");
            portDomainCanon.put(portNameCanon, domainCanon);
        }
        return portDomainCanon;
    }    

    public static Set<String> getVDCCellSet() {
        return vdcCellMap.keySet();
    }
  
    public static boolean isVDCCell(final String cellName) {
        if (vdcCellMap.containsKey(cellName)) {
            return true;
        } else {
            return false;
        }
    }
	public static String getCdlStyle() {
		return cdlStyle;
	}	
  
  /**
  * Returns the domain associated with the input portName in the input vdcCellName.
  * @vdcCellName is the name of the VDC Cell in CDL form.
  * @portName is the canonical name of the port whose voltage domain is requested in 
  * vdcCellName.
  **/
    public static String getVDCCellPortDomain(final String vdcCellName, final String portName) throws Exception {
        String castPortName; 
		if (getCdlStyle().equals("gds2")) { 
            GDS2ReverseNameInterface gds2CastName = new GDS2ReverseNameInterface(); 
		    castPortName = gds2CastName.renameNode(portName);
		} else {
			castPortName = portName;
		}
        if (!isVDCCell(vdcCellName)) {
            System.err.println("Error: '" + vdcCellName + "' is not marked as a VDC cell!");
            return null;
        } else if (VDCVerify.GROUND_LIST.contains(castPortName) || vdcCellMap.get(vdcCellName).containsValue(castPortName)) {
            return portName;
        } else if (vdcCellMap.get(vdcCellName).containsKey(castPortName)) {
            return vdcCellMap.get(vdcCellName).get(castPortName);
        } else {
            return vdcCellMap.get(vdcCellName).get(DirectiveConstants.VDD_DEFAULT_DOMAIN);
        }
    }
  
    private void makeVDCCellMap(CastFileParser castParser, 
                               String castRoot, HashMap<String, String> vdcHash) throws Exception {
        final Cadencize cadencize = new Cadencize(false);
        CellInterface cellInterface;
        CadenceNameInterface cast2cdlName = new CadenceNameInterface();
        for (String castCellName : vdcHash.keySet()) {
            cellInterface = castParser.getFullyQualifiedCellPretty(castCellName, System.out, 1);
            String cdlCellName = vdcHash.get(castCellName);
            String defaultDomain = DirectiveUtils.getTopLevelDirective(cellInterface, DirectiveConstants.VDD_DEFAULT_DOMAIN).toString();
            final AliasedSet localNodes = cadencize.convert(cellInterface).getLocalNodes();
            HashMap portDomain = new HashMap(
                                     DirectiveUtils.getTopLevelDirective(
                                     cellInterface, 
                                     DirectiveConstants.VDD_DOMAIN, 
                                     DirectiveConstants.NODE_TYPE));
            HashMap<String, String> portDomainCanon = canonicalize(portDomain, localNodes); 
            portDomainCanon.put(DirectiveConstants.VDD_DEFAULT_DOMAIN, canonicalize(defaultDomain, localNodes));
            this.vdcCellMap.put(cdlCellName, portDomainCanon);
            System.out.println("castName: " + castCellName + " ****cdlName: " + cdlCellName + " ****default domain: " + defaultDomain + " *****other domains: " + portDomainCanon);
        }  
    }
    
    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs); 
        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs(argsWithConfigs);
        final PedanticCommandLineArgs pedanticArgs = 
            new PedanticCommandLineArgs(cachedArgs);
        final CommandLineArgs theArgs = pedanticArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String cdlFile = theArgs.getArgValue("cdl", ".");
        final String cellName = (new CadenceNameInterface()).renameCell(theArgs.getArgValue("cell", null));
        final String cdlStyle = theArgs.getArgValue("cdl-style", ".");
        final String vddString = theArgs.getArgValue("vdd-list", ".");
        final String[] vddArray = vddString.split(":");
        final ArrayList<String> vddList = new ArrayList<String>();
        for (int i = 0; i < vddArray.length; i++) {
            vddList.add(vddArray[i]);
        }
        final String vdcString = theArgs.getArgValue("vdc-cells", ".");
        final String[] vdcArray = vdcString.split(":");
        final HashMap<String, String> vdcHash = new HashMap<String, String>();
        for (int i = 0; i < vdcArray.length; i++) {
		    String[] vdcCast2Cdl = vdcArray[i].split("=");
		    if (vdcCast2Cdl.length == 1) { 	
			    vdcHash.put(vdcCast2Cdl[0], vdcCast2Cdl[0]);
			} else if (vdcCast2Cdl.length == 2) {
				vdcHash.put(vdcCast2Cdl[0], vdcCast2Cdl[1]);
			} else {
			    System.err.println("Maximum of one equivalence is allowed for each cell listed in vdc cell!");
			}
		}
        final CastFileParser castParser =
            CastCacheManager.getDefault().getCastFileParser(
                new FileSearchPath(castRoot), "2",
                new StandardParsingOption(theArgs) );
        System.out.println("Parsing cdl...");
        VDCVerify vdcverify = new VDCVerify(cdlFile,
										    cdlStyle,	
                                            cellName,
                                            vddList);
        System.out.println("Finished parsing cdl successfully.");
        System.out.println("Parsing VDC leaf cells...");
        vdcverify.makeVDCCellMap(castParser, castRoot, vdcHash);
        System.out.println("Finished parsing VDC cells.");
        System.out.println("Starting the VDCVerify first pass: finding the supply ports for all the cells...");
        vdcverify.firstPass();
        System.out.println("VDCVerify first pass finished successfully.");
        System.out.println("Starting the VDCVerify second pass: finding the illegal domain crossings...");
        vdcverify.secondPass();
        System.out.println("VDCVerify second pass finished successfully.");
/*        for (String cell : vdcverify.splitHashMap.getCellSet()) {
            if (VDCVerify.isVDCCell(cell)) {
                System.out.println(cell+ "##########################################");
                for (String port : vdcverify.splitHashMap.getPortList(cell)) {
                    System.out.println("port: " + port + "**** domain: " + VDCVerify.getVDCCellPortDomain(cell, port));
                }
            }
        }*/
    }
}
