package com.avlsi.tools.jauto;

import java.io.*;
import java.util.*;
import java.util.zip.*;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveTable;

import com.avlsi.fast.CastDesign;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.HalfOperator;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.BlockInterface;

import com.avlsi.cell.CellInterface;

import com.avlsi.io.FileSearchPath;

import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.dsim.Rule;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.text.PrintfFormat;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.debug.GetTime;

import org.jdom.*;
import org.jdom.output.XMLOutputter;
import org.jdom.output.Format;

import org.xml.sax.*;
import org.xml.sax.helpers.XMLReaderFactory;
import org.apache.xerces.parsers.SAXParser;

/**
 * Objective
 * Change TimingPath to String based
 * Change unit to ps
 * dont keep interpath delay slew
 **/
public final class Jtimer {
    private static Map errmap = null;
    private        String plt = "plt";

    private static void printStats(final String what) {
        if (verbose) {
            System.err.println("CPU Time (" + what + "): " + 
                               GetTime.userCPUTime()/1000 + "(User), " + 
                               GetTime.systemCPUTime()/1000 + "(System)");
            final Runtime rt = Runtime.getRuntime();
            System.gc(); System.gc(); System.gc();
            System.err.println("Memory: " +
                   (int) ((rt.totalMemory() - rt.freeMemory())/1e6) + "M used, " +
                   (int) (rt.totalMemory()/1e6) + "M total, " +
                   (int) (rt.freeMemory()/1e6) + "M free, " +
                   (int) (rt.maxMemory()/1e6) + "M max");
        }
    }

    private static void printErr(final String what, final BufferedWriter errFile) throws IOException {
        if (errmap == null)
            errmap = new HashMap();
        Integer I = (Integer) errmap.get(what);
        if(I == null){
            errmap.put(what,(Integer) 1);
            errFile.write(what);
        }
    }

    /**
     * Class that handling array of String
     */
    static class StringArrayUtil {
        public static String join(final String [] stra, final char c){
            return join(stra,c,0,stra.length);
        } 
        public static String join(final String[] stra, final char c, int start, int len) {
            if(stra == null)
                return "";
            final StringBuffer buf = new StringBuffer();
            for (int i = start, j=0; i < stra.length && j<len; i++,j++) {
        	if (i != start)
        	    buf.append(c);
        	buf.append(stra[i]);
            }
            return buf.toString();
        }
        public static String[] split(final String s, final char c) {
            String [] retval;
            if(s==null)
                retval = new String[0];
            else 
                retval = StringUtil.split(s,c);
            return retval;
        }

        public static String [] slice(final String [] stra, int start, int len) {
            if(stra == null)
                return new String[0];
            int length;
            if(stra.length-1 < start || len < 0)
                length = 0;
            else if (stra.length - start < len)
                length = stra.length - start;
            else
                length = len;
            String [] retval = new String[length];
            System.arraycopy(stra,start,retval,0,length);
            return retval;
        }
        public static String [] concatenate(final String [] stra1,final String [] stra2){
            if(stra1 == null)
                return duplicate(stra2);
            if(stra2 == null)
                return duplicate(stra1);
            String [] retval = new String[stra1.length+stra2.length];
            System.arraycopy(stra1,0,retval,0,stra1.length);
            System.arraycopy(stra2,0,retval,stra1.length,stra2.length);
            return retval;
        }
        public static String [] append(final String [] stra, final String s){
            String [] retval;
            if(stra == null)
                return fromString(s);
            if(s == null)
                return duplicate(stra);
            retval = new String[stra.length+1];
            System.arraycopy(stra,0,retval,0,stra.length);
            retval[stra.length] = s;
            return retval;
        }
        public static String [] prepend(final String [] stra, final String s){
            String [] retval;
            if(stra == null)
                return fromString(s);
            if(s == null)
                return duplicate(stra);
            retval = new String[stra.length+1];
            retval[0] = s;
            System.arraycopy(stra,0,retval,1,stra.length);
            return retval;
        }
        public static String [] duplicate(final String [] stra){
            if(stra == null)
                return new String[0];
            String [] retval = new String[stra.length];
            System.arraycopy(stra,0,retval,0,stra.length);
            return retval;
        }
        public static String [] fromString(final String s){
            String [] retval = new String[1];
            retval[0] = s;
            return retval;
        }
        public static boolean equals(final String [] stra1, final String [] stra2){
            if(stra1 == stra2)
                return true;
            if(stra1 == null || stra2 == null)
                return false;
            if(stra1.length != stra2.length)
                return false;
            for(int i=0;i<stra1.length;i++){
                if(! stra1[i].equals(stra2[i]))
                   return false;
            }
            return true;
        }
    }
    /**
     * Class that represent hierarchical net information
     **/
    static class HierNet {
        public String [] instance;
        public CellNet cnet;
        public HierNet(String [] instance, CellNet cnet) {
            this.instance = instance;
            this.cnet = cnet;
        }
        public HierNet() {
            this.instance = null;
            this.cnet = null;
        }
        public HierNet duplicate(){
            HierNet retval = new HierNet();
            if(this.instance == null)
                retval.instance =null;
            else{
                retval.instance = new String[this.instance.length];
                System.arraycopy(this.instance,0,retval.instance,0,this.instance.length);
            }
            retval.cnet = this.cnet;
            return retval;
        }
        public String toString(final char c){
            String netname;
            if(this.cnet == null)
                netname = "";
            else
                netname = cnet.canonicalName.getAsString('.');
            String instance_prefix;
            if(this.instance == null)
                instance_prefix = "";
            else
                instance_prefix = StringArrayUtil.join(this.instance,c);
            if(instance_prefix.length() == 0)
                return netname;
            else
                return instance_prefix + c + netname;
        }
        public String toString(){
            return toString('/');
        }
        public HierName toHierName() throws Exception {
            return HierName.makeHierName(toString('.'),'.');
        }
        public boolean equals(Object o){
            if(o == null)
                return false;
            if(o == this)
                return true;
            HierNet hnet = (HierNet) o;
            if(!this.cnet.equals(hnet.cnet))
                return false;
            if(!StringArrayUtil.equals(this.instance,hnet.instance))
                return false;
            return true;
        }
        public int hashCode(){
            int retval = cnet.hashCode();
            for(int i=0;i<instance.length;i++){
                retval = instance[i].hashCode();
            }
            return retval;
        }
    }
    
    interface NetType {
        int INPUT=0;
        int OUTPUT=1;
        int INOUT=2;
        int VDD=3;
        int GND=4;
        int INTERNAL=5;
        int UNKNOWN=6;
    }
    private String netTypeToString(int nt){
        if(nt == NetType.INPUT)
            return "INPUT";
        else if(nt == NetType.OUTPUT)
            return "OUTPUT";
        else if(nt == NetType.INOUT)
            return "INOUT";
        else if(nt == NetType.VDD)
            return "VDD";
        else if(nt == NetType.GND)
            return "GND";
        else if(nt == NetType.INTERNAL)
            return "INTERNAL";
        else
            return "UNKNOWN";
    }
    private int stringToNetType(String s){
        if(s.equals("INPUT")){
            return NetType.INPUT;
        }else if(s.equals("OUTPUT")){
            return NetType.OUTPUT;
        }else if(s.equals("INOUT")){
            return NetType.INOUT;
        }else if(s.equals("VDD")){
            return NetType.VDD;
        }else if(s.equals("GND")){
            return NetType.GND;
        }else if(s.equals("INTERNAL")){
            return NetType.INTERNAL;
        }else{
            return NetType.UNKNOWN;
        }
    }
    class HierNetName {
        public String [] instance;
        public String net;
        public String container;
        public int type;
        public HierNetName(String [] instance, String container, String net, int type) {
            this.instance = instance;
            this.net = net;
            this.container = container;
            this.type = type;
        }
        public HierNetName(){
            this.instance = null;
            this.net = null;
            this.container = null;
            this.type = NetType.UNKNOWN;
        }
        public HierNetName(final CellNet n){
            this.instance = new String[0];
            this.net = n.canonicalName.getAsString('.');
            this.container = n.container.typeName;
            if(n.isGND())
                this.type = NetType.GND;
            else if(n.isVdd())
                this.type = NetType.VDD;
            else if(n.isPortNet()){
                if(n.portDirection == CellNet.INPUT)
                    this.type = NetType.INPUT;
                else if(n.portDirection == CellNet.OUTPUT)
                    this.type = NetType.OUTPUT;
                else if(n.portDirection == CellNet.INPUTOUTPUT)
                    this.type = NetType.INOUT;
                else
                    this.type = NetType.UNKNOWN;
            } else
                this.type = NetType.INTERNAL;
        }
        public HierNetName(final HierNet n){
            this(n.cnet);
            this.instance = n.instance;
        }
        public HierNetName(final Element net_e){
            net = net_e.getText();
            container = net_e.getAttributeValue("container");
            type = stringToNetType(net_e.getAttributeValue("type"));
            instance = StringArrayUtil.split(net_e.getAttributeValue("instance"),'/');
        }
        public HierNetName duplicate(){
            HierNetName retval = new HierNetName();
            if(this.instance == null)
                retval.instance =null;
            else{
                retval.instance = new String[this.instance.length];
                System.arraycopy(this.instance,0,retval.instance,0,this.instance.length);
            }
            retval.net = this.net;
            retval.container = this.container;
            retval.type = this.type;
            return retval;
        }
        public String toString(final char c){
            String instance_prefix;
            instance_prefix = StringArrayUtil.join(this.instance,c);
            if(instance_prefix.length() == 0)
                return net;
            else
                return instance_prefix + c + net;
        }
        public Element toXML(){
            Element net_e = new Element("NET");
            net_e.setText(net);
            net_e.setAttribute("container", container);
            net_e.setAttribute("instance",StringArrayUtil.join(instance,'/'));
            net_e.setAttribute("type",netTypeToString(type));
            return net_e;
        }
        public String toString(){
            return toString('/');
        }
        public HierName toHierName() throws Exception {
            return HierName.makeHierName(toString('.'),'.');
        }
        public boolean equals(Object o){
            if(o == null)
                return false;
            if(o == this)
                return true;
            HierNetName hnet = (HierNetName) o;
            if(!this.net.equals(hnet.net))
                return false;
            if(!this.container.equals(hnet.container))
                return false;
            if(!StringArrayUtil.equals(this.instance,hnet.instance))
                return false;
            if(this.type != hnet.type)
                return false;
            return true;
        }
        public int hashCode(){
            int retval = net.hashCode();
            for(int i=0;i<instance.length;i++){
                retval = instance[i].hashCode();
            }
            return retval;
        }
        
    }

    /**
     * Class that translate names between design hierarchy
     *
     **/
    static class HierarchyTraverse {
        static List /*CellType*/ getCells(final CellType container, int max_depth, Set visited){
            List retval = new ArrayList();
            if(visited.contains(container))
                return retval;
            if(max_depth == 0)
                return retval;
            visited.add(container);
            retval.add(container);
            Collection subcell_conns = container.getAllSubcellConnections();
            for(Iterator conn_i = subcell_conns.iterator();conn_i.hasNext();){
        	ConnectionInfo conn_info = (ConnectionInfo) conn_i.next();
                int child_depth = max_depth > 0 ? max_depth - 1 : max_depth;
                retval.addAll(getCells(conn_info.child, child_depth, visited));
            }
            return retval;
        }
        /**
         * Find all the instances of a cell contained in another cell (container)
         * The result is a list of all the instance paths. Each instance path is 
         * described as an array of instance names at each hierarchical level.
         *
         * @param container the cell containing the other cell
         * @param cell the cell being contained
         * @param recursive whether the process is recursive or not
         **/
        static List /*String []*/ findInstances(CellType container, CellType cell, boolean recursive) {
            List retval = new ArrayList();
            Collection subcell_conns = container.getAllSubcellConnections();
            for(Iterator conn_i = subcell_conns.iterator();conn_i.hasNext();){
        	ConnectionInfo conn_info = (ConnectionInfo) conn_i.next();
        	String instance_name = conn_info.nameInParent.getAsString('.');
        	if(conn_info.child.typeName.equals(cell.typeName)) {
        	    retval.add(StringArrayUtil.fromString(instance_name));
        	} else if(recursive) {
        	    List child_instances = findInstances(conn_info.child, cell, recursive);
        	    for(Iterator child_instances_i=child_instances.iterator();child_instances_i.hasNext();){
        		String [] child_instance = (String []) child_instances_i.next();
        		retval.add(StringArrayUtil.prepend(child_instance,instance_name));
        	    }
        	}
            }
            return retval;
        }

        /**
         * Split a String (representing a hierarchical path) seperated by '.' based on hierarchy.
         * In the given String, some dots are part of the instance name, some dots are hierarchical seperator.
         * 
         * @param container the cell containing the path
         * @param name the path name to be splited
         * @param recursive whether the process is recursive or not
         **/
        static String [] hierarchicalSplit(CellType container, String name, boolean recursive) throws Exception {
            if(name == null || name.length() == 0)
        	return new String[0];
            String remain_part = name;
            String instance_part = "";
            ConnectionInfo subcell_conn = null;
            while(remain_part.length() != 0){
        	int dot_pos = remain_part.indexOf('.');
        	if(dot_pos == -1) {
        	    instance_part = remain_part;
        	    remain_part = "";
        	} else {
        	    instance_part += remain_part.substring(0,dot_pos+1);
        	    remain_part = remain_part.substring(dot_pos+1);
        	    String instance_name = instance_part.substring(0,instance_part.length()-1);
        	    HierName instance_hier = HierName.makeHierName(instance_name,'.');
        	    subcell_conn = container.getSubcellNamed(instance_hier);
        	    if(subcell_conn != null)  
        		break;
        	}
            }
            String [] retval;
            if(subcell_conn!= null) {
        	if(recursive) {
        	    String [] remain_array = hierarchicalSplit(subcell_conn.child, remain_part, recursive);
                    retval = StringArrayUtil.prepend(remain_array,subcell_conn.nameInParent.getAsString("."));
        	} else {
        	    retval = new String [2];
        	    retval[1] = remain_part;
                    retval[0] = subcell_conn.nameInParent.getAsString(".");
        	}
            } else {
                retval = StringArrayUtil.fromString(name);
            }
            return retval;
        }

        /**
         * Push down a net along the hierarchy path as much as possible
         *
         * @param cnet the net to be pushed down from the current hierarchy
         * @param path_to_push is the hiearchy to push 
         **/
        static HierNet pushDownNet(final CellNet cnet, final String [] path_to_push) throws Exception {
//             System.err.println("pushDownNet IN, net " + cnet.canonicalName.getAsString('.') + " path " + 
//                                StringArrayUtil.join(path_to_push,'/'));
            CellNet current_net=cnet;
            int i;
            for(i=0;i<path_to_push.length;i++) {
        	String instance = path_to_push[i];
        	HierName instance_hier = HierName.makeHierName(instance,'.');
        	ConnectionInfo subcell_conn = current_net.container.getSubcellNamed(instance_hier);
        	if(subcell_conn == null)
        	    break;
        	HierName child_net_hier = null;
        	//child_net_hier = subcell_conn.getChildName(current_net);
        	Set subcell_connection_names = current_net.subcellconnectionNames;
        	for(Iterator scn_i = subcell_connection_names.iterator();scn_i.hasNext();){
        	    HierName scn = (HierName) scn_i.next();
        	    child_net_hier = subcell_conn.getChildName(scn);
        	    if(child_net_hier != null)
        		break;
        	}
        	if(child_net_hier==null)
        	    break;
        	current_net = (CellNet) subcell_conn.child.nets.get(child_net_hier);
            }
            String [] pushed_hier = StringArrayUtil.slice(path_to_push,0,i);
            return new HierNet(pushed_hier,current_net);
        }

        /**
         * Popup a net as much as possible
         *
         * @param container
         * @param hnet HierNet
         **/
        static HierNet popUpNet(final CellType container, final HierNet hnet) throws Exception {
//             System.err.println("popUpNet IN, container " + container.typeName + " Net " + hnet);
            HierNet retval;
            if(hnet.instance.length == 0){
                retval = hnet.duplicate();
            } else {
                HierName subcell_instance_hier = HierName.makeHierName(hnet.instance[0],'.');
                ConnectionInfo subcell_conn = container.getSubcellNamed(subcell_instance_hier);
                CellType subcell = subcell_conn.child;
                HierNet child_hnet = new HierNet();
                child_hnet.instance = StringArrayUtil.slice(hnet.instance,1,hnet.instance.length-1);
                child_hnet.cnet = hnet.cnet;
                child_hnet = popUpNet(subcell,child_hnet);
                
                retval = new HierNet();
                if(child_hnet.instance.length == 0){
                    HierName net_hier = subcell_conn.getParentName(child_hnet.cnet);
                    if(net_hier == null){
                        retval.instance = StringArrayUtil.fromString(hnet.instance[0]);
                        retval.cnet = child_hnet.cnet;
                    }else{
                        retval.instance = new String[0];
                        retval.cnet = container.getNet(net_hier);
                    }
                } else {
                    retval.instance = StringArrayUtil.prepend(child_hnet.instance,hnet.instance[0]);
                    retval.cnet = child_hnet.cnet;
                }
            }
            return retval;
        }

        /**
         * Push down or pop up the net to match the hierarchical path
         * @param container
         * @param path_to_match hierarchy starting from container
         * @param hnet
         **/
        static HierNet getTopMatchingNet(final CellType container, final String [] path_to_match, final HierNet hnet) throws Exception{
//             System.err.println("MACHINGNET IN, container " + container.typeName + " Net " + hnet + " path " + 
//                                StringArrayUtil.join(path_to_match,'/'));
            HierNet retval;
            // pushdown
            CellType current_container = container;
            int i;
            for(i=0;i<path_to_match.length && i<hnet.instance.length;i++){
        	if(path_to_match[i].equals(hnet.instance[i])){
        	    HierName subcell_instance_hier = HierName.makeHierName(path_to_match[i],'.');
        	    ConnectionInfo subcell_conn = current_container.getSubcellNamed(subcell_instance_hier);
        	    current_container = subcell_conn.child;
        	} else {
        	    break;
        	}
            }
            //String [] prefix = StringArrayUtil.slice(path_to_match,0,i);
            String [] current_path_to_match = StringArrayUtil.slice(path_to_match,i,path_to_match.length - i);
            HierNet current_hnet = hnet.duplicate();
            current_hnet.instance = StringArrayUtil.slice(hnet.instance,i,hnet.instance.length - i);
            // popup
            current_hnet = popUpNet(current_container, current_hnet);
            // pushdown
            if(current_hnet.instance.length != 0) {
                if(current_path_to_match.length == 0)
                    retval = current_hnet;
                else {
                    System.err.println("Could not match " + hnet + " to " + 
                                       StringArrayUtil.join(path_to_match,'/') + " in " +
                                       container.typeName + "\n");
                    retval = current_hnet;
                }
            } else {
                current_hnet = pushDownNet(current_hnet.cnet,current_path_to_match);
                if(current_hnet.instance.length != current_path_to_match.length){
                    System.err.println("Could not match " + hnet + " to " + 
                                       StringArrayUtil.join(path_to_match,'/') + " in " +
                                       container.typeName + "\n");
                    retval =  current_hnet;
                } else{
                    current_hnet.instance = new String[0];
                    retval = current_hnet;
                }
            }
            return retval;
        }
    }

    class MaxMinFloat {
        float max = 0;
        float min = 0;
        public MaxMinFloat(){
            this.max = 0;
            this.min = 0;
        }
        public MaxMinFloat(final MaxMinFloat mm){
            this.max = mm.max;
            this.min = mm.min;
        }
        public MaxMinFloat(float ma, float mi){
            if(ma != Float.NEGATIVE_INFINITY && 
               ma != Float.POSITIVE_INFINITY && 
               mi != Float.NEGATIVE_INFINITY && 
               mi != Float.POSITIVE_INFINITY){
                max = Math.max(ma,mi);
                min = Math.min(ma,mi);
            } else {
                max = ma;
                min = mi;
            }
        }
        public void update(final MaxMinFloat mm){
            if(max < mm.max)
                max = mm.max;
            if(min > mm.min)
                min = mm.min;
        }
        public void set(float ma, float mi){
            if(ma != Float.NEGATIVE_INFINITY && 
               ma != Float.POSITIVE_INFINITY && 
               mi != Float.NEGATIVE_INFINITY && 
               mi != Float.POSITIVE_INFINITY){
                max = Math.max(ma,mi);
                min = Math.min(ma,mi);
            }else{
                max = ma;
                min = mi;
            }
        }
        public String toString(){
            return "{" + max + "," + min + "}";
        }
        public MaxMinFloat plus(MaxMinFloat m){
            return new MaxMinFloat(this.max + m.max,this.min + m.min);
        }
        public MaxMinFloat plus(float ma, float mi){
            MaxMinFloat in = new MaxMinFloat(ma,mi);
            return plus(in);
        }
        public void reset(){
            max = 0;
            min = 0;
        }
    }
    class NetSlew {
        MaxMinFloat up;
        MaxMinFloat dn;
        public NetSlew(){
            up = new MaxMinFloat(defaultInputSlewUp,defaultInputSlewUp);
            dn = new MaxMinFloat(defaultInputSlewDown,defaultInputSlewDown);
        }
        public String toString(){
            return "[" + up + "," + dn + "]";
        }
    }
    class InterPathSlew {
        NetSlew curr;
        NetSlew next;
        public InterPathSlew(){
            curr = new NetSlew();
            curr.up.set(defaultInputSlewUp,defaultInputSlewUp);
            curr.dn.set(defaultInputSlewDown,defaultInputSlewDown);
            next = new NetSlew();
            next.up.set(Float.NEGATIVE_INFINITY,Float.POSITIVE_INFINITY);
            next.dn.set(Float.NEGATIVE_INFINITY,Float.POSITIVE_INFINITY);
        }
        public void updateNext(MaxMinFloat slew, boolean rising){
            if(rising) {
                next.up.update(slew);
            } else {
                next.dn.update(slew);
            }
        }
        public void shift(){
            if(next.up.max == Float.NEGATIVE_INFINITY)
                curr.up.max = defaultInputSlewUp;
            else 
                curr.up.max = next.up.max;
            if(next.up.min == Float.POSITIVE_INFINITY)
                curr.up.min = defaultInputSlewUp;
            else
                curr.up.min = next.up.min;
            if(next.dn.max == Float.NEGATIVE_INFINITY)
                curr.dn.max = defaultInputSlewDown;
            else 
                curr.dn.max = next.dn.max;
            if(next.dn.min == Float.POSITIVE_INFINITY)
                curr.dn.min = defaultInputSlewDown;
            else
                curr.dn.min = next.dn.min;
            next.up.max = Float.NEGATIVE_INFINITY;
            next.up.min = Float.POSITIVE_INFINITY;
            next.dn.max = Float.NEGATIVE_INFINITY;
            next.dn.min = Float.POSITIVE_INFINITY;
        }
        public boolean recalculate(MaxMinFloat slew, boolean rising){
            if(rising){
                if(Math.abs(curr.up.max-slew.max) > iterationThreshold)
                    return true;
                if(Math.abs(curr.up.min-slew.min) > iterationThreshold)
                    return true;
            } else{
                if(Math.abs(curr.dn.max-slew.max) > iterationThreshold)
                    return true;
                if(Math.abs(curr.dn.min-slew.min) > iterationThreshold)
                    return true;
            }
            return false;
        }
        public String toString(){
            return "[" + curr + "," + next + "]";
        }
    }

    class TimingPathMeasured {
        //[startNet_index][containerInstance_index]
        public boolean [][] noMeasure = null;
        public MaxMinFloat [][] inputSlew = null;
        public MaxMinFloat [][] pathDelay = null; 

        public TimingPathMeasured(final TimingPath timingPath){
            pathDelay = new MaxMinFloat[timingPath.numOfStartNets()][];
            inputSlew = new MaxMinFloat[timingPath.numOfStartNets()][];
            noMeasure = new boolean[timingPath.numOfStartNets()][];
            for(int i=0;i<timingPath.numOfStartNets();i++){
        	pathDelay[i] = new MaxMinFloat[timingPath.numOfInstances()];
        	inputSlew[i] = new MaxMinFloat[timingPath.numOfInstances()];
                noMeasure[i] = new boolean[timingPath.numOfInstances()];
        	for(int j=0;j<timingPath.numOfInstances();j++){
        	    pathDelay[i][j] = new MaxMinFloat(0,0);
                    if(timingPath.startPolarity)
                        inputSlew[i][j] = new MaxMinFloat(defaultInputSlewUp,defaultInputSlewUp);
                    else
                        inputSlew[i][j] = new MaxMinFloat(defaultInputSlewDown,defaultInputSlewDown);
                    noMeasure[i][j] = false;
        	}
            }
        }

    }

    class TimingPath {
        public long pathGroupID;
        public String container = null;
        public boolean startPolarity = true;
        public float budget = Float.POSITIVE_INFINITY;
        //[startNet_index][containerInstance_index]
        public String [][] containerInstances = null;
        public HierNetName [] startNetsLocal = null;
        public HierNetName [] pathNetsLocal = null;
        //[containerInstance_index][net_index]
        public HierNetName [][] pathNetsCanonical = null;
        public HierNetName [][] pathNetsMatching = null;
        //[startNet_index][containerInstance_index]
        public HierNetName [][] startNetsCanonical = null;
        public HierNetName [][] startNetsMatching = null;

        public TimingPathMeasured measured;

        public Element toXML(){
            Element path_group_e = new Element("PATH_GROUP");
            path_group_e.setAttribute("id",Long.toString(pathGroupID));
            Element container_e = new Element("CONTAINER");
            container_e.setText(container);
            path_group_e.addContent(container_e);
            Element start_polarity_e = new Element("START_POLARITY");
            start_polarity_e.setText(startPolarity?"rising":"falling");
            path_group_e.addContent(start_polarity_e);
            Element budget_e = new Element("BUDGET");
            budget_e.setText(Float.toString(budget));
            path_group_e.addContent(budget_e);
            Element container_instances_e = new Element("CONTAINER_INSTANCES");
            container_instances_e.setAttribute("size",Integer.toString(containerInstances.length));
            for(int i=0;i<containerInstances.length;i++){
                Element cont_inst_e = new Element("CONTAINER_INSTANCE");
                cont_inst_e.setText(StringArrayUtil.join(containerInstances[i],'/'));
                cont_inst_e.setAttribute("inst_i",Integer.toString(i));
                container_instances_e.addContent(cont_inst_e);
            }
            path_group_e.addContent(container_instances_e);
            // start nets
            Element startnets_e = new Element("START_NETS");
            startnets_e.setAttribute("size",Integer.toString(numOfStartNets()));
            for(int i=0;i<numOfStartNets();i++){
                Element net_info_e = new Element("NET_INFO");
                net_info_e.setAttribute("start_i",Integer.toString(i));
                Element local_net_e = new Element("LOCAL_NET");
                local_net_e.addContent(startNetsLocal[i].toXML());
                net_info_e.addContent(local_net_e);
                Element canonical_nets_e = new Element("CANONICAL_NETS");
                canonical_nets_e.setAttribute("size",Integer.toString(numOfInstances()));
                for(int j=0;j<numOfInstances();j++){
                    Element canonical_net_e = startNetsCanonical[i][j].toXML();
                    canonical_net_e.setAttribute("inst_i",Integer.toString(j));
                    canonical_nets_e.addContent(canonical_net_e);
                }
                net_info_e.addContent(canonical_nets_e);
                Element matching_nets_e = new Element("MATCHING_NETS");
                matching_nets_e.setAttribute("size",Integer.toString(numOfInstances()));
                for(int j=0;j<numOfInstances();j++){
                    Element matching_net_e = startNetsMatching[i][j].toXML();
                    matching_net_e.setAttribute("inst_i",Integer.toString(j));
                    matching_nets_e.addContent(matching_net_e);
                }
                net_info_e.addContent(matching_nets_e);
                startnets_e.addContent(net_info_e);
            }
            path_group_e.addContent(startnets_e);
            // path nets
            Element pathnets_e = new Element("PATH_NETS");
            pathnets_e.setAttribute("size",Integer.toString(length()));
            for(int i=0;i<length();i++){
                Element net_info_e = new Element("NET_INFO");
                net_info_e.setAttribute("net_i",Integer.toString(i));
                Element local_net_e = new Element("LOCAL_NET");
                local_net_e.addContent(pathNetsLocal[i].toXML());
                net_info_e.addContent(local_net_e);
                Element canonical_nets_e = new Element("CANONICAL_NETS");
                canonical_nets_e.setAttribute("size",Integer.toString(numOfInstances()));
                for(int j=0;j<numOfInstances();j++){
                    Element canonical_net_e = pathNetsCanonical[j][i].toXML();
                    canonical_net_e.setAttribute("inst_i",Integer.toString(j));
                    canonical_net_e.setAttribute("net_i",Integer.toString(i));
                    canonical_nets_e.addContent(canonical_net_e);
                }
                net_info_e.addContent(canonical_nets_e);
                if(i<length()-1){
                    Element matching_nets_e = new Element("MATCHING_NETS");
                    matching_nets_e.setAttribute("size",Integer.toString(numOfInstances()));
                    for(int j=0;j<numOfInstances();j++){
                        Element matching_net_e = pathNetsMatching[j][i].toXML();
                        matching_net_e.setAttribute("inst_i",Integer.toString(j));
                        matching_net_e.setAttribute("net_i",Integer.toString(i));
                        matching_nets_e.addContent(matching_net_e);
                    }
                    net_info_e.addContent(matching_nets_e);
                }
                pathnets_e.addContent(net_info_e);
            }
            path_group_e.addContent(pathnets_e);
            return path_group_e;
        }
        public int length(){
            return pathNetsCanonical[0].length;
        }
        public boolean endPolarity(){
            if(length()/2 == 0)
                return startPolarity;
            else
                return !startPolarity;
        }
        public int numOfInstances(){
            return containerInstances.length;
        }
        public int numOfStartNets(){
            return startNetsCanonical.length;
        }
        public long numOfPaths(){
            return numOfInstances() * numOfStartNets();
        }
        public TimingPath(Element path_group_e){
            pathGroupID = Integer.parseInt(path_group_e.getAttributeValue("id"));
            container = path_group_e.getChildText("CONTAINER");
            startPolarity = path_group_e.getChildText("START_POLARITY").equals("rising")?true:false;
            budget = Float.parseFloat(path_group_e.getChildText("BUDGET"));
            {
                Element container_instances_e = path_group_e.getChild("CONTAINER_INSTANCES");
                int size = Integer.parseInt(container_instances_e.getAttributeValue("size"));
                containerInstances = new String[size][];
                for(Iterator it=container_instances_e.getChildren("CONTAINER_INSTANCE").iterator();it.hasNext();){
                    Element container_instance_e = (Element) it.next();
                    int inst_i = Integer.parseInt(container_instance_e.getAttributeValue("inst_i"));
                    containerInstances[inst_i] = StringArrayUtil.split(container_instance_e.getText(),'/');
                }
            }
            {
                Element startnets_e = path_group_e.getChild("START_NETS");
                int size = Integer.parseInt(startnets_e.getAttributeValue("size"));
                startNetsLocal = new HierNetName[size];
                startNetsCanonical = new HierNetName[size][];
                startNetsMatching = new HierNetName[size][];
                for(Iterator it=startnets_e.getChildren("NET_INFO").iterator();it.hasNext();){
                    Element net_info_e = (Element) it.next();
                    int start_i = Integer.parseInt(net_info_e.getAttributeValue("start_i"));
                    Element local_net_e = net_info_e.getChild("LOCAL_NET");
                    startNetsLocal[start_i] = new HierNetName(local_net_e.getChild("NET"));
                    Element canonical_nets_e = net_info_e.getChild("CANONICAL_NETS");
                    startNetsCanonical[start_i] = new HierNetName[numOfInstances()];
                    for(Iterator it2=canonical_nets_e.getChildren("NET").iterator();it2.hasNext();){
                        Element net_e = (Element) it2.next();
                        int inst_i = Integer.parseInt(net_e.getAttributeValue("inst_i"));
                        startNetsCanonical[start_i] [inst_i] = new HierNetName(net_e);
                    }
                    Element matching_nets_e = net_info_e.getChild("MATCHING_NETS");
                    startNetsMatching[start_i] = new HierNetName[numOfInstances()];
                    for(Iterator it2=matching_nets_e.getChildren("NET").iterator();it2.hasNext();){
                        Element net_e = (Element) it2.next();
                        int inst_i = Integer.parseInt(net_e.getAttributeValue("inst_i"));
                        startNetsMatching[start_i] [inst_i] = new HierNetName(net_e);
                    }
                }                
            }
            {
                Element pathnets_e = path_group_e.getChild("PATH_NETS");
                int length = Integer.parseInt(pathnets_e.getAttributeValue("size"));
                pathNetsLocal = new HierNetName[length];
                pathNetsCanonical = new HierNetName[numOfInstances()][];
                pathNetsMatching = new HierNetName[numOfInstances()][];
                for(int i=0;i<numOfInstances();i++){
                    pathNetsCanonical[i] = new HierNetName[length];
                    pathNetsMatching[i] = new HierNetName[length-1];
                }
                for(Iterator it=pathnets_e.getChildren("NET_INFO").iterator();it.hasNext();){
                    Element net_info_e = (Element) it.next();
                    int net_i = Integer.parseInt(net_info_e.getAttributeValue("net_i"));
                    Element local_net_e = net_info_e.getChild("LOCAL_NET");
                    pathNetsLocal[net_i] = new HierNetName(local_net_e.getChild("NET"));
                    Element canonical_nets_e = net_info_e.getChild("CANONICAL_NETS");
                    for(Iterator it2=canonical_nets_e.getChildren("NET").iterator();it2.hasNext();){
                        Element net_e = (Element) it2.next();
                        int inst_i = Integer.parseInt(net_e.getAttributeValue("inst_i"));
                        pathNetsCanonical[inst_i] [net_i] = new HierNetName(net_e);
                    }
                    Element matching_nets_e = net_info_e.getChild("MATCHING_NETS");
                    if(matching_nets_e == null)
                        continue;
                    for(Iterator it2=matching_nets_e.getChildren("NET").iterator();it2.hasNext();){
                        Element net_e = (Element) it2.next();
                        int inst_i = Integer.parseInt(net_e.getAttributeValue("inst_i"));
                        pathNetsMatching[inst_i][net_i] = new HierNetName(net_e);
                    }
                }                
                
            }
        }
        public TimingPath(final AbstractPath path, final CellType cell, final CellType topCell, Map canonicalNetCache) throws Exception {
            // Jauto bug? 
            // assert path.container.equals(cell);
            // set container
            container = cell.typeName;
            // set containerInstances
            containerInstances = (String [][]) HierarchyTraverse.findInstances(topCell,cell, true).toArray(new String[0][]);
            if(containerInstances.length == 0){
                String [] instance = new String[0];
                containerInstances = new String[1][];
                containerInstances[0] = instance;
            }
            // set startnet pathnet
            List startnets = new ArrayList(path.getStartNets());
            List pathnets = new ArrayList();
            if(path instanceof CatPath) {
                if (DebugOption.printLevel < 1) System.err.println("Adding CatPath");
                CatPath catpath = (CatPath) path;
                for (Iterator sizpath_i = catpath.getCatPath().iterator(); sizpath_i.hasNext(); ) {
                    SizingPath sizpath = (SizingPath) sizpath_i.next();
                    String [] sizpath_instance = null;
                    HierName sizpath_instance_hier = sizpath.getInstanceName();
                    if(sizpath_instance_hier != null)
                        sizpath_instance = HierarchyTraverse.hierarchicalSplit(cell,
                                                                               sizpath_instance_hier.getAsString("."),
                                                                               true);
                    else
                        sizpath_instance = new String[0];
                    
                    for (Iterator ho_i = sizpath.getPath().iterator(); ho_i.hasNext(); ) {
                        HalfOperator ho = (HalfOperator) ho_i.next();
                        // first half operator in the catpath
                        if(pathnets.size() == 0) 
                            startPolarity = ho.driveDirection == HalfOperator.DriveDirection.PULL_DOWN?true:false;
                        HierNet hnet = new HierNet(sizpath_instance,ho.outputNet);
                        hnet = HierarchyTraverse.popUpNet(cell,hnet);
                        pathnets.add(hnet);
                    }
                }
            } else if(path instanceof SizingPath) {
                if (DebugOption.printLevel < 1) System.err.println("Adding SizingPath");
                SizingPath sizpath = (SizingPath) path;
                for (Iterator ho_i = sizpath.getPath().iterator(); ho_i.hasNext(); ) {
                    HalfOperator ho = (HalfOperator) ho_i.next();
                    if(pathnets.size() == 0) {
                        startPolarity = ho.driveDirection == HalfOperator.DriveDirection.PULL_DOWN?true:false;
                    }
                    HierNet hnet = new HierNet(new String[0],ho.outputNet);
                    pathnets.add(hnet);
                }
            }
            startNetsLocal = new HierNetName[startnets.size()];
            startNetsCanonical = new HierNetName[startnets.size()][];
            startNetsMatching = new HierNetName[startnets.size()][];
            for(int i=0;i<startnets.size();i++){
                CellNet startnet=(CellNet) startnets.get(i);
                startNetsLocal[i] = new HierNetName(startnet);
                startNetsCanonical[i] = new HierNetName[numOfInstances()];
                startNetsMatching[i] = new HierNetName[numOfInstances()];
            }
            pathNetsLocal = new HierNetName[pathnets.size()];
            for(int i=0;i<pathnets.size();i++){
                HierNet pathnet = (HierNet) pathnets.get(i);
                pathNetsLocal[i] = new HierNetName(pathnet);
            }
            pathNetsCanonical = new HierNetName[numOfInstances()][];
            pathNetsMatching = new HierNetName[numOfInstances()][];
            for(int i=0;i<numOfInstances();i++){
                pathNetsCanonical[i] = new HierNetName[pathnets.size()];
                pathNetsMatching[i] = new HierNetName[pathnets.size()-1];
            }
            for(int j=0;j<numOfInstances();j++){
        	String [] container_instance = containerInstances[j];
                for(int k=pathnets.size()-1;k>=0;k--){
                    HierNet net = new HierNet(StringArrayUtil.concatenate(container_instance,
                                                                          ((HierNet) pathnets.get(k)).instance),
                                              ((HierNet) pathnets.get(k)).cnet);
                    HierNet canonical_net;
                    if(((HierNet)pathnets.get(k)).instance.length == 0){
                        canonical_net = (HierNet) canonicalNetCache.get(net);
                        if(canonical_net == null){
                            canonical_net = HierarchyTraverse.popUpNet(topCell,net);
                            canonicalNetCache.put(net,canonical_net);
                        }
                    }else
                        canonical_net = net.duplicate();
                    pathNetsCanonical[j][k] = new HierNetName(canonical_net);
                    if(k==(pathnets.size()-1))
                        continue;
                    HierNet matching_net = 
                        HierarchyTraverse.getTopMatchingNet(topCell,pathNetsCanonical[j][k+1].instance,
                                                            net);
                    pathNetsMatching[j][k] = new HierNetName(matching_net);
                }
                for(int i=0;i<startnets.size();i++){
                    HierNet net = new HierNet(container_instance, (CellNet) startnets.get(i));
                    HierNet canonical_net = (HierNet) canonicalNetCache.get(net);
                    if(canonical_net == null){
                        canonical_net = HierarchyTraverse.popUpNet(topCell,net);
                        canonicalNetCache.put(net,canonical_net);
                    }
                    startNetsCanonical[i][j] = new HierNetName(canonical_net);
                    HierNet matching_net = HierarchyTraverse.getTopMatchingNet(topCell,pathNetsCanonical[j][0].instance,net);
                    startNetsMatching[i][j] = new HierNetName(matching_net);
                }                
            }

            budget = Float.POSITIVE_INFINITY;
            if(path.delay != null) {
                for(int i=0;i<path.delay.length;i++) {
                    float db_tmp = (float) ((path.delay[i] + path.slack[i])/1e-12);
                    if(db_tmp < budget)
                        budget = db_tmp;
                }
            }
            pathGroupID = nextPathGroupID++;
        }

        public String toString() {
            StringBuffer result = new StringBuffer();
            result.append("Container: " + container + "\n"); 
            result.append("Budget: " + budget + "\n"); 
            for(int i=0;i<numOfInstances();i++)
                 result.append("Container Instances: " + 
                               StringArrayUtil.join(containerInstances[i],'/') + "\n");
            result.append("Start polarity: " + startPolarity + "\n");
            for(int i=0;i<numOfStartNets();i++){
                result.append("Start Net Local: " + startNetsLocal[i] + " ("+i+")\n");
                for(int j=0;j<numOfInstances();j++){
                    result.append("Start Net Canonical: " + startNetsCanonical[i][j] + " ("+i+","+j+")\n");
                    result.append("Start Net Matching: " + startNetsMatching[i][j] + " ("+i+","+j+")\n");
                }
            }
            for(int i=0;i<length();i++){
                result.append("Path Net Local: " + pathNetsLocal[i] + " ("+i+")\n");
                for(int j=0;j<numOfInstances();j++){
                    result.append("Path Net Canonical: " + pathNetsCanonical[j][i] + " ("+i+","+j+")\n");
                    if(i<length()-1)
                        result.append("Path Net Matching: " + pathNetsMatching[j][i] + " ("+i+","+j+")\n");
                }
            }
            return result.toString();
        }

        public String toShortString() {
            return toShortString(0);
        }

        public String toShortString(int instance) {
            StringBuffer result = new StringBuffer();
            int j=startPolarity ? 1 : 0;
            String p = "";
            for(int i=0;i<length();i++){
                p = (j % 2) == 0 ? "+" : "-";
                result.append(" " + pathNetsCanonical[instance][i] + p);
                j++;
            }
            return result.toString();
        }

        public String targetNameDirection() {
            return targetNameDirection(0);
        }

        public String targetNameDirection(int instance) {
            String p = (startPolarity ? length() : length()+1)%2 == 0 ? "+" : "-";
            return pathNetsCanonical[instance][length()-1].toString() + p;
        }

        public void calculatePathDelay(int startnet_i, int instance_i, MaxMinFloat [] delay, MaxMinFloat [] slew) throws Exception {
            if(measured.noMeasure[startnet_i][instance_i]) {
                return;
            }
            MaxMinFloat current_slew = measured.inputSlew[startnet_i][instance_i];
            if (allPaths) {
                if(startPolarity)
                    current_slew.set(defaultInputSlewUp,defaultInputSlewUp);
                else
                    current_slew.set(defaultInputSlewDown,defaultInputSlewDown);
            }
            float input_slew = current_slew.max;
            float output_slew = current_slew.max;
            boolean target_direction = ! startPolarity;
            for(int j=0;j<length();j++){
                HierNetName trigger;
                HierNetName target;
                if(j==0){
                    trigger = startNetsMatching[startnet_i][instance_i];
                }else {
                    trigger = pathNetsMatching[instance_i][j-1];
                }
                HierName triggerHname = trigger.toHierName();
                target = pathNetsCanonical[instance_i][j];
                if(target.type != NetType.INTERNAL && false) {
                    measured.noMeasure[startnet_i][instance_i] = true;
                    if (DebugOption.printLevel < 1)
                        System.err.println("No Measure "+topCellName+" Internal "+target);
                }
                else {
                    if (DebugOption.printLevel < 1)
                        System.err.println("DX "+trigger.toString()+" "+target.net+" "+target.container);
                    MaxMinFloat [] extracted = delayExtractor.extract(target.container,
                                                                target.net,
                                                                trigger.toString('.'),
                                                                current_slew,
                                                                target_direction);
                    if(extracted == null) {
                        if (DebugOption.printLevel < 1)
                            System.err.println("extracted = null "+target);
                        if(measured.noMeasure[startnet_i][instance_i] == false){
                            printErr("No transition found " + target.container +
                                          " {" + target.net + (target_direction?"+":"-") + "," + 
                                          trigger.toString('.') + (target_direction?"-":"+") + "}\n", errFile);
                            measured.noMeasure[startnet_i][instance_i] = true;
                            if (DebugOption.printLevel < 1)
                                System.err.println("No Measure "+topCellName+" not extracted "+target);
                        }
                        break;
                    } else {
                        delay[j] = new MaxMinFloat(extracted[0]);
                        slew[j] = new MaxMinFloat(extracted[1]);
                        // correct for slews to 50% point of input and output
                        //delay[j].set(delay[j].max-current_slew.max/2+slew[j].max/2,delay[j].min-current_slew.min/2+slew[j].min/2);
                        if (DebugOption.printLevel < 1)
                            System.err.println("Trig="+triggerHname+" Targ="+target+" "+j+" Slew "+current_slew+","+slew[j]);
                        if (DebugOption.printLevel < 1)
                            System.err.println("extracted = "+delay[j]+" "+target);
                        current_slew = slew[j];
                        output_slew = current_slew.max;
                        if(current_slew.max > maxSlew)
                            printErr("Slew Violation: " + 
                                          pathNetsCanonical[instance_i][j] + 
                                          " " + current_slew.max + "\n", errFile);
                    }
                }
                target_direction = ! target_direction;
            }
        }
        public long numOfNoMeasure(){
            long retval = 0;
            for(int i=0;i<measured.noMeasure.length;i++){
                for(int j=0;j<measured.noMeasure[i].length;j++){
                    if(measured.noMeasure[i][j])
                        retval++;
                }
            }
            return retval;
        }
        public long numOfViolated(){
            long retval = 0;
            for(int i=0;i<measured.pathDelay.length;i++){
                for(int j=0;j<measured.pathDelay[i].length;j++){
                    if(measured.pathDelay[i][j].max > budget)
                        retval++;
                }
            }
            return retval;
        }
        public SortedMap delayHistgram(){
            SortedMap retval = new TreeMap();
            for(int i=0;i<numOfStartNets();i++){
                for(int j=0;j<numOfInstances();j++){
                    if(measured.noMeasure[i][j])
                        continue;
                    Integer key = new Integer((int) Math.ceil((measured.pathDelay[i][j].max * tau /budget)));
                    Integer value;
                    if(retval.containsKey(key)){
                        int intvalue = ((Integer) retval.get(key)).intValue();
                        intvalue+=length();
                        retval.put(key, new Integer(intvalue));
                    } else {
                        retval.put(key, new Integer(length()));
                    }
                }
            }
            return retval;
        }
        public void checkTiming() throws Exception {
            // Jauto bug?
            // assert timingPath.startNetsLocal.length > 0
            if (DebugOption.printLevel < 1)
                System.err.println("CT "+numOfStartNets()+" "+numOfInstances()+" "+targetNameDirection());
            if(startNetsLocal.length == 0)
                return;

            int nummeas=0;
            for(int instance_i=0;instance_i<numOfInstances();instance_i++){
                for(int startnet_i=0;startnet_i<numOfStartNets();startnet_i++) {
                    if(measured.noMeasure[startnet_i][instance_i])
                        continue;
                    boolean recalculate = true;
                    if(startNetsMatching[startnet_i][instance_i].type == NetType.VDD ||
                       startNetsMatching[startnet_i][instance_i].type == NetType.GND){
                        measured.pathDelay[startnet_i][instance_i] = new MaxMinFloat(0,0);
                        //outputSlew[startnet_i][instance_i] = new MaxMinFloat(0,0);
                        updateInterPathSlew(pathNetsCanonical[instance_i][length()-1],new MaxMinFloat(0,0), endPolarity());
                        measured.noMeasure[startnet_i][instance_i] = true;
                        if (DebugOption.printLevel < 1)
                            System.err.println("No Measure "+topCellName+" vdd or gnd "+startnet_i+","+instance_i);
                        recalculate = false;
                        continue;
                    } else if(currentIteration > 0) {
                        InterPathSlew ips = (InterPathSlew) interPathSlew.get(startNetsCanonical[startnet_i][instance_i]);
                        if(ips != null) {
                            recalculate = ips.recalculate(measured.inputSlew[startnet_i][instance_i],startPolarity);
                            //if(measured.pathDelay[startnet_i][instance_i].max >= budget)
                            //    recalculate = true;
                            if(recalculate){
                                if(startPolarity)
                                    measured.inputSlew[startnet_i][instance_i] = ips.curr.up;
                                else
                                    measured.inputSlew[startnet_i][instance_i] = ips.curr.dn;
                            }
                        } else {
                            recalculate = false;
                        }
                    }
                    if(recalculate == false)
                        continue;
                    MaxMinFloat [] delay = new MaxMinFloat[length()];
                    MaxMinFloat [] slew = new MaxMinFloat[length()];
                    calculatePathDelay(startnet_i,instance_i,delay,slew);
                    if(measured.noMeasure[startnet_i][instance_i])
                        continue;
                    nummeas++;
                    MaxMinFloat path_delay = new MaxMinFloat(0,0);
                    for(int i=0;i<length();i++){
                        path_delay = path_delay.plus(delay[i]);
                    }
                    measured.pathDelay[startnet_i][instance_i] = path_delay;
                    updateInterPathSlew(pathNetsCanonical[instance_i][length()-1],slew[length()-1],endPolarity());
                    recalculated++;
                }
            }
            if (DebugOption.printLevel < 1)
                System.err.println("CT2 "+numOfStartNets()+" "+numOfInstances()+" "+nummeas+" "+targetNameDirection());
        }

        public void printViolation(final PrintStream vioFile, int startnet_i, int instance_i, MaxMinFloat [] delay, MaxMinFloat [] slew) throws Exception {
            if(measured.noMeasure[startnet_i][instance_i])
                return;
            MaxMinFloat total_delay = new MaxMinFloat(0,0);
            for(int i=0;i<length();i++){
                total_delay = total_delay.plus(delay[i]);
            }
            if(! allPaths && total_delay.max <= budget)
                return;
            PrintfFormat floatFmt = new PrintfFormat("%.2f");
            vioFile.println("path group: " + pathGroupID);
            vioFile.println("container: " + container);
            String container_instance;
            container_instance = StringArrayUtil.join(containerInstances[instance_i],'/');
            vioFile.println("Container Instance: " + container_instance);
            vioFile.println("Budget: " + floatFmt.sprintf(budget));
            vioFile.println("Slack: " + floatFmt.sprintf(budget - total_delay.max));
            vioFile.println("Effective Tau: " + floatFmt.sprintf(total_delay.max * tau /budget));
            vioFile.println("Transition                                      Delay          Slew            Total Delay");
            vioFile.println("                                               min,  max       min,  max        min,   max");
            vioFile.println("------------------------------------------   -----------    ------------     ---------------");
            //@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  @>>>>,@>>>>  @>>>>,@>>>>>  @>>>>>>,@>>>>>>
            PrintfFormat fmt = new PrintfFormat("%-42s  %6.1f,%6.1f  %6.1f,%6.1f     %6.1f,%6.1f");
            Object [] fmtObj = new Object[7];
            fmtObj[0] = startNetsLocal[startnet_i] + (startPolarity?"+":"-");
            fmtObj[1] = new Float(0);
            fmtObj[2] = new Float(0);
            fmtObj[3] = new Float(measured.inputSlew[startnet_i][instance_i].min);
            fmtObj[4] = new Float(measured.inputSlew[startnet_i][instance_i].max);
            fmtObj[5] = new Float(0);
            fmtObj[6] = new Float(0);
            vioFile.println(fmt.sprintf(fmtObj));
            MaxMinFloat current_total_delay = new MaxMinFloat(0,0);
            boolean target_direction = !startPolarity;
            for(int i=0;i<length();i++){
                fmtObj[0] = pathNetsLocal[i].toString() + (target_direction?"+":"-");
                fmtObj[1] = new Float(delay[i].min);
                fmtObj[2] = new Float(delay[i].max);
                current_total_delay = current_total_delay.plus(delay[i]);
                fmtObj[3] = new Float(slew[i].min);
                fmtObj[4] = new Float(slew[i].max);
                fmtObj[5] = new Float(current_total_delay.min);
                fmtObj[6] = new Float(current_total_delay.max);
                vioFile.println(fmt.sprintf(fmtObj));
                target_direction = !target_direction;
            }
            vioFile.println("");
        }

        public void reportViolation() throws Exception {
            if (DebugOption.printLevel < 1)
                System.err.println("RV "+pathGroupID+" "+numOfStartNets()+" "+numOfInstances()+" "+targetNameDirection());
            // need to be modified
            if(numOfStartNets() == 0)
        	return;
            // assuming startnets is not 0
            int instance_i=0;
            int startnet_i=0;
            int nummeas=0;
            for(int i=0;i<numOfInstances();i++){
        	for(int k=0;k<numOfStartNets();k++) {
                    if(measured.noMeasure[k][i] == true)
                        continue;
                    if (DebugOption.printLevel < 1)
                        System.err.println("RV1 "+k+" "+i+" "+measured.pathDelay[k][i].max+" "+targetNameDirection(i));
                    if (allPaths && (pathNetsCanonical[i][length()-1].type == NetType.INOUT || pathNetsCanonical[i][length()-1].type == NetType.OUTPUT)) {
                        MaxMinFloat [] delay = new MaxMinFloat[length()];
                        MaxMinFloat [] slew = new MaxMinFloat[length()];
                        calculatePathDelay(k,i,delay,slew);
                        printViolation(vioFile, k, i, delay, slew);
                        float slack = budget-measured.pathDelay[k][i].max;
                        String stau = String.format(Locale.US,"%.4f", (double)(tau/1000));
                        String sslack = String.format(Locale.US,"%.4f", (double)(slack/1000));
                        if (measured.pathDelay[k][i].max > 0.0)
                            slackFile.println("slack " + stau + " "+ targetNameDirection(i) + " " + sslack);
                        if (DebugOption.printLevel < 1)
                            System.err.println("TP NT="+pathNetsCanonical[i][length()-1].type+" M="+measured.pathDelay[k][i].max+" B="+budget+" S="+slack+" "+this.toShortString(i));
                    }
        	    if(measured.pathDelay[k][i].max > measured.pathDelay[startnet_i][instance_i].max){
        		instance_i = i;
        		startnet_i = k;
        	    }
                    nummeas++;
        	}
            }
            if (DebugOption.printLevel < 1)
                System.err.println("RV2 "+numOfStartNets()+" "+numOfInstances()+" "+nummeas+" "+startnet_i+" "+instance_i+" "+targetNameDirection());
            if(allPaths || measured.pathDelay[startnet_i][instance_i].max <= budget){
                return;
            }
            MaxMinFloat [] delay = new MaxMinFloat[length()];
            MaxMinFloat [] slew = new MaxMinFloat[length()];
            calculatePathDelay(startnet_i, instance_i, delay, slew);
            printViolation(vioFile, startnet_i,instance_i, delay, slew);
            float slack = budget-measured.pathDelay[startnet_i][instance_i].max;
            String stau = String.format(Locale.US,"%.4f", (double)(tau/1000));
            String sslack = String.format(Locale.US,"%.4f", (double)(slack/1000));
            if (measured.pathDelay[startnet_i][instance_i].max > 0.0)
                slackFile.println("slack " + stau + " "+ targetNameDirection() + " " + sslack);
            if (DebugOption.printLevel < 1) {
                System.err.println("TP M="+measured.pathDelay[startnet_i][instance_i].max+" B="+budget+" S="+slack+" "+this.toShortString());
                System.err.println("TS "+this);
                System.err.println("PD S="+startnet_i+" I="+instance_i+" D="+length()+" M="+measured.pathDelay[startnet_i][instance_i].max+" B="+budget+" I="+StringArrayUtil.join(containerInstances[instance_i],'.'));
            }
        }
    }

    public void pathGeneration() throws Exception {
        if(skipPathGeneration){
            System.err.println("Skipped Path Generation ...");
            return;
        }

        // initialize jauto
        final CastFileParser castFileParser = 
             CastCacheManager.getDefault().getCastFileParser(new FileSearchPath(castRoot), 
         						    castVersion,
         						    new StandardParsingOption(theArgs));
        /*
        final CastFileParser castFileParser = 
            new CastFileParser(new FileSearchPath(castRoot), 
                               castVersion,
                               new StandardParsingOption(theArgs));
        */

        if (verbose) System.err.println("Parsing ...");
        final TechnologyData techData = new TechnologyData(theArgs);
        final CastDesign castDesign = Jauto.loadFullyQualifiedCell(castFileParser, topCellName, (float) (tau*1e-12),1,1, techData, routed, true, skipCellsJauto, null, theArgs.argExists("useSteinerTree"));
        printStats("Parser");
        final String staticizer = theArgs.getArgValue("staticizer", null);
        final String weakInverter = theArgs.getArgValue("weak-inverter", null);
        final String smallInverter = theArgs.getArgValue("small-inverter", null);
        final String[] gateNames;
        {
            final String gates = theArgs.getArgValue("gates", null);
            if (gates == null) gateNames = new String[0];
            else gateNames = StringArrayUtil.split(gates, ':');
        }
        if (verbose) System.err.println("Jauto ...");
        final JautoMessageCenter messageCenter = new JautoMessageCenter(); 
        castDesign.setMessageCenter(messageCenter);
        final CellType topCell = castDesign.getCell(topCellName);
        castDesign.getTopLevelCell().instanceMinimalSubTypes(CastDesign.TRANSISTORS,staticizer, weakInverter, smallInverter, gateNames,castFileParser);
        List/*<GlobalNet>*/ listAllGlobalNets = new ArrayList/*<GlobalNet>*/();
        GlobalNet.generateGlobalNets(topCell, listAllGlobalNets);
        Set allCellTypes = new TreeSet(CellType.getNameComparator());
        allCellTypes.addAll(castDesign.allCellTypes);
        allCellTypes.remove(castDesign.getTopLevelCell());
        SizingPath.generatePaths(allCellTypes, ignoreReset, true, Integer.MAX_VALUE);
        CatPath.generateCatPaths(allCellTypes, ignoreReset, true, false);
        CatPath.reduceCatPaths(allCellTypes, 0);
        TransistorSizingTool sizingTool = new CGTransistorSizingTool(-1,-1,true);
        sizingTool.setOptionUnitDelay((float) (tau*1e-12));
        DelayCalculator delayCalculator = new DelayCalculator(castDesign,sizingTool);
        delayCalculator.calculateDelay();
        printStats("Jauto");
        
        if (verbose) System.err.println("Generating Paths ...");
        List allCells;
        {
            Set visited = new HashSet();
            for(Iterator it=skipCells.iterator();it.hasNext();){
                String cell_name = (String) it.next();
                CellType cell = castDesign.getCell(cell_name);
                if(cell != null)
                    visited.add(cell);
            }
            allCells = HierarchyTraverse.getCells(topCell,depth,visited);
        }

        PrintStream pthFile = new PrintStream(new GZIPOutputStream(new FileOutputStream(topCellName+".pth.gz")),true);
        pthFile.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        pthFile.println("<TIMING_PATHS xmlns=\"http://www.avlsi.com/cad/jtimer/\">");
        org.jdom.output.Format xmloutformat = Format.getCompactFormat();
        xmloutformat.setIndent("  ");
        xmloutformat.setLineSeparator("\n");
        XMLOutputter serializer = new XMLOutputter(xmloutformat);
        for (Iterator cell_i = allCells.iterator(); cell_i.hasNext(); ) {
            Map canonicalNetCache = new HashMap();
            CellType cell = (CellType) cell_i.next();
            if (DebugOption.printLevel < 1)
                System.err.println("CELL "+cell.getLevel()+"="+cell.typeName);
            if(cell.getLevel() > 0){
        	for (Iterator catpath_i = cell.getReducedCatPaths().iterator(); catpath_i.hasNext(); ) {
        	    CatPath catpath = (CatPath)catpath_i.next();
                    if (DebugOption.printLevel < 1) {
                        StringBuffer sb = new StringBuffer();
                        for (Iterator ita = catpath.getCatPath().iterator(); ita.hasNext(); ) {
                            SizingPath spa = (SizingPath)ita.next();
                            for (Iterator its = spa.getPath().iterator(); its.hasNext(); ) {
                                HalfOperator hoa = (HalfOperator)its.next();
                                CellNet cna = hoa.outputNet;
                                sb.append(spa.container.typeName+":"+spa.getInstanceName()+".");
                                sb.append(cna.canonicalName.getCadenceString());
                                if (hoa.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                                    sb.append("- ");
                                }
                                else {
                                    sb.append("+ ");
                                }
                            }
                        }

                        System.err.println("CATPATH"+cell.getLevel()+" "+cell.typeName+":"+catpath.isFragment()+":"+catpath.getStartNets().size()+":"+catpath.getStartNets()+":"+sb);
                    }
        	    if(catpath.isFragment())
        		continue;
                    if(catpath.getStartNets().size() == 0)
                        continue;
        	    TimingPath timingPath = new TimingPath(catpath, cell, topCell, canonicalNetCache);
                    serializer.output(timingPath.toXML(), pthFile);
                    pthFile.println("");
        	}
        	for (Iterator sizpath_i = cell.getSizingPaths().iterator(); sizpath_i.hasNext(); ) {
        	    CatPath sizpath = (CatPath) sizpath_i.next();
        	    TimingPath timingPath = new TimingPath(sizpath, cell, topCell, canonicalNetCache);
                    if (DebugOption.printLevel < 1)
                        System.err.println("SIZPATH2"+cell.getLevel()+" "+cell.typeName+":"+sizpath.isFragment()+":"+sizpath.getStartNets()+":"+sizpath.toString()+":");
        	}
        	for (Iterator sizpath_i = cell.getSizingPaths().iterator(); sizpath_i.hasNext(); ) {
        	    CatPath catpath = (CatPath)sizpath_i.next();
                    StringBuffer sb = new StringBuffer();
                    for (Iterator ita = catpath.getCatPath().iterator(); ita.hasNext(); ) {
                        SizingPath spa = (SizingPath)ita.next();
                        for (Iterator its = spa.getPath().iterator(); its.hasNext(); ) {
                            HalfOperator hoa = (HalfOperator)its.next();
                            CellNet cna = hoa.outputNet;
                            sb.append(spa.container.typeName+":"+spa.getInstanceName()+".");
                            sb.append(cna.canonicalName.getCadenceString());
                            if (hoa.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                                sb.append("- ");
                            }
                            else {
                                sb.append("+ ");
                            }
                        }
                    }
                    StringBuffer ss = new StringBuffer();
                    for (Iterator itd = catpath.getStartNets().iterator(); itd.hasNext(); ) {
                        CellNet cna = (CellNet)itd.next();
                        if (ss.length() > 0) ss = ss.append(" ");
                        ss = ss.append(cna.getInternalCanonical());
                    }

        	    TimingPath timingPath = new TimingPath(catpath, cell, topCell, canonicalNetCache);
                    if (DebugOption.printLevel < 1) {
                        String s = " ok";
                        if (catpath.delay == null) s = " null";
                        System.err.println("SZPATH"+cell.getLevel()+" "+cell.typeName+":"+catpath.isFragment()+":"+catpath.getStartNets().size()+":"+ss+":"+sb+s);
                    }
                    serializer.output(timingPath.toXML(), pthFile);
                    pthFile.println("");
        	}
            } else{
        	for (Iterator catpath_i = cell.getReducedCatPaths().iterator(); catpath_i.hasNext(); ) {
        	    CatPath catpath = (CatPath)catpath_i.next();
                    if (DebugOption.printLevel < 1) {
                        StringBuffer sb = new StringBuffer();
                        for (Iterator ita = catpath.getCatPath().iterator(); ita.hasNext(); ) {
                            SizingPath spa = (SizingPath)ita.next();
                            for (Iterator its = spa.getPath().iterator(); its.hasNext(); ) {
                                HalfOperator hoa = (HalfOperator)its.next();
                                CellNet cna = hoa.outputNet;
                                sb.append(spa.container.typeName+":"+spa.getInstanceName()+".");
                                sb.append(cna.canonicalName.getCadenceString());
                                if (hoa.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                                    sb.append("- ");
                                }
                                else {
                                    sb.append("+ ");
                                }
                            }
                        }
                        System.err.println("CATPATH"+cell.getLevel()+" "+cell.typeName+":"+catpath.isFragment()+":"+catpath.getStartNets().size()+":"+catpath.getStartNets()+":"+sb);
                    }
        	    if(catpath.isFragment())
        		continue;
                    if(catpath.getStartNets().size() == 0)
                        continue;
        	    TimingPath timingPath = new TimingPath(catpath, cell, topCell, canonicalNetCache);
                    serializer.output(timingPath.toXML(), pthFile);
                    pthFile.println("");
        	}
        	for (Iterator sizpath_i = cell.getSizingPaths().iterator(); sizpath_i.hasNext(); ) {
        	    SizingPath sizpath = (SizingPath) sizpath_i.next();
        	    if(sizpath.isFragment())
        		continue;
                    if(sizpath.getStartNets().size() == 0)
                        continue;
                    if (DebugOption.printLevel < 1) {
                        StringBuffer sb = new StringBuffer();
                        for (Iterator its = sizpath.getPath().iterator(); its.hasNext(); ) {
                            HalfOperator hoa = (HalfOperator)its.next();
                            CellNet cna = hoa.outputNet;
                            String in = "";
                            if (sizpath.getInstanceName() != null) in = sizpath.getInstanceName()+".";
                            sb.append(sizpath.container.typeName+":"+in);
                            sb.append(cna.canonicalName.getCadenceString());
                            if (hoa.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                                sb.append("- ");
                            }
                            else {
                                sb.append("+ ");
                            }
                        }
                        System.err.println("SIZPATH"+cell.getLevel()+" "+cell.typeName+":"+sizpath.isFragment()+":"+sizpath.getStartNets().size()+":"+sb);
                    }
        	    TimingPath timingPath = new TimingPath(sizpath, cell, topCell, canonicalNetCache);
                    serializer.output(timingPath.toXML(), pthFile);
                    pthFile.println("");
        	}
            }
            canonicalNetCache.clear();
        }
        pthFile.println("\n</TIMING_PATHS>");
        pthFile.close();
        printStats("Path Generation");
        errFile.flush();
    }

    class DelayExtractor {
        private CastFileParser castFileParser;
        private final CellInterface topCell;
        private Map dirCache;
        private Set errorCells;
        public DelayExtractor(final CastFileParser castFileParser) throws Exception {
            this.castFileParser = castFileParser;
            topCell = routed?
                castFileParser.getFullyQualifiedCell(plt+"." + topCellName).routedSubcells(true):
                castFileParser.getFullyQualifiedCell(plt+"." + topCellName);
            dirCache = new HashMap();
            errorCells = new HashSet();
        }
        private MaxMinFloat [] interpolate(final List measured, final MaxMinFloat in_slew) throws Exception {
            if(((List) measured.get(1)).size() == 0)
                return null;
            if(((List) measured.get(1)).size() == 1){
                return new MaxMinFloat [] {
                    new MaxMinFloat(((Float) ((List) measured.get(2)).get(0)).floatValue(),
                                    ((Float) ((List) measured.get(4)).get(0)).floatValue()),
                    new MaxMinFloat(((Float) ((List) measured.get(3)).get(0)).floatValue(),
                                    ((Float) ((List) measured.get(5)).get(0)).floatValue())
                };
            }
            Map measured_data = new HashMap();
            for(int i=0;i<((List) measured.get(1)).size();i++){
                float measured_in_slew = ((Float) ((List) measured.get(1)).get(i)).floatValue();
                float measured_delay1 = ((Float) ((List) measured.get(2)).get(i)).floatValue();
                float measured_slew1 = ((Float) ((List) measured.get(3)).get(i)).floatValue();
                float measured_delay2 = ((Float) ((List) measured.get(4)).get(i)).floatValue();
                float measured_slew2 = ((Float) ((List) measured.get(5)).get(i)).floatValue();
                measured_data.put(new Float(measured_in_slew), 
                                  new MaxMinFloat[] {new MaxMinFloat(measured_delay1,measured_delay2),
                                                     new MaxMinFloat(measured_slew1,measured_slew2)});
            }
            float retMaxDelay, retMinDelay, retMaxSlew, retMinSlew;
            {
                // max condition
                SortedSet measured_in_slew = new TreeSet(measured_data.keySet());
                SortedSet measured_head = measured_in_slew.headSet(new Float(in_slew.max));
                SortedSet measured_tail = measured_in_slew.tailSet(new Float(in_slew.max));
                Float keyFrom,keyTo;
                if(measured_head.size() == 0){
                    Float [] keys = (Float []) measured_in_slew.toArray(new Float[0]);
                    keyFrom = keys[0];
                    keyTo = keys[1];
                } else if(measured_tail.size() == 0){
                    Float [] keys = (Float []) measured_in_slew.toArray(new Float[0]);
                    keyFrom = keys[keys.length-2];
                    keyTo = keys[keys.length-1];
                } else {
                    keyFrom = (Float) measured_head.last();
                    keyTo = (Float) measured_tail.first();
                }
                MaxMinFloat delayFrom = ((MaxMinFloat []) measured_data.get(keyFrom))[0];
                MaxMinFloat slewFrom = ((MaxMinFloat []) measured_data.get(keyFrom))[1];
                MaxMinFloat delayTo = ((MaxMinFloat []) measured_data.get(keyTo))[0];
                MaxMinFloat slewTo = ((MaxMinFloat []) measured_data.get(keyTo))[1];
                retMaxDelay = linearInterpolate(in_slew.max,keyFrom.floatValue(),keyTo.floatValue(),delayFrom.max,delayTo.max);
                // saturate on max slew if slower than measured
                if(measured_head.size() == 0)
                    retMaxSlew = slewTo.max;
                else
                    retMaxSlew = linearInterpolate(in_slew.max,keyFrom.floatValue(),keyTo.floatValue(),slewFrom.max,slewTo.max);
            }
            {
                // min condition
                SortedSet measured_in_slew = new TreeSet(measured_data.keySet());
                SortedSet measured_head = measured_in_slew.headSet(new Float(in_slew.min));
                SortedSet measured_tail = measured_in_slew.tailSet(new Float(in_slew.min));
                Float keyFrom,keyTo;
                if(measured_head.size() == 0){
                    Float [] keys = (Float []) measured_in_slew.toArray(new Float[0]);
                    keyFrom = keys[0];
                    keyTo = keys[1];
                } else if(measured_tail.size() == 0){
                    Float [] keys = (Float []) measured_in_slew.toArray(new Float[0]);
                    keyFrom = keys[keys.length-2];
                    keyTo = keys[keys.length-1];
                } else {
                    keyFrom = (Float) measured_head.last();
                    keyTo = (Float) measured_tail.first();
                }
                MaxMinFloat delayFrom = ((MaxMinFloat []) measured_data.get(keyFrom))[0];
                MaxMinFloat slewFrom = ((MaxMinFloat []) measured_data.get(keyFrom))[1];
                MaxMinFloat delayTo = ((MaxMinFloat []) measured_data.get(keyTo))[0];
                MaxMinFloat slewTo = ((MaxMinFloat []) measured_data.get(keyTo))[1];
                if(measured_head.size() == 0)
                    retMinDelay = linearInterpolate(in_slew.min,
                                                    keyFrom.floatValue(),
                                                    keyTo.floatValue(),
                                                    delayFrom.min,
                                                    delayFrom.min + (delayTo.min - delayFrom.min)*2);
                else if(measured_tail.size() == 0)
                    retMinDelay = delayTo.min;
                else
                    retMinDelay = linearInterpolate(in_slew.min,
                                                    keyFrom.floatValue(),
                                                    keyTo.floatValue(),
                                                    delayFrom.min,
                                                    delayTo.min);
                if(measured_tail.size() == 0)
                    retMinSlew = slewTo.min;
                else
                    retMinSlew = linearInterpolate(in_slew.min,
                                                   keyFrom.floatValue(),
                                                   keyTo.floatValue(),
                                                   slewFrom.min,
                                                   slewTo.min);
            }
            retMaxDelay = (float) Math.max(retMaxDelay,0.0);
            retMinDelay = (float) Math.max(retMinDelay,0.0);
            retMaxSlew  = (float) Math.max(retMaxSlew,0.0);
            retMinSlew  = (float) Math.max(retMinSlew,0.0);
            return new MaxMinFloat[] {new MaxMinFloat(retMaxDelay,retMinDelay),
                                      new MaxMinFloat(retMaxSlew,retMinSlew)};
        }

        private float linearInterpolate(float slew_in, float slew1, float slew2, float data1, float data2){
            if(slew1==slew2)
                return data1;
            else
                return (slew_in-slew1)*(data2-data1)/(slew2-slew1) + data1;
        }

        /**
         * Calculate the delay and output slew for a transition given the input
         * slew using back-annotated <code>measured_delay</code> directives.  The
         * result is interpolated when there is no data point for the input slew.
         * (unit used in return data is ps)
         * @param cell the cell containing the target
         * @param target the node that is affected by the transition
         * @param trigger the node that caused the transition
         * @param inputSlew the input slew rate
         * @param useMin if <code>true</code> returns the minimum delay, otherwise
         * the maximum delay
         * @param targetDirection <code>true</code> means <code>target</code> is
         * rising, <code>false</code> means <code>target</code> is falling.
         **/
        public MaxMinFloat[] extract(final String cell_name,
                               final String target_net,
                               final String trigger_net,
                               final MaxMinFloat inSlew,
                               final boolean targetDirection) throws Exception {
            CellInterface cell;
            if(errorCells.contains(cell_name)) return null;
            final String cn = (cell_name.equals(topCellName) ? plt : "plt")+"."+cell_name;
            try {
                cell = castFileParser.getFullyQualifiedCell(cn);
            } catch (Exception e) {
                    printErr("WARNING: " + "no file found for " + cn + "\n", errFile);
                    errorCells.add(cell_name);
                    return null;
            }
            
            Map dirs = (Map) dirCache.get(cell_name);
            if(dirs == null){
                dirs = DirectiveUtils.getMultipleBlockDirective(
                                                         Arrays.asList(new BlockInterface[] {
                                                             DirectiveUtils.getUniqueBlock(cell.getBlockInterface(),
                                                                                           BlockInterface.PRS),
                                                             DirectiveUtils.getUniqueBlock(cell.getBlockInterface(),
        									       BlockInterface.SUBCELL)
                                                         }),
                                                         DirectiveConstants.MEASURED_DELAY,
                                                         DirectiveTable.arrayify(DirectiveConstants.UNCHECKED_HALFOP_TYPE));
                dirCache.put(cell_name,dirs);
            }
            HierName target = HierName.makeHierName(target_net,'.');
            HierName trigger = HierName.makeHierName(trigger_net,'.');
            final Pair targetOp =
                new Pair(target, Boolean.valueOf(targetDirection));
            final Pair triggerOp =
                new Pair(trigger, Boolean.valueOf(!targetDirection));
            
            List vals = (List) dirs.get(Arrays.asList(new Pair[] { targetOp, triggerOp }));
            if (vals == null) {
                vals = (List) dirs.get(Collections.singletonList(targetOp));
            }

            if (vals != null) {
                final int model =
                    Math.round(((Float) ((List) vals.get(0)).get(0)).floatValue());
                if (model == Rule.LINEAR_MODEL) {
                    SortedSet slews = new TreeSet((List) vals.get(1));
                    String transition_info = cell_name + "{" + 
                        target_net + (targetDirection?"+":"-") + "," +
                        trigger_net + (targetDirection?"-":"+") + "}";
                    if(inSlew.max > ((Float) slews.last()).floatValue()){
                        printErr("WARNING: " + transition_info +
                                      " Input slew rate " + inSlew.max + 
                                      " is slower than slowest simulated point " + slews.last() +
                                      "\n", errFile);
                    }
                    if(inSlew.min < ((Float) slews.first()).floatValue()){
                        printErr("WARNING: " + transition_info +
                                      " Input slew rate " + inSlew.min + 
                                      " is faster than fastest simulated point " + slews.first() +
                                      "\n", errFile);
                    }
                    return interpolate(vals, inSlew);
                }
            }
            return null;
        }
    }

    public void updateInterPathSlew(final HierNetName hnet, MaxMinFloat slew, boolean direction){
        InterPathSlew ips = (InterPathSlew) interPathSlew.get(hnet);
        if(ips == null){
            ips = new InterPathSlew();
            interPathSlew.put(hnet,ips);
        }
        ips.updateNext(slew,direction);
    }

    public void shiftInterPathSlew(){
        for(Iterator ips_v_i=interPathSlew.values().iterator();ips_v_i.hasNext();){
            InterPathSlew ips_v = (InterPathSlew) ips_v_i.next();
            ips_v.shift();
        }
    }
    
    public void debugElement(final Element e){
        try {
            Format outformat = Format.getCompactFormat();
            outformat.setIndent("  ");
            outformat.setLineSeparator("\n");
            XMLOutputter serializer = new XMLOutputter(outformat);
            
            serializer.output(e, System.err);
            System.err.println("");
        } catch (Exception exc){
            System.err.println(exc);
        }
    }

    abstract class JtimerContentHandler implements ContentHandler {
        public Locator locator;
        public LinkedList stack_e;
        public void setDocumentLocator(Locator locator){
            this.locator = locator;
        }
        public void startDocument() throws SAXException {
            stack_e = new LinkedList();
        }
        public void endDocument() throws SAXException {
        }
        public void processingInstruction(String target, String data)
            throws SAXException {
        }
        public void startPrefixMapping(String prefix, String uri) {
        }
        public void endPrefixMapping(String prefix) {
        }
        public void startElement(String namespaceURI, String localName,
                                 String rawName, Attributes atts)
            throws SAXException {
            if(localName.equals("TIMING_PATHS"))
                return;
            Element top= new Element(localName);
            for (int i=0; i<atts.getLength(); i++){
                top.setAttribute(atts.getLocalName(i),atts.getValue(i));
            }
            stack_e.addLast(top);
         }
        public void endElement(String namespaceURI, String localName,
                               String rawName)
            throws SAXException {
            if(localName.equals("TIMING_PATHS"))
                return;
            Element old_top = (Element) stack_e.removeLast();
            if(localName.equals("PATH_GROUP")){
                TimingPath timingPath = new TimingPath(old_top);
                handleTimingPath(timingPath);
            } else {
                Element top = (Element) stack_e.removeLast();
                top.addContent(old_top);
                stack_e.addLast(top);
            }
        }
        
        public void characters(char[] ch, int start, int end)
            throws SAXException {
            if(stack_e == null || stack_e.isEmpty())
                return;
            String s = (new String(ch, start, end)).trim();
            if(s.length() == 0)
                return;
            Element top = (Element) stack_e.removeLast();
            top.addContent(s);
            stack_e.addLast(top);
        }
        
        public void ignorableWhitespace(char[] ch, int start, int end)
            throws SAXException {
        }
        
        public void skippedEntity(String name) throws SAXException {
        }
        abstract public void handleTimingPath(final TimingPath timingPath);
    }

    class CheckTimingContentHandler extends JtimerContentHandler {
        public void handleTimingPath(final TimingPath timingPath) {
            long id = timingPath.pathGroupID;
            TimingPathMeasured timingPathMeasured;
            timingPathMeasured = (TimingPathMeasured) timingPathMeasuredList.get(new Long(id));
            if (DebugOption.printLevel < 1)
                System.err.println("CTC "+id+" "+timingPath.numOfStartNets()+" "+timingPath.numOfInstances()+" "+timingPath.targetNameDirection());
            if(timingPathMeasured == null){
                timingPathMeasured = new TimingPathMeasured(timingPath);
                timingPathMeasuredList.put(new Long(id), timingPathMeasured);
            }
            timingPath.measured = timingPathMeasured;
            try {
                timingPath.checkTiming();
            } catch (Exception e) {
                System.err.println(e);
                System.exit(1);
            }
        }
    }

    class ReportContentHandler extends JtimerContentHandler {
        public void handleTimingPath(final TimingPath timingPath) {
            long id = timingPath.pathGroupID;
            TimingPathMeasured timingPathMeasured;
            timingPathMeasured = (TimingPathMeasured) timingPathMeasuredList.get(new Long(id));
            timingPath.measured = timingPathMeasured;
            if (DebugOption.printLevel < 1)
                System.err.println("RCH "+id+" "+timingPath.numOfStartNets()+" "+timingPath.numOfInstances()+" "+timingPath.targetNameDirection());
            try {
                timingPath.reportViolation();
                numOfPathGroups++;
                numOfPaths += timingPath.numOfPaths();
                numOfViolatedPaths += timingPath.numOfViolated();
                numOfNotMeasuredPaths += timingPath.numOfNoMeasure();
                SortedMap path_histgram = timingPath.delayHistgram();
                for(Iterator it=path_histgram.entrySet().iterator();it.hasNext();){
                    Map.Entry entry = (Map.Entry) it.next();
                    Integer key = (Integer) entry.getKey();
                    Integer value = (Integer) entry.getValue();
                    if(delayHistogram.containsKey(key)){
                        int intvalue = ((Integer) delayHistogram.get(key)).intValue();
                        intvalue += value.intValue();
                        delayHistogram.put(key, new Integer(intvalue));
                    }else{
                        delayHistogram.put(key,value);
                    }
                }
            } catch (Exception e) {
                System.err.println(e);
                System.exit(1);
            }
        }
    }

    public void checkTiming() throws Exception {
        if(pathGenerationOnly)
            return;
//      CastCacheManager.getDefault().clearCache();
//         if(castFileParser == null)
//             castFileParser = 
//                 CastCacheManager.getDefault().getCastFileParser(new FileSearchPath(castRoot), 
//                                                                 castVersion,
//                                                                 new StandardParsingOption(theArgs));
        final CastFileParser castFileParser = 
                new CastFileParser(new FileSearchPath(castRoot), 
                                   castVersion,
                                   new StandardParsingOption(theArgs));
        delayExtractor = new DelayExtractor(castFileParser);
        
//        File intFileTest = new File(topCellName + ".inf");
//         if(intFileTest.exists()){
//         }else{
//         }

        
        if (verbose) System.err.println("Checking Timing ...");
        for(currentIteration=0; currentIteration<iterationNumber;currentIteration++){
            if (verbose) System.err.println("    iteration " + currentIteration + " ...");
            // read path file
            XMLReader parser = new SAXParser();
            parser.setContentHandler(new CheckTimingContentHandler());
            try {
                parser.parse(new InputSource(new GZIPInputStream(new FileInputStream(topCellName+".pth.gz"))));
            } catch (Exception e){
                System.err.println(e);
                throw new RuntimeException(e);
            }

            shiftInterPathSlew();
            printStats("Checking Timing Iteration " + currentIteration);
            errFile.flush();
            if(recalculated == 0){
                if (verbose) System.err.println("Early Termination");
                break;
            } else{
                if (verbose) System.err.println("Recalculated Paths: " + recalculated);
                recalculated = 0;
            }
        }
    }

    public void report() throws Exception {
        if(pathGenerationOnly)
            return;

        if (verbose) System.err.println("Reporting ...");
        vioFile = new PrintStream(new GZIPOutputStream(new FileOutputStream(topCellName+".vio.gz")),true);
        slackFile = new PrintStream(new FileOutputStream(slackFileName));
        delayHistogram = new TreeMap();
        numOfPaths = 0;
        numOfPathGroups = 0;
        numOfViolatedPaths = 0;
        numOfNotMeasuredPaths = 0;

        // read path file
        XMLReader parser = new SAXParser();
        parser.setContentHandler(new ReportContentHandler());
        try {
            parser.parse(new InputSource(new GZIPInputStream(new FileInputStream(topCellName+".pth.gz"))));
        } catch (Exception e){
            //System.err.println(e);
            throw new RuntimeException(e);
        }
        vioFile.close();
        slackFile.close();
        BufferedWriter hisFile = new BufferedWriter(new FileWriter(topCellName+".his"));
        for(Iterator it=delayHistogram.entrySet().iterator();it.hasNext();){
            Map.Entry entry = (Map.Entry) it.next();
            Integer key = (Integer) entry.getKey();
            Integer value = (Integer) entry.getValue();
            hisFile.write(key + "\t" + value + "\n");
        }
        hisFile.close();
        BufferedWriter sumFile = new BufferedWriter(new FileWriter(topCellName+".sum"));
        sumFile.write("Cell" + " " + topCellName + "\n");
        sumFile.write("Tau"  + " " + tau + "\n");
        sumFile.write("Path Groups" + " " + numOfPathGroups + "\n");
        sumFile.write("Paths(total)" + " " + numOfPaths + "\n");
        long measured = numOfPaths - numOfNotMeasuredPaths;
        sumFile.write("Paths(measured)" + " " + measured + "\n");
        sumFile.write("Paths(violated)" + " " + numOfViolatedPaths + "\n");
        sumFile.close();
    }

    private String topCellName;
    private Map /* TimingPathMeasured */ timingPathMeasuredList;
    private final float defaultInputSlewUp;
    private final float defaultInputSlewDown;
    private final float maxSlew;
    private final int iterationNumber;
    private int currentIteration;
    private final float iterationThreshold;
    private Map /* HierNet, SlewInfo */ interPathSlew;
    private Map /* HierNet, SlewInfo */ ioSlew;
    private final BufferedWriter errFile;
    private PrintStream vioFile;
    private PrintStream slackFile;
    private String slackFileName;
    private int depth;
    private Set/*String*/ skipCells;
    private Set/*String*/ skipCellsJauto;
    private long recalculated = 0;
    private boolean routed = false;
    private static boolean verbose = false;
    private final String castRoot;
    private final Boolean allPaths;
    private final Boolean ignoreReset;
    private final String fulcrumPdkRoot;
    private final String castVersion;
    private float tau;
    private CommandLineArgs theArgs;
    //private CastFileParser castFileParser;
    private DelayExtractor delayExtractor;
    private boolean skipPathGeneration;
    private boolean pathGenerationOnly;
    private boolean useInterfaceFile;
    private long nextPathGroupID = 0;
    private long numOfPaths = 0;
    private long numOfPathGroups = 0;
    private long numOfViolatedPaths = 0;
    private long numOfNotMeasuredPaths = 0;
    private SortedMap delayHistogram;

    private void usage(){
        System.err.print(
                         "java com.avlsi.tools.jauto.Jtimer\n" +
                         "    --cast-path=<path> (defaults to .)\n" +
                         "    --cell=<cell> (fully qualified cell name)\n" +
                         "    --fulcrum-pdk-root=<path> (path to pdk root)\n" +
                         "    --tau=<number> (tau, default to 53)\n" +
                         "    [--cast-version=[ 1 | 2 ]] (defaults to 2)\n" +
                         "    [--depth=<-1> (defaults to -1, max depth)\n" +
                         "    [--verbose (progress messages)\n" +
                         "    [--skip-path-generation] (use the generated path file)\n" +
                         "    [--path-generation-only] (only generate the path file)\n" +
                         "    [--skip-cell=<cell1,cell2> (defaults to empty) no paths analysized for these cells\n" +
                         "    [--skip-cell-jauto=<cell1,cell2> (defaults to empty) the cells are ignored by Jauto\n" +
                         "    [--iteration-number=<1>] (times the previous path slew propagates,default 1)\n" +
                         "    [--iteration-threshold=<1>] (slew progagates only when the slew differs more than, default 1 ps)\n" +
                         "    [ --max-slew=<250>] (max slew, defaults to 250)\n" +
                         "    [ --use-interface-file] (use interface file)\n" +
                         "    [ --input-slew-up=<string> ] (input slew for rising, defaults to 30)\n" +
                         "    [ --input-slew-down=<string> ] (input slew for falling, defaults to 30)\n");
    }

    public Jtimer(String[] args) throws Exception {
        theArgs = commandLineProcess(args);
        allPaths = theArgs.argExists("allpaths");
        ignoreReset = ! theArgs.argExists("include-reset");
        castRoot = theArgs.getArgValue("cast-path", ".");
        fulcrumPdkRoot = theArgs.getArgValue("fulcrum-pdk-root", ".");
        castVersion = theArgs.getArgValue("cast-version", "2");
        depth = Integer.parseInt(theArgs.getArgValue("depth","-1"));
        if(depth < 0)
            depth = -1;
        maxSlew = Float.parseFloat(theArgs.getArgValue("max-slew", "250"));
        defaultInputSlewUp = Float.parseFloat(theArgs.getArgValue("input-slew-up", "20"));
        defaultInputSlewDown = Float.parseFloat(theArgs.getArgValue("input-slew-down", "20"));
        iterationNumber = Integer.parseInt(theArgs.getArgValue("iteration-number","2"));
        DebugOption.printLevel = Integer.parseInt(theArgs.getArgValue("debug-level","10"));
        iterationThreshold = Float.parseFloat(theArgs.getArgValue("iteration-threshold","1.0"));
        //routed = theArgs.argExists("routed");
        skipPathGeneration = theArgs.argExists("skip-path-generation");
        pathGenerationOnly = theArgs.argExists("path-generation-only");
        useInterfaceFile = theArgs.argExists("useInterfaceFile");
        slackFileName = theArgs.getArgValue("slack-file", "/dev/null");
        if (theArgs.argExists("verbose") || DebugOption.printLevel < 10)
            verbose = true;

        topCellName = theArgs.getArgValue("cell", null);
        if (topCellName == null) {
            System.err.println("ERROR: You must specify a cell name.");
            usage();
            System.exit(-1);
        }

        String tau_s = theArgs.getArgValue("tau",null);
        if(tau_s == null){
            System.err.println("WARNING: tau is not specified, used the default number 53");
            tau_s = "53";
        }
        tau=(float) (Float.parseFloat(tau_s));
        
        errFile = new BufferedWriter(new FileWriter(topCellName+".err"));
        
        timingPathMeasuredList = new HashMap();
        interPathSlew = new HashMap();
        ioSlew = new HashMap();

        String skip_cells = theArgs.getArgValue("skip-cell", null);
        skipCells = new HashSet();
        if(skip_cells != null) {
            final StringTokenizer tokenizer = new StringTokenizer(skip_cells, "," );
            while ( tokenizer.hasMoreTokens() ) {
                final String cell_name = tokenizer.nextToken();
                skipCells.add(cell_name);
            }
        }
        
        String skip_cells_jauto = theArgs.getArgValue("skip-cell-jauto", null);
        plt = theArgs.getArgValue("prefix", "plt");
        skipCellsJauto = new HashSet();
        if(skip_cells_jauto != null) {
            final StringTokenizer tokenizer = new StringTokenizer(skip_cells_jauto, "," );
            while ( tokenizer.hasMoreTokens() ) {
                final String cell_name = tokenizer.nextToken();
                skipCellsJauto.add(cell_name);
            }
        }
        
        printStats("Args");
    }

    private CommandLineArgs commandLineProcess(String[] args) {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 
        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
        final CommandLineArgs theArgs = cachedArgs;
        return theArgs;
    }

    public static void main(String[] args) throws Exception {
        Jtimer jtimer = null;
        DebugOption.printLevel = 10;
        try {
            if (verbose) System.err.println(Calendar.getInstance().getTime());
            jtimer = new Jtimer(args);
            jtimer.pathGeneration();
            jtimer.checkTiming();
            jtimer.report();
        }
        catch (Exception e){
            throw new RuntimeException(e);
        }
        finally {
            printStats("Total");
            if(jtimer != null){
                jtimer.errFile.close();
            }
            if (verbose) System.err.println(Calendar.getInstance().getTime());
        }
    }
}
