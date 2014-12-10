package com.avlsi.tools.jauto;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Formatter;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.Map;
import java.util.List;
import java.util.HashSet;

import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.DirectiveUtils.IdleState;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.HalfOperator;
import com.avlsi.io.FileSearchPath;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.BinaryPredicate;
import JaCoP.constraints.And;
import JaCoP.constraints.Not;
import JaCoP.constraints.Or;
import JaCoP.constraints.PrimitiveConstraint;
import JaCoP.constraints.XeqC;
import JaCoP.core.BooleanVar;
import JaCoP.core.Store;
import JaCoP.search.DepthFirstSearch;
import JaCoP.search.IndomainMin;
import JaCoP.search.InputOrderSelect;
import JaCoP.search.Search;
import JaCoP.search.SelectChoicePoint;

public final class AutoThreshold {
    public static enum Option {
        FAST_NON_LEAKY,
        FAST_2OFF_NMOS,
        FAST_2OFF_PMOS,
        SLOW_1OFF_NMOS,
        SLOW_1OFF_PMOS,
        FAST_VS_WEAK_ALLOWED,
        SLOW_VS_WEAK_ALLOWED;
    };
	
    static IdleStateSolver s;
    static int minOffTransistors = 2;	
    //Returns AND of all gates in one path
    private static PrimitiveConstraint expandLogic(final NetGraph.NetPath path,
           final boolean invert, final boolean print) {
        final Set<NetGraph.NetNode> gates =
            (Set<NetGraph.NetNode>) path.getGateNodes();
        boolean first = true;
        
        
        PrimitiveConstraint c_path = null;
        ArrayList<PrimitiveConstraint> c_gates =
            new ArrayList<PrimitiveConstraint>();
        BooleanVar Var;
      
        for (NetGraph.NetNode gate : gates) {
            if (first)
                first = false;
            else{
                if(DebugOption.printLevel <= 1){
                    if (print)	System.out.print(" & ");
                }
            }   
            Var = s.getVariable(gate.name.toString());
            
            if (invert)
                c_gates.add(new Not(new XeqC(Var,1)));
            else
                c_gates.add(new XeqC(Var,1));
            if(print && DebugOption.printLevel <= 1){
                 System.out.print((invert ? "~" : "") + gate.name);
            }
            
        }
        c_path = new And(c_gates);  
       	
        return c_path;
    }
   // Returns OR of all paths in one half operator
    private static PrimitiveConstraint expandLogic(
           final Collection<NetGraph.NetPath> paths,
           final boolean invert ,final boolean print){
        boolean first = true;
        PrimitiveConstraint c_half ;
        ArrayList<PrimitiveConstraint> c_paths = 
            new ArrayList<PrimitiveConstraint>();;
       
        for (NetGraph.NetPath path : paths) {
            final String lead;
            if (first) {
                first = false;
            }
	    else {
                if(print && DebugOption.printLevel <= 1){
                     System.out.print(" | ");
                }
            }
            c_paths.add(expandLogic(path, invert,print));
        }
        c_half = new Or(c_paths);
        return c_half;
    }

    /* Adds the final constraint for each node
    For any node X pulluplogic -> X+ pulldownlogic -> X-
    The constraint will be ( ~pulluplogic & ~X)|(~pulldownlogic & X) = T
    */
    private static void expandLogic(final NetGraph.NetNode node,boolean print){
        final Collection<NetGraph.NetPath> up =
            new ArrayList<NetGraph.NetPath>();
        final Collection<NetGraph.NetPath> dn =
            new ArrayList<NetGraph.NetPath>();

        for (Iterator j = node.getLogicPaths().iterator(); j.hasNext(); ) {
            final NetGraph.NetPath path = (NetGraph.NetPath) j.next();
            final int dir = path.getType();
            if (dir == DeviceTypes.N_TYPE) {
                dn.add(path);
            } else {
                assert dir == DeviceTypes.P_TYPE;
                up.add(path);
            }
        }

        final boolean comb = node.isCombinational();
    
        
        BooleanVar Var= s.getVariable(node.name.toString());
        PrimitiveConstraint c_pullUp =null,c_pullDown = null;
        PrimitiveConstraint c_node,c1,c2;
       
        if (!up.isEmpty()){
            c_pullUp = expandLogic(up, true ,print);
            if(print && DebugOption.printLevel <= 1){
                System.out.println(" " + (comb ? "=>" : "->") + " " +
                        node.name + "+");
            }
        }
        if (!dn.isEmpty()) {
            if (!comb){
                c_pullDown = expandLogic(dn, false ,print);
        	    if (print && DebugOption.printLevel <= 1){
                    System.out.println(" -> " + node.name + "-");
                }
            }
            else{
                c_pullDown = expandLogic(dn, false ,false);
            }
        }
    	if (c_pullUp!= null)
    	    c1  = new And(new Not(c_pullUp),new Not(new XeqC(Var,1)));
    	else
    	    c1  = new Not(new XeqC(Var,1));
    	if(c_pullDown!= null)
    	    c2 = new And(new Not(c_pullDown),new XeqC(Var,1));
    	else
    	    c2 = new XeqC(Var,1);
    	c_node = new Or (c1,c2);
    	s.addConstraint(c_node);
    }
    private static boolean getOptionValue(Option key,Map <Option,Boolean> options){
        boolean val = false;
        if(options.containsKey(key)){
            if(options.get(key)) val = true;
        }
        return val;

    }

    /*
    Counts the off transistors in a single path. Checks if at least 2 are OFF
    For pullup path ( invert = true) check for the gate to be equal to 1 and 
    for pulldown path( invert = false) check for the gate to be equal to 0 */
    private static boolean getTransistorType(final NetGraph.NetPath path,
            final boolean invert) {
        final Set<NetGraph.NetNode> gates =
            (Set<NetGraph.NetNode>) path.getGateNodes();
       
        BooleanVar Var;
        int countOffs=0;
        boolean minOffs = false;
  
        for (NetGraph.NetNode gate : gates) {
            Var = s.getVariable(gate.name.toString());
            if (s.idleStates.containsKey(Var.id)) {
                if(s.idleStates.get(Var.id) == 1 && invert) countOffs++;
            	else if (s.idleStates.get(Var.id) == 0 && !invert) countOffs++;
            }
        }
        if (countOffs >= minOffTransistors) minOffs=true;
        return minOffs;
    }
    /* Checks if all the paths for the half operator have 2 or more than 2 
    off transistors*/
    private static boolean getTransistorType(final Collection<NetGraph.NetPath> paths,
                                   final boolean invert) {
        boolean offAllPaths = false;
        int tempCountOffs=0;
        boolean temp;
        for (NetGraph.NetPath path : paths) {
            temp = getTransistorType(path, invert);
            if(temp) tempCountOffs++;
        }
        //If all paths on the half operator have 2 or more than 2 
        //transistors off
        if (tempCountOffs == paths.size()) offAllPaths = true;;
        return offAllPaths;
    }
    /* Finds the transistor type. For idle state of X = 1 the X+ = LVT and 
    X-= HVT and for idle state of X = 0 X+ = HVT and X- = LVT. The HVT operator
    is again checked for the off transistor condition and can be assigned LVT 
    if the ocndition is satisfied*/
    private static void getTransistorType(
            final String cellName,                              
            final NetGraph.NetNode node,
            final int stt,
            final int tt,
            final int ftt,
            final Map<HierName,Integer> ttup, 
            final Map<HierName,Integer> ttdn,
            final Map<HierName,DirectiveUtils.IdleState> is,
            final Map<Option, Boolean> options){
        final Collection<NetGraph.NetPath> up =
            new ArrayList<NetGraph.NetPath>();
        final Collection<NetGraph.NetPath> dn =
            new ArrayList<NetGraph.NetPath>();

      
        for (Iterator j = node.getLogicPaths().iterator(); j.hasNext(); ) {
            final NetGraph.NetPath path = (NetGraph.NetPath) j.next();
            final int dir = path.getType();
            if (dir == DeviceTypes.N_TYPE) {
                dn.add(path);
            } else {
                assert dir == DeviceTypes.P_TYPE;
                up.add(path);
            }
        }

      
        final BooleanVar Var= s.getVariable(node.name.toString());
       
        
        boolean fastnonleaky = getOptionValue(Option.FAST_NON_LEAKY,options);
        boolean fast2offpmos = getOptionValue(Option.FAST_2OFF_NMOS,options);
        boolean fast2offnmos = getOptionValue(Option.FAST_2OFF_PMOS,options);

        boolean slow1offpmos = getOptionValue(Option.SLOW_1OFF_PMOS,options);
        boolean slow1offnmos = getOptionValue(Option.SLOW_1OFF_NMOS,options);
        boolean fastvsweakallowed = getOptionValue(Option.FAST_VS_WEAK_ALLOWED,options);
        boolean slowvsweakallowed = getOptionValue(Option.SLOW_VS_WEAK_ALLOWED,options);


        Pair<HierName,Boolean> pairUp,pairDown;
        boolean isLibraryGate = (node.getGate() != null);
        boolean hasWeakFeedBackUp = node.hasWeakFeedBack(DeviceTypes.N_TYPE);
        boolean hasWeakFeedBackDown = node.hasWeakFeedBack(DeviceTypes.P_TYPE);
        boolean upfastallowed = true;
        boolean upslowallowed = true;
        boolean downfastallowed = true;
        boolean downslowallowed = true;

        if(hasWeakFeedBackUp && !fastvsweakallowed) upfastallowed = false;
        if(hasWeakFeedBackUp && !slowvsweakallowed) upslowallowed = false;
        if(hasWeakFeedBackDown && !fastvsweakallowed) downfastallowed = false;
        if(hasWeakFeedBackDown && !slowvsweakallowed) downslowallowed = false;

    	if (s.idleStates.containsKey(Var.id)){

            pairUp = new Pair<HierName,Boolean>( node.name,true);
            pairDown = new Pair<HierName,Boolean>(node.name,false);
            // IdleState of this node is IDLE_1 
            if (s.idleStates.get(Var.id) ==1){

                // Non Leaky Direction
                if(fastnonleaky && upfastallowed){
                    if(!ttup.containsKey(node.name)) s.transistorTypes.put(pairUp,ftt);
                    System.out.println("TransType("+Var.id+
                                        "+)=FAST");
                }
                else{
                    //if(!ttup.containsKey(node.name)) s.transistorTypes.put(pairUp,tt);
                    System.out.println("TransType("+Var.id+
                                        "+)=DEF");

                }
                //Leaky Direction
                boolean offDown2 = false, offDown1 = false;
                if (isLibraryGate) {
                    minOffTransistors = 2;
                    offDown2 = getTransistorType(dn, false);
                    if(!offDown2){
                        minOffTransistors = 1;
                        offDown1 = getTransistorType(dn, false);
                    }
                }
                if(fast2offnmos && offDown2 && downfastallowed){
                    if(!ttdn.containsKey(node.name)) s.transistorTypes.put(pairDown,ftt);
                    System.out.println("TransType("+Var.id+
                                        "-)=FAST");

                }
        
                else if(slow1offnmos && offDown1 && downslowallowed){
                    if(!ttdn.containsKey(node.name)) s.transistorTypes.put(pairDown,stt);
                    System.out.println("TransType("+Var.id+
                                        "-)=SLOW");

                }
                else{
                    //if(!ttdn.containsKey(node.name)) s.transistorTypes.put(pairDown,tt);
                    System.out.println("TransType("+Var.id+
                                        "-)=DEF");
                }
                
            }
            else if (s.idleStates.get(Var.id)==0){
                //Non-leaky direction
                if(fastnonleaky && downfastallowed){
                     if(!ttdn.containsKey(node.name)) s.transistorTypes.put(pairDown,ftt);
                     System.out.println("TransType("+Var.id+
                                        "-)=FAST");
                }
                else{
                     //if(!ttdn.containsKey(node.name)) s.transistorTypes.put(pairDown,tt);
                     System.out.println("TransType("+Var.id+
                                        "-)=DEF");
                }
                //Leaky direction
                boolean offUp2 = false, offUp1 = false;
                if (isLibraryGate) {
                    minOffTransistors = 2;
                    offUp2 = getTransistorType(up, true);
                    if(!offUp2){
                        minOffTransistors = 1;
                        offUp1= getTransistorType(up, true);
                    }
                }
                if(fast2offpmos && offUp2 && upfastallowed){
                    if(!ttup.containsKey(node.name)) s.transistorTypes.put(pairUp,ftt);
                    System.out.println("TransType("+Var.id+
                                        "+)=FAST");

                }
                else if(slow1offpmos && offUp1 && upslowallowed){
                    if(!ttup.containsKey(node.name)) s.transistorTypes.put(pairUp,stt);
                    System.out.println("TransType("+Var.id+
                                        "+)=SLOW");

                }
                else{
                    //if(!ttup.containsKey(node.name)) s.transistorTypes.put(pairUp,tt);
                    System.out.println("TransType("+Var.id+
                                        "+)=DEF");
                }


            }
            else
            {
                boolean unknown = true;
                if(is.containsKey(node.name)){
                    IdleState id= is.get(node.name);
                     if (id == IdleState.IDLE_UNKNOWN) unknown=false;
                }

            	if(unknown) System.err.println("WARNING TransType ("+
                                   Var.id+"+) = UNKNOWN in "+cellName 
                                   +"\nWARNING TransType ("+
                                   Var.id+"-) = UNKNOWN in "+ cellName);
            }
    	}   
    }
    private static void resolveSharingConflict(
            final List<HalfOperator> halfops,
            final int stt,
            final int ftt,
            final Map<HierName,Integer> ttup,
            final Map<HierName,Integer> ttdn,
            final String cellName){
        // s is the hash map of Map<Pair<HierName,Boolean>,Integer>
        /* The last step is to check if any of the half operators are sharing
         * transistor. If they are sharing and if there is a conflict in 
         * their transistor types, then the method should make a 
         * conservative decision and assign HVT or Slow transistor type in 
         * this case */

         //halfOpSharings : A list of lists. Each list is a list of Haloperators that share transistors.
        //Its a partition of the hals operators from the cell formed by the
        //transitive closure of the isSharingWith Relation
        Collection<HalfOperator> coll = halfops;
        Collection<Collection<HalfOperator>> halfOpSharings = 
            CollectionUtils.computeTransitiveClosure(coll,
                    new BinaryPredicate<HalfOperator,HalfOperator>() {
                        public boolean evaluate(final HalfOperator a,final HalfOperator b){
                            return a.isSharingWith(b);
                        }
                    });
        /*For each such list/group in the halfOpSharings
         * Check if there is a conflict in any of the 
         * halfoperators.
         * If there is, change the transistor types to SLOW
         * This is applicable only for the halfoperators that do no have user defined
         * transistor types*/
        for (final Collection<HalfOperator>  hoList: halfOpSharings){
            boolean conflict = false;
            int prev = -1;
            for(final HalfOperator ho: hoList){
                int curr = -1;
                final NetGraph.NetNode node = 
                        ho.outputNode;
                Pair<HierName,Boolean> hPair;
                if(ho.driveDirection == HalfOperator.DriveDirection.PULL_UP){
                    hPair = new Pair<HierName,Boolean>(node.name,true);
                    if(ttup.containsKey(node.name))
                        curr = ttup.get(node.name).intValue();
                }
                else{
                    hPair = new Pair<HierName,Boolean>(node.name,false);
                    if(ttdn.containsKey(node.name))
                        curr = ttdn.get(node.name).intValue();
                }
                if (s.transistorTypes.containsKey(hPair)){
                    curr = s.transistorTypes.get(hPair).intValue();
                }
                if(prev!= -1 && curr!=prev){
                    conflict = true;
                    break;
                }
            }
            /*There is a conflict in the group so change the 
             * transistor types of all the half operators
             * in this group to SLOW*/
            if (conflict){
                System.out.println("There was a conflict");
                for(final HalfOperator ho:hoList){
                    final NetGraph.NetNode node = 
                            ho.outputNode;
                    Pair<HierName,Boolean> hPair;
                    if(ho.driveDirection == HalfOperator.DriveDirection.PULL_UP){
                        hPair = new Pair<HierName,Boolean>(node.name,true);
                    }
                    else{
                        hPair = new Pair<HierName,Boolean>(node.name,false);
                    }
                    if (s.transistorTypes.containsKey(hPair)){
                        s.transistorTypes.put(hPair,stt);
                    }
                    else{
                        System.err.println("ERROR :Conflict with a user defined "+
                                     "transistor type for a HalfOperator"+
                                     cellName);
                    }
                }
            }

        }
       
    }    

    public static Map<Pair<HierName,Boolean>,Integer>  getTransistorTypesMap(
              final String cellName,                                                               
              final Map<HierName,DirectiveUtils.IdleState> is,
              final NetGraph graph,
              final int stt, 
              final int tt,
              final int ftt,
              final Map<HierName,Integer> ttup,
              final Map<HierName,Integer> ttdn,
              final List<HalfOperator> halfops,
              final Map <Option,Boolean> options){
    

        ///Get all the nodes from the Half Operators list 
       
        final Set<NetGraph.NetNode>  nodeList =
                     new HashSet<NetGraph.NetNode>();
        for (final HalfOperator ho : halfops) {
            final NetGraph.NetNode node = (NetGraph.NetNode) ho.outputNode;
            if (!node.isOutput() || !node.isNamed()) continue;
            if(!nodeList.contains(node))
                nodeList.add(node);
        }


        final int size = graph.getNodes().size();
        boolean print = true;
        s = new IdleStateSolver(size);

        //Expand the PRS, Add variables to the JaCoP store, add 
        //the constraints

        for (final NetGraph.NetNode node : nodeList){
            expandLogic(node,print);
        }

        
    	//Add the constraints from the directives 
        for (final HierName var : is.keySet()){
            IdleState id= is.get(var);
            //System.out.println("The var is :"+var.toString()+"="+ id);
            if (id == IdleState.IDLE_0)
                s.addDirectiveConstraint(var.toString(),0);
            else if(id == IdleState.IDLE_1)
                s.addDirectiveConstraint(var.toString(),1);
        }

        // Solve the PRS equations and get the Idle states
        s.printSolution(cellName,size);



        //Get the Transistor Type directives
       
        for (final NetGraph.NetNode node : nodeList){
            getTransistorType(cellName,node,stt,tt,ftt,ttup,ttdn,is,options);
        }
        
       resolveSharingConflict(halfops,stt,ftt,ttup,ttdn,cellName); 
        return s.transistorTypes;

       
    }
}


