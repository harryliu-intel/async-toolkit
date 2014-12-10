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

import com.avlsi.util.container.Pair;
import com.avlsi.file.common.HierName;

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



public final class IdleStateSolver
{
    private static Store store;
	
    private BooleanVar[] store_vars;  //References to all the store variables
    static int storevar_cnt;  // Counter for the number of store variables

    private BufferedWriter out;      //File to output all the idle states
    //A Hash Map of Store variable names and their value
    //0- IdLE_0 1 - IDLE_1 and -1 - UNKNOWN
    public HashMap<String,Integer> idleStates; 
    //A hash map to store the transistor Types
    /* 
     * The hash map uses only 2 types (stt - slow transistor type
     * and ftt - fast transistor type)
     * The nodes whose transistor types are unknown are not entered
     * in this hash map. The boolean value is true- for halfoperators 
     * whose DriveDirection is PULLUP and false for those whose 
     * DriveDirection is PULLDOWN
     * Eg entry : <rv,true> : stt
     */
    public Map<Pair<HierName,Boolean>,Integer> transistorTypes;
    
     								

    //Default constructor for the solver
    IdleStateSolver(int varSize){
        store = new Store(varSize);
        store_vars = new BooleanVar[varSize];
        storevar_cnt=0;
        idleStates= new HashMap<String,Integer>();
        transistorTypes = new HashMap<Pair<HierName,Boolean>,Integer>();
    }
    /*
    * Returns a reference to the variable if it already exists in the store
    * Creates a new variable and returns its index if it does not exist
    */
    public BooleanVar getVariable(String Name){
        for (int i =0 ;i <storevar_cnt; i++){
            if(store_vars[i].id.equals(Name)){
                return store_vars[i];
            }
        }
        store_vars[storevar_cnt] = new BooleanVar(store,Name);
        return store_vars[storevar_cnt++];
		
    }
    /*
    * Imposes the passed primitive constraint on the store
    */
    public void addConstraint(PrimitiveConstraint c){
        //System.out.println("The constraint added is :"+c.toString());
        store.impose(c);
    }
	
    /*Adds the constraints for the directives passed from the 
    getIdleState functions
    */
    public void addDirectiveConstraint(String Name, int val){
        int flag=0;
        for (int i =0 ;i <storevar_cnt; i++){
	    if(store_vars[i].id.equals(Name)){
                if(DebugOption.printLevel <= 1){
                    System.out.println("IdleStateSolver:Adding constraint" +
                                       Name+ " = "+val);
                }
                if (val ==1) store.impose(new XeqC(store_vars[i],1));
	          	else store.impose(new Not(new XeqC(store_vars[i],1)));
                flag = 1;
            }
        }
        if (flag==0){
            //System.err.print("IdleStateSolver:addDirectiveConstraint:Invalid ");
            //System.err.println("Constraint (variable name )"+Name+ "!!");
            //System.exit(0);
        }
		
    }
	
    public void printSolution(final String cellName,final int size){
        boolean[] changed = new boolean[size];
        int[] idleStates = new int[size];
        Search<BooleanVar> search = new DepthFirstSearch<BooleanVar>();
        // If there are no store variables present
        if(storevar_cnt == 0){
            System.err.println("IdleStateSolver: printSolutions: No variables present.");
            return;
        }
        SelectChoicePoint<BooleanVar> select =
                        new InputOrderSelect<BooleanVar>(store, store_vars,
	                                    new IndomainMin<BooleanVar>());
        //The listener is added so that the solver returns all the solutions 
        //that satisfy the constraints
        search.getSolutionListener().searchAll(true);
        search.getSolutionListener().recordSolutions(true);        
        boolean result = search.labeling(store, select); 
        if(DebugOption.printLevel <= 1){
            System.out.print("Variables");
            for(int k=0 ; k< storevar_cnt; k++){
                StringBuilder sb = new StringBuilder();
                Formatter formatter  = new Formatter(sb,Locale.US);
                formatter.format("%6s",store_vars[k].id.toString());
                //System.out.print(store_vars[k].id+"\t\t");
                System.out.print(sb.toString());
            }
            System.out.println("");
            //Printing all the solutions 
        }
        for (int i=1; i<=search.getSolutionListener().solutionsNo(); i++){
            if(DebugOption.printLevel <= 1){
                System.out.print("Solution " + i + ": ");
            }
            for (int j=0; j<search.getSolution(i).length; j++){
                StringBuilder sb = new StringBuilder();
                Formatter formatter  = new Formatter(sb,Locale.US);
                formatter.format("%6s",search.getSolution(i)[j]);
                if(DebugOption.printLevel <= 1){
                    System.out.print(sb.toString());
                }
                //System.out.print("\t" + search.getSolution(i)[j] );
                if(changed[j] == false && i>1){
                    if(search.getSolution(i)[j] !=
                       search.getSolution(i-1)[j])
                        changed[j] = true;
                }
            }
            System.out.println();
        }
        /*Calculating the idle states of the nodes from the solutions
        If the value of a variable changes across the solutions, the idle
        state is UNKNOWN else it is 0 or 1*/
        if (result){
            for (int k=0; k< search.getSolution(1).length ;k++){
                if (changed[k]== true){
                    //The Idle state changes across the solutions
                    //in this case, hence its unknown
                    if(DebugOption.printLevel <= 1){
                        System.err.println("IdleState("+ store_vars[k].id+ ")= UNKNN");
                    }
                    this.idleStates.put(store_vars[k].id,new Integer(-1));
                }
                else{
                    int temp;
                    temp = Integer.parseInt(search.getSolution(1)[k].toString());
                    //The Idle state is known in this case
                    this.idleStates.put(store_vars[k].id,new Integer(temp));
                    if(DebugOption.printLevel <= 1){
                        System.err.println("IdleState("+ store_vars[k].id+") = "+
                                           this.idleStates.get(store_vars[k].id).toString());
                    }
                }
            }
        }
        else {
            System.err.println("ERROR: IdleStateSolver: possible conflicting idle_state " +
                               "directives,\nno solutions found in " + cellName);
        }
    }
}
