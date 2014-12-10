/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.graph.Edge;
import com.avlsi.util.graph.GraphUtil;
import com.avlsi.util.graph.ShortestPaths;
import com.avlsi.util.graph.Vertex;

class SlewChecker {
    private final HalfOpGraph circuit;
    private SlewCheck outputSlewCheck;
    private SlewCheck inputSlewCheck;
    private SlewCheck staticizerSlewCheck;

    private final HashMap<String, Double> slowSlews;
    private final HashMap<String, Double> fastSlews;

    SlewChecker(DSim dsim) {
        circuit = new HalfOpGraph(dsim);
        outputSlewCheck = new OutputSlewCheck();
        inputSlewCheck = new InputSlewCheck();
        staticizerSlewCheck = new StaticizerSlewCheck();
        slowSlews = new HashMap<String, Double>();
        fastSlews = new HashMap<String, Double>();
    }

    void computeSlowSlews(final double initSlew) {
        final double initSlewPs = HalfOpEdge.toPs(initSlew);
        slowSlews.clear();
        slowSlews.putAll(relaxSlews(initSlewPs, false));
        // TODO(piyush): This method should be renamed.
        // evaluate slowPaths with slow delays here.
        outputSlewCheck.evaluateSlowPaths(circuit, slowSlews);
        inputSlewCheck.evaluateSlowPaths(circuit, slowSlews);
        staticizerSlewCheck.evaluateSlowPaths(circuit, slowSlews);
    }

    void computeFastSlews(final double initSlew) {
        final double initSlewPs = HalfOpEdge.toPs(initSlew);
        fastSlews.clear();
        fastSlews.putAll(relaxSlews(initSlewPs, true));
    }

    private Map<String, Double> relaxSlews(
        final double initSlew, boolean doMin) {
        circuit.init();
        HashMap<String, Double> slews = new HashMap<String, Double>();
        for(int i=0; i<10; i++) {
            slews = relax(slews, initSlew, doMin);
        }
        return slews;
    }

    private HashMap<String, Double> relax(
        final HashMap<String, Double> slews, 
        final double initSlew, 
        final boolean doMin) {
        final HashMap<String, Double> nextSlews = 
            new HashMap<String, Double>();
        circuit.foreachVertex(
            new UnaryPredicate<Vertex>() {
                @Override public boolean evaluate(Vertex v) {
                    v.foreachEdge(
                        new UnaryPredicate<Edge>() {
                            @Override public boolean evaluate(Edge e) {
                                final String key = e.getSnk().toString();
                                final double inputSlewPs = 
                                    (slews.containsKey(key)) ? 
                                    HalfOpEdge.toPs(slews.get(key)) : initSlew;
                                final double nextSlewPs = 
                                    ((HalfOpEdge) e).getSlew(inputSlewPs, -1);
                                final double nextSlew = 
                                    HalfOpEdge.toDSim(nextSlewPs);
                                boolean updateSlew  = nextSlewPs != -1 && 
                                    (!nextSlews.containsKey(key) || 
                                     ((nextSlews.get(key) > nextSlew) && 
                                      doMin) ||
                                     ((nextSlews.get(key) < nextSlew) && 
                                      !doMin));
                                if(updateSlew) {
                                    nextSlews.put(key, nextSlew);
                                }
                                return true;
                            }
                        });
                    return true;
                }
            });
        return nextSlews;
    }

    private void initializeSlews(final Map<String, Double> slews, 
                                 final double slew) {
        slews.clear();
        circuit.foreachVertex(
            new UnaryPredicate<Vertex>() {
                @Override public boolean evaluate(Vertex v) {
                    HalfOp h = (HalfOp) v;
                    slews.put(h.toString(), slew);
                    return true;
                }
            });      
    }

    void setSlowSlews(final double inputSlew) {
        initializeSlews(slowSlews, inputSlew);
    }

    void setFastSlews(final double inputSlew) {
        initializeSlews(fastSlews, inputSlew);
    }

    //TODO(piyush) : rename methods and dsim command line
    void saveSlowSlews(ObjectOutputStream oos) {
        try {
            oos.writeObject(slowSlews);
            oos.writeObject(outputSlewCheck);
            oos.writeObject(inputSlewCheck);
            oos.writeObject(staticizerSlewCheck);
        } catch (IOException e) {
            System.out.println("Error saving slews: " + e.getMessage());
        }
    }

    void saveFastSlews(ObjectOutputStream oos) {
        try {
            oos.writeObject(fastSlews);
        } catch (IOException e) {
            System.out.println("Error saving slews: " + e.getMessage());
        }
    }

    private HashMap<String, Double> readSlews(HashMap<String, Double> slews,
                                              ObjectInputStream ois) {
        circuit.init();
        slews.clear();
        HashMap<String, Double> read = null;
        try {
            read = (HashMap<String, Double>) ois.readObject();
        } catch (IOException e) {
            System.out.println("Error reading slews: " + e.getMessage());
        } catch (ClassNotFoundException e) {
            System.out.println("Error reading slews: " + e.getMessage());
        }
        if (read != null) {
            slews.putAll(read);
        }
        return read;
    }

    void readFastSlews(ObjectInputStream ois) {
        readSlews(fastSlews, ois);
    }

    void readSlowSlews(ObjectInputStream ois) {
        readSlews(slowSlews, ois);
        try {
            outputSlewCheck = (OutputSlewCheck) ois.readObject();
            inputSlewCheck = (InputSlewCheck) ois.readObject();
            staticizerSlewCheck = (StaticizerSlewCheck) ois.readObject();
        } catch (IOException e) {
            System.out.println("Error reading slews: " + e.getMessage());
        } catch (ClassNotFoundException e) {
            System.out.println("Error reading slews: " + e.getMessage());
        }
    }

    void evaluate(final double absMargin,
                  final double multMargin,
                  final boolean checkIsochronicForks) {
        // TODO(piyush): Warning if slews table is empty, i.e input slews
        //               not initialized.
        System.out.println(
            "Slew checker started evaluating at  " +
            new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date()));

        circuit.foreachVertex(
            new UnaryPredicate<Vertex>() {
                @Override public boolean evaluate(Vertex v) {
                    double[] slowDelays = new double[3];
                    slowDelays[0] = outputSlewCheck.getSlowDelay(v);
                    slowDelays[1] = inputSlewCheck.getSlowDelay(v);
                    slowDelays[2] = staticizerSlewCheck.getSlowDelay(v);

                    Arrays.sort(slowDelays);
                    double slowDelay = slowDelays[2];

                    if ((slowDelay < 0) ||
                        !fastSlews.containsKey(v.toString())) {
                        return true;
                    }
                    double effectiveSlowDelay = 
                        SlewCheck.computeEffectiveDelay(slowDelay, absMargin, 
                                                        multMargin);
                    ((HalfOp) v).setSlew
                        (HalfOpEdge.toPs(fastSlews.get(v.toString())));
                    ShortestPaths paths = 
                        GraphUtil.singleSrcShortestPaths
                        (v, effectiveSlowDelay, 
                         circuit.getExcludePaths((HalfOp)v));

                    outputSlewCheck.checkViolations(circuit,
                                                    (HalfOp) v, paths, 
                                                    absMargin, multMargin, 
                                                    checkIsochronicForks);
                    inputSlewCheck.checkViolations(circuit, (HalfOp) v, paths,
                                                   absMargin, multMargin,
                                                   checkIsochronicForks);
                    staticizerSlewCheck.checkViolations(circuit,
                                                        (HalfOp) v, paths,
                                                        absMargin, multMargin,
                                                        checkIsochronicForks);
                    return true;
                }
            });
        System.out.println(
            "Slew checker finished evaluating at  " +
            new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date()));
    }

    void printViolations(PrintWriter pwOutSlew, PrintWriter pwIOSlew,
                         PrintWriter pwStatSlew, PrintWriter pwAll, 
                         boolean reportVictimsOnce) {
        boolean passed = 
            outputSlewCheck.printViolations(pwOutSlew, reportVictimsOnce) &
            inputSlewCheck.printViolations(pwIOSlew, reportVictimsOnce) &
            staticizerSlewCheck.printViolations(pwStatSlew, reportVictimsOnce);
        String result = (passed ? "PASS" : "FAIL");
        pwAll.println(result);

    }

    private void printSlews(Map<String, Double> slews, PrintWriter pw) {
        for (Map.Entry<String, Double> slew : slews.entrySet()) {
            pw.println(slew.getKey() + " "  + slew.getValue());
        }
    }

    void printFastSlews(PrintWriter pw) {
        printSlews(fastSlews, pw);
    }

    void printSlowSlews(PrintWriter pw) {
        printSlews(slowSlews, pw);
    }
}