/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.io.PrintWriter;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeSet;

import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.graph.GraphUtil;
import com.avlsi.util.graph.ShortestPaths;
import com.avlsi.util.graph.Vertex;

public class CutoffChecker {
    private final HalfOpGraph circuit;

    public CutoffChecker(DSim dsim) {
        circuit = new HalfOpGraph(dsim) {
            @Override public HalfOpEdge newHalfOpEdge(final HalfOp src,
                                                      final HalfOp snk,
                                                      final Rule r) {
                return new HalfOpEdge(src, snk, r) {
                    @Override public double getWeight(boolean updateSlew) {
                        // env dummy rules have no prefix
                        return r.prefix == null ? r.getDelay() / 100 : 1.0;
                    }
                };
            }
        };
    }

    public void evaluate(final int tCounts, final PrintWriter pw) {
        circuit.init();

        final Set<Violation> violations = new TreeSet<Violation>();
        circuit.foreachVertex(
            new UnaryPredicate<Vertex>() {
                @Override public boolean evaluate(Vertex v) {
                    ShortestPaths paths = 
                        GraphUtil.singleSrcShortestPaths(v, tCounts + 0.5);
                    checkViolations((HalfOp) v, paths, tCounts, violations);
                    return true;
                }
            });

        pw.println(violations.isEmpty() ? "PASS" : "FAIL");

        for (Violation v : violations) {
            pw.println(v);
        }
    }

    private void checkViolations(final HalfOp src,
                                 final ShortestPaths paths,
                                 final int tCounts,
                                 final Collection<Violation> violations) {
        Collection<Vertex> reached =  paths.getReached();
        HalfOp notSrc = src.not();
        for(Vertex v : reached) {
            HalfOp r = (HalfOp) v;
            if(r.isDuplicate()) {
                continue;
            }

            final int dist = (int) Math.round(paths.getDistance(r, 0));
            if (r.equals(src) || dist > tCounts) {
                continue;
            }

            final HalfOpEdge enableEdge = r.getEdge(notSrc);
            final HalfOpEdge disableEdge = r.not().getEdge(src);
            if (enableEdge != null) {
                int effDist = dist + (int) Math.round(enableEdge.getWeight());
                if (effDist <= tCounts) {
                    violations.add(new Violation(r, notSrc, paths, effDist));
                }
            }
            if (disableEdge != null) {
                int effDist = dist + (int) Math.round(disableEdge.getWeight());
                if (effDist <= tCounts) {
                    violations.add(new Violation(r, src, paths, effDist));
                }
            }
        }
    }

    private static class Violation implements Comparable<Violation> {
        private final LinkedList<String> path;
        private final int transitions;
        Violation(final HalfOp reached, final HalfOp end,
                  final ShortestPaths paths,
                  final int transitions) {
            this.path = rotate(paths.getPath(reached));
            this.transitions = transitions;
        }

        private String strip(Vertex v) {
            String s = ((HalfOp) (v)).getNode().getName().toString();
            if (s.startsWith("x.")) {
                s = s.substring(2);
            }
            return s;
        }

        private LinkedList<String> rotate(LinkedList<Vertex> path) {
            LinkedList<String> result = null;

            final int size = path.size();
            if (size == 0) {
                result = new LinkedList<String>();
            } else {
                Vertex min = null;
                for (Vertex v : path) {
                    if (min == null || v.compareTo(min) < 0) {
                        min = v;
                        result = new LinkedList<String>();
                    }
                    result.add(strip(v));
                }
                int missing = size - result.size();
                
                for (Iterator<Vertex> i =
                        CollectionUtils.take(path.iterator(),
                                             size - result.size());
                     i.hasNext(); ) {
                    result.add(strip(i.next()));
                }
            }

            return result;
        }

        public boolean equals(Object o) {
            return (o instanceof Violation) && compareTo((Violation) o) == 0;
        }

        public int hashCode() {
            return path.hashCode();
        }

        public int compareTo(Violation o) {
            int result = transitions - o.transitions;
            if (result != 0) return result;

            Iterator<String> i = path.iterator();
            Iterator<String> j = o.path.iterator();
            while (i.hasNext() && j.hasNext()) {
                result = i.next().compareTo(j.next());
                if (result != 0) return result;
            }

            if (i.hasNext()) return -1;
            else if (j.hasNext()) return 1;
            else return 0;
        }

        public String toString() {
            return "" + transitions + " " + path;
        }
    }
}
