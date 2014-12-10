/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

import java.util.Collection;
import java.util.Iterator;
import java.util.PriorityQueue;

import com.avlsi.util.functions.UnaryPredicate;

public class GraphUtil {
    static void relax(Edge e,
               PriorityQueue<WeightedVertex> queue,
               ShortestPaths paths) {
        double srcDist = paths.getDistance(e.getSrc(),
                                           Double.POSITIVE_INFINITY);
        assert srcDist != Double.POSITIVE_INFINITY;
        double snkDist = paths.getDistance(e.getSnk(), 
                                           Double.POSITIVE_INFINITY);
        double newDist = e.relax(srcDist, snkDist);

        if(newDist >= 0) {
            WeightedVertex wv;
            if(snkDist == Double.POSITIVE_INFINITY) {
                wv = new WeightedVertex(e.getSnk(), newDist);
            } else {
                wv = new WeightedVertex(e.getSnk(),snkDist);
                queue.remove(wv);
                wv.setWeight(newDist);
            }
            queue.add(wv);
            paths.updateVertex(e.getSnk(), e.getSrc(), newDist);
        }
    }

    public static ShortestPaths singleSrcShortestPaths(Vertex src, double d) {
        final PriorityQueue<WeightedVertex> queue =
            new PriorityQueue<WeightedVertex>();
        final ShortestPaths paths = new ShortestPaths(src);

        WeightedVertex weightedSrc = new WeightedVertex(src, 0.0);

        queue.add(weightedSrc);

        while(!queue.isEmpty()) {
            final WeightedVertex curr = queue.poll();
            if(curr.getWeight() > d) {
                break;
            }
            Vertex currVert = curr.getVertex();
            currVert.foreachEdge(
                new UnaryPredicate<Edge>(){
                    public boolean evaluate(Edge e) {
                        relax(e, queue, paths);
                        return true;
                    }
                });
        }
        return paths;
    }

    public static ShortestPaths singleSrcShortestPaths
        (Vertex src, double d,  Collection<Edge> excludePath) {
        for(Edge e : excludePath) {
            Vertex u = e.getSrc();
            Vertex v = e.getSnk();
            Vertex du = (u.equals(src) ? u : u.duplicate());
            Vertex dv = v.duplicate();
            u.deleteEdge(v);
            du.addEdge(dv);
        }

        ShortestPaths paths = singleSrcShortestPaths(src,d);

        for(Edge e : excludePath) {
            Vertex u = e.getSrc();
            Vertex v = e.getSnk();
            Vertex du = (u.equals(src) ? u : u.duplicate());
            Vertex dv = v.duplicate();
            u.restore();
            v.restore();
            du.restore();
            dv.restore();
        }
        return paths;
    }
}