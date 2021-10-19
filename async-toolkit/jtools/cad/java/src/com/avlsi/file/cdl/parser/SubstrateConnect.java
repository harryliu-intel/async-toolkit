package com.avlsi.file.cdl.parser;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.file.common.HierName;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.container.Pair;

public class SubstrateConnect {
    private final Template templates;
    public SubstrateConnect(final Template templates) {
        this.templates = templates;
    }

    private String getSpecialized(final String subName, final HierName subPort,
                                  Template orig) {
        final String name = orig == null ? subName + "_substrate" : subName;
        Template specialized = templates.getTemplate(name);
        if (specialized == null) {
            final boolean top = orig != null;
            if (orig == null) orig = templates.getTemplate(subName);
            specialized = new Template(templates.getTemplates());
            orig.execute(new SpecializeSubckt(specialized, subPort, top),
                         NullEnvironment.getInstance(),
                         name);
        }

        return name;
    }

    private class SpecializeSubckt extends CDLFactoryFilter {
        private final boolean top;
        private final HierName subPort;
        public SpecializeSubckt(final Template t, final HierName subPort,
                                final boolean top) {
            super(t);
            this.subPort = subPort;
            this.top = top;
        }
        public void beginSubcircuit(String subName, String[] in, String[] out,
                                    Map parameters, Environment env) {
            if (!top) {
                final String[] augmented = new String[out.length + 1];
                System.arraycopy(out, 0, augmented, 0, out.length);
                augmented[out.length] = subPort.getAsString('.');
                out = augmented;
            }
            inner.beginSubcircuit(subName, in, out, parameters, env);
        }
        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters, Environment env) {
            final HierName[] augmented = new HierName[args.length + 1];
            System.arraycopy(args, 0, augmented, 0, args.length);
            augmented[args.length] = subPort;
            inner.makeCall(name, getSpecialized(subName, subPort, null),
                           augmented, parameters, env);
        }
        public void makeTransistor(HierName name, String type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                                   Map parameters, Environment env) {
            if (Character.toLowerCase(type.charAt(0)) == 'n') {
                nb = subPort;
            }
            inner.makeTransistor(name, type, ns, nd, ng, nb, w, l, parameters, env);
        }
    }

    public void makeConnections(String subName, HierName subPort) {
        final Template t = templates.getTemplate(subName);
        final Pair<String[],String[]> ports = t.getArguments();
        final String port = subPort.getAsString('.');
        if (Arrays.asList(ports.getFirst()).contains(port) ||
            Arrays.asList(ports.getSecond()).contains(port)) {
            templates.removeTemplate(subName);
            getSpecialized(subName, subPort, t);
        } else {
            throw new IllegalArgumentException(
                subPort + " is not a port of " + subName);
        }
    }

    private static void getDirectives(final CellInterface cell,
                                      final Set<String> seen,
                                      final Map<String,HierName> result) {
        final HierName connectSubstrate = 
            (HierName) DirectiveUtils.getTopLevelDirective(
                cell, DirectiveConstants.CONNECT_SUBSTRATE);
        if (connectSubstrate == null) {
            for (Pair<HierName,CellInterface> p :
                    new IterableIterator<>(cell.getLocalSubcellPairs())) {
                final CellInterface subcell = p.getSecond();
                if (seen.add(subcell.getFullyQualifiedType())) {
                    getDirectives(subcell, seen, result);
                }
            }
        } else {
            result.put(cell.getFullyQualifiedType(), connectSubstrate);
        }
    }

    public static Map<String,HierName> getDirectives(final CellInterface cell) {
        final Map<String,HierName> result = new HashMap<>();
        getDirectives(cell, new HashSet<>(), result);
        return result;
    }
}
