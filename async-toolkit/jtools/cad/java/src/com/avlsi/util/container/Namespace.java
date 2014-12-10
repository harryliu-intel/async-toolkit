/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

import java.util.Iterator;

import com.avlsi.file.common.HierName;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

/**
 * Wraps an AliasedSet with HierName type safety and convienience routines.
 * 
 * @todo write a higher order function that maps over pairs of
 * (canon node, connected node).
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Namespace {
    /**
     * This class should not be instantiated.
     **/
    private Namespace() { }

    /**
     * Returns a new namespace equivalent to the old one, but with
     * all names prefixed by <code>prefix</code>.
     **/
    public static AliasedSet prefixNamespace(final HierName prefix,
                                             final AliasedSet namespace) {
        final AliasedSet newNamespace =
            new AliasedSet(new NaturalOrderComparator());

        for (final Iterator iCanonNode = namespace.getCanonicalKeys();
             iCanonNode.hasNext(); ) {

            final HierName canonNode = (HierName) iCanonNode.next();
            final HierName prefixedCanonNode =
                HierName.append(prefix, canonNode);
            newNamespace.add(prefixedCanonNode);

            for (final Iterator iConnNode = namespace.getAliases(canonNode);
                 iConnNode.hasNext(); ) {

                final HierName connNode = (HierName) iConnNode.next();
                final HierName prefixedConnNode =
                    HierName.append(prefix, connNode);

                newNamespace.makeEquivalent(prefixedCanonNode,
                                            prefixedConnNode);
            }
        }

        return newNamespace;
    }

    /**
     * Returns a new namespace equivalent to the old one, but with
     * all names prefixed by <code>prefix</code>.  The old values
     * will be copied by reference.
     * XXX This code duplication is really horrible.
     * @todo jmr refactor
     **/
    public static AliasedMap prefixNamespace(final HierName prefix,
                                             final AliasedMap namespace) {
        return prefixNamespace(prefix, namespace, false);
    }

    /**
     * Returns a new namespace equivalent to the old one, but with
     * all names prefixed by <code>prefix</code>.  The old values
     * will be copied by reference.  The newly created names are marked
     * as ports if <code>isPort</code> is true.
     * XXX This code duplication is really horrible.
     * @todo jmr refactor
     **/
    public static AliasedMap prefixNamespace(final HierName prefix,
                                             final AliasedMap namespace,
                                             final boolean isPort) {
        try {
            final AliasedMap newNamespace =
                new AliasedMap(namespace.getMergeFunction(),
                        namespace.getComparator());

            for (final Iterator iCanonNode = namespace.getCanonicalKeys();
                 iCanonNode.hasNext(); ) {

                final HierName canonNode = (HierName) iCanonNode.next();
                final HierName prefixedCanonNode =
                    HierName.makePortName(
                        HierName.append(prefix, canonNode),
                        isPort);
                newNamespace.addData(prefixedCanonNode,
                        namespace.getValue(canonNode));

                for (final Iterator iConnNode =
                        namespace.getAliases(canonNode);
                     iConnNode.hasNext(); ) {

                    final HierName connNode = (HierName) iConnNode.next();
                    final HierName prefixedConnNode =
                        HierName.makePortName(
                            HierName.append(prefix, connNode),
                            isPort);

                    newNamespace.makeEquivalent(prefixedCanonNode,
                                                prefixedConnNode);
                }
            }

            return newNamespace;
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Adds the names and connections in ns2 to those in ns1.
     **/
    public static void addNamespace(final AliasedSet ns1,
                                    final AliasedSet ns2) {

        for (final Iterator iCanonNode = ns2.getCanonicalKeys();
             iCanonNode.hasNext(); ) {

            final HierName canonNode = (HierName) iCanonNode.next();

            ns1.add(canonNode);

            for (final Iterator iConnNode = ns2.getAliases(canonNode);
                 iConnNode.hasNext(); ) {

                final HierName connNode = (HierName) iConnNode.next();

                ns1.makeEquivalent(canonNode, connNode);
            }
        }
    }

    /**
     * Adds the names and connections in ns2 to those in ns1.
     * XXX This code duplication is a really horrible hack.
     * @todo jmr refactor
     **/
    public static void addNamespace(final AliasedMap ns1,
                                    final AliasedMap ns2)
        throws AliasedMap.MergeFailedException {

        for (final Iterator iCanonNode = ns2.getCanonicalKeys();
             iCanonNode.hasNext(); ) {

            final HierName canonNode = (HierName) iCanonNode.next();

            ns1.addData(canonNode, ns2.getValue(canonNode));

            for (final Iterator iConnNode = ns2.getAliases(canonNode);
                 iConnNode.hasNext(); ) {

                final HierName connNode = (HierName) iConnNode.next();

                ns1.makeEquivalent(canonNode, connNode);
            }
        }
    }

    /**
     * @deprecated Just use AliasedSet.toString.
     **/
    public static String toString(final AliasedSet ns) {
        return ns.toString();
    }
}
