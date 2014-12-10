/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.common;

import java.util.Collection;
import java.util.Comparator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.HashMap;
import java.util.Iterator;

import com.avlsi.util.debug.Debug;
import com.avlsi.util.container.Pair;
import com.avlsi.util.text.ByteArrayString;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.text.UnrepresentableCharException;

/**
 * Node name class.  The same object is always returned by makeHierName
 * for the same String input.
 * <p> The names are stored with common prefixes shared, also
 * for space efficiency.  For example, if the names "a/b/c" and "a/b/d"
 * were both created, the tree would look like this:
 * <pre>
 *         a
 *         ^
 *         |
 *         |
 *         b
 *        ^ ^
 *       /   \
 *      /     \
 *     c       d
 * </pre>
 * ("a" and "a/b" are automatically created by creating "a/b/c".)
 *
 * @todo jmr make this use weak references
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 * @review kiniry - Reviewed 16-23 July 2002.
 *
 * @bug kiniry 23 July 2002 - HierName is the primary culprit in memory and
 * performance issues for many tools (e.g., PrsToNet).  For example, a moderate
 * run of PrsToNet generates over 30M HierName objects (10s to 100s MB of memory
 * used at a time and 100s MB memory garbage collected throughout the run) and
 * accounts for over 1/3rd of the run-time of the tool.  Many of these calls are
 * via Alias$AliasComparator.compare().
 * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1173">Bug#1173</a>
 **/
public final class HierName implements Comparable {
    private final static byte ORDER_CLASS_MASK = 7;
    private final static byte IS_ANONYMOUS = 4;
    private final static byte IS_GENERATED = 8;
    private final static byte IS_POINTER = 16;
    private final static byte IS_GLOBAL = 32;
    private final static byte IS_PORT = 64;

    private final int flags;
    /**
     * cache mapping the string representation of HierNames to 
     * An interned HierName object.
     **/
    // private static final Map cache = new HashMap();

    /**
     * Pointer to parent HierName, or null if this one has no parent.
     **/
    private final HierName prefix;

    /**
     * The string representing the name, stored as a byte array.
     **/
    private final byte[] name;

    /**
     * Constructor.  Only to be used by append.  Only copies reference
     * to <code>name</code>.
     **/
    private HierName(final HierName prefix, final byte[] name) {
        this.prefix = prefix;
        this.name = name;
        this.flags = initializeFlags();
    }

    /**
     * Constructor.  Only to be used by makeHierName.
     **/
    private HierName(final HierName prefix, final String hierNameString)
        throws InvalidHierNameException
    {
        try {
            this.prefix = prefix;
            this.name = ByteArrayString.fromString(hierNameString);
            this.flags = initializeFlags();
        } catch (UnrepresentableCharException e) {
            throw new InvalidHierNameException
                ("Invalid char in name: " + e.getUnrepresentableChar(), e);
        }
    }

    /**
     * Constructor.  Only to be used by makeHierName.
     * 
     * <pre><jml>
     *   private normal_behavior
     *     requires prefix != null;
     *     requires suffix != null;
     *     requires suffix.prefix != null;
     * </jml></pre>
     **/
    private HierName(final HierName prefix, final HierName suffix) {
        this(prefix, suffix.name);
    }

    private HierName(final HierName clone) {
        this.prefix = clone.prefix;
        this.name = clone.name;
        this.flags = clone.flags | IS_PORT;
    }

    private int initializeFlags() {
        final boolean global =
            ByteArrayString.lastIndexOf(name, (byte) '!') != -1;
        final boolean pointer =
            ByteArrayString.lastIndexOf(name, (byte) '#') == name.length - 1;
        final boolean generated =
            ByteArrayString.indexOf(name, (byte) '#') != -1;
        final boolean anonymous =
            ByteArrayString.indexOf(name, (byte) '$') == 0 ||
            (prefix != null && prefix.isAnonymous());
        return computeOrderClass(global, generated, anonymous) |
               (global ? IS_GLOBAL : 0) |
               (pointer ? IS_POINTER : 0) |
               (generated ? IS_GENERATED : 0);
    }

    /**
     * Determine whether the name is of the format of a global name, 
     * ie it contains '!'.
     **/
    public boolean isGlobal() {
        return (flags & IS_GLOBAL) != 0;
    }

    /**
     * Returns whether this name is marked as a port.  This designation only
     * makes sense in the context of a cell, so use with caution.
     **/
    private boolean isPort() {
        return (flags & IS_PORT) != 0;
    }

    /**
     * Determine whether the name is of the format of a node pointer, 
     * ie it ends in '#'.
     **/
    public boolean isPointer() {
        return (flags & IS_POINTER) != 0;
    }

    /**
     * Determine whether the name is a local name, ie if its prefix is null.
     **/
    public boolean isLocal() {
        return prefix == null;
    }

    /**
     * Determine whether the name is of the format of a generated name, 
     * ie it contains '#'.
     **/
    public boolean isGenerated() {
        return (flags & IS_GENERATED) != 0;
    }

    /**
     * Determine whether any component of this name is the autogenerated name
     * of an anonymous variable, i.e., if any components start with "$" and
     * does not contain "!".
     **/
    public boolean isAnonymous() {
        return (flags & IS_ANONYMOUS) != 0;
    }

    /**
     * Determine whether the name is all digits.  This might indicate a
     * generated name.
     **/
    public boolean isNumeric() {
        final String s = getCadenceString();
        for (int i = 0; i < s.length(); i++) {
            if (!Character.isDigit(s.charAt(i))) return false;
        }
        return true;
    }

    // do we want isGnd, isVdd methods?  I think so (-mid)

    /**
     * Returns true if the suffix string is "GND" or "GND!".
     **/
    public boolean isGND() {
        int i = ByteArrayString.lastIndexOf(name, (byte) '!');
        if (i == -1)
            return ByteArrayString.getString(name).equals("GND");
        else
            return ByteArrayString.getString(name).substring(0,i).equals("GND");
    }

    /**
     * Returns true if the suffix string is "Vdd" or "Vdd!".
     **/
    public boolean isVdd() {
        int i = ByteArrayString.lastIndexOf(name, (byte) '!');
        if (i == -1)
            return ByteArrayString.getString(name).equals("Vdd");
        else
            return ByteArrayString.getString(name).substring(0,i).equals("Vdd");
    }

    public boolean isResetNode() {
        int i = ByteArrayString.lastIndexOf(name, (byte) '!');
        if (i == -1)
            return ByteArrayString.getString(name).equals("_RESET") ||
                ByteArrayString.getString(name).equals("_Reset") ||
                ByteArrayString.getString(name).equals("Reset") ||
                ByteArrayString.getString(name).equals("Reset_");
        else
            return ByteArrayString.getString(name).substring(0,i).equals("_RESET") ||
                ByteArrayString.getString(name).equals("_Reset") ||
                ByteArrayString.getString(name).equals("Reset") ||
                ByteArrayString.getString(name).equals("Reset_");
    }

    /**
     * Returns the number of components in the name.  Ie "a/b/c" has 3.
     **/
    public int getNumComponents() {
        int n = 0;
        for (HierName hn = this; hn != null; hn = hn.prefix)
            ++n;
        return n;
    }

    /**
     * Returns a string representation of the name.  Components are
     * separated by <code>separator</code>.
     *
     * @bug kiniry 23 July 2002 - Hot spot for optimization.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1173">Bug#1173</a>
     *
     * @see String getAsString(String)
     * @see StringBuffer getAsStringBuffer(StringBuffer, char)
     * @see StringBuffer getAsStringBuffer(StringBuffer, String)
     **/
    public String getAsString(final char separator) {
        return getAsStringBuffer(new StringBuffer(), separator).toString();
    }

    public String getAsString( final String seperator ) {
        return getAsStringBuffer( new StringBuffer(), seperator ).toString();
    }

    /**
     * Returns a string representation of the suffix.
     **/
    public String getSuffixString() {
        return ByteArrayString.getString(name);
    }

    public String getSuffixTrimmed() {
        if (name.length <= 1) return getSuffixString();
        return ByteArrayString.getString(
                ByteArrayString.substring(name, 0, name.length-1)
               );
    }

    /**
     * Returns a string representation of the name as a string buffer.
     **/
    private StringBuffer getAsStringBuffer(final StringBuffer sb,
                                           final char separator) {
        if (prefix != null)
            prefix.getAsStringBuffer(sb, separator).append(separator);

        return sb.append(ByteArrayString.getString(name));
    }

    private StringBuffer getAsStringBuffer(final StringBuffer sb,
                                           final String separator) {
        if (prefix != null)
            prefix.getAsStringBuffer(sb, separator).append(separator);

        return sb.append(ByteArrayString.getString(name));
    }

    /**
     * Returns the string that aspice wants to see.  This means that
     * a global a.b.c.foo!ext is output as foo.  Otherwise, 
     * return the dot separated name.
     **/
    public String getAspiceString() {
        if (isGlobal()) {
            final int bangIdx
                = ByteArrayString.lastIndexOf(name, (byte) '!');

            Debug.assertTrue(bangIdx != -1);

            return ByteArrayString.getString(
                    ByteArrayString.substring(name,
                        0, bangIdx));
        } else {
            return getAsString('.');
        }
    }

    /**
     * If this HierName corresponds to an array element, return the
     * base name of the array.  Otherwise, return this HierName.  Only
     * looks at the current level, not at the parent HierName.
     **/
    public HierName getArrayBase() {
        final int braceIdx
            = ByteArrayString.lastIndexOf(name, (byte) '[');
        if (braceIdx == -1) return this;
        return makeSiblingName(this, ByteArrayString.getString(ByteArrayString.substring(name, 0, braceIdx)));
    }

    /**
     * Returns the string that cadence wants to see.  This means that
     * a global a.b.c.foo!ext is output as a.b.c.foo!ext.  
     **/
    public String getCadenceString() {
        return getAsString('.');
    }

    /**
     * Returns the string with the resistive subnet identifier suffix
     * stripped off, i.e. a.b.c.foo:bar is output as a.b.c.foo.
     **/
    public String getResistiveSubnetString() {
        String s = getAsString('.');
        int i = s.indexOf(':');
        if (i == -1)
            return s;
        else
            return s.substring(0,i);
    }

    /**
     * Returns the resistive subnet base HierName.
     **/
    public HierName getResistiveSubnetName() {
        final int colonIdx = ByteArrayString.lastIndexOf(name, (byte) ':');
        if (colonIdx == -1) return this;
        else 
            return intern(new HierName(prefix,
                                ByteArrayString.substring(name,0,colonIdx)));
    }

    /**
     * Used only for debugging
     **/
    public String toString() {
        return getAsString('.');
    }

    /**
     * Makes a HierName.  The String must not contain any .'s or 
     * or non-ascii chars or an IllegalArgumentException will be thrown.
     **/
    public static HierName makeHierName(final String s) {
        final char sep = '.';
        if (s.indexOf(sep) != -1)
            throw new IllegalArgumentException(s + " contains '.'");

        try {
            return HierName.makeHierName(s, sep);
        } catch (InvalidHierNameException e) {
            throw (IllegalArgumentException)
                new IllegalArgumentException("Bad HierName " + s)
                    .initCause(e);
        }
    }

    /**
     * Factory to make a HierName for the given hierNameString.  
     * The same HierName will always be returned for the 
     * same string
     **/
    public static HierName makeHierName(final String hierNameString,
                                        final char separator)
        throws InvalidHierNameException
    {
        final String[] a = StringUtil.split(hierNameString, separator);
        HierName p = null;

        // @review jmr
        // this is somewhat inefficent for "a/b/c" and "a/b/d", 
        // an attempt will be made to add "a" and "a/b" twice.
        // pehaps going from back to front could fix this.
        for (int i = 0; i < a.length; ++i)
            p = makeHierName(p, a[i]);

        return p;
    }

    /**
     * Given a HierName prefix and suffix, return the canonical HierName 
     * for this pair of (prefix, name).  If 
     * <code>p1.equals(p2) &amp;&amp; n1.equals(n2)</code>
     * then <code>makeHierName(p1,n1) == makeHierName(p2,n2)</code>;
     * <code>prefix</code> is assumed
     * to be in the cache.  The HierName is added to 
     * the cache if it is not already there.
     **/
    public static HierName makeHierName(final HierName prefix,
                                        final String name)
        throws InvalidHierNameException
    {
        // Debug.assertTrue(cache.get(prefix) == prefix);

        return intern(new HierName(prefix,  name));
    }

    /**
     * Returns a HierName with the specified prefix and suffix.
     * The suffix must not have a parent, or an IllegalArgumentException
     * will be thrown.
     **/
    public static HierName makeHierName(final HierName prefix,
                                        final HierName suffix) {
        // Debug.assertTrue(cache.get(prefix) == prefix);
        // Debug.assertTrue(cache.get(suffix) == suffix);

        return intern(new HierName(prefix, suffix));
    }

    /**
     * Returns a HierName that is a clone of an existing HierName, except
     * marked as a port, if so identified.
     **/
    public static HierName makePortName(final HierName clone,
                                        final boolean port) {
        return !port || clone.isPort() ? clone : new HierName(clone);
    }

    /**
     * Returns a HierName that is the concatenation of the two names.
     **/
    public static HierName append(final HierName hn1, final HierName hn2) {
        if (hn2.prefix == null)
            return new HierName(hn1, hn2.name);
        else
            return new HierName(append(hn1, hn2.prefix), hn2.name);
    }

    /**
     * Returns the HierName with the specified suffix appended.  For example:
     * <code>makeHierName("a.b.c", '.').appendString("d") returns
     * "a.b.cd"</code>.
     **/
    public HierName appendString(final String suffix) {
        return makeSiblingName(this, this.getSuffixString()+suffix);
    }

    /**
     * Memorizes the given name if it is not already memorized.
     **/
    private static HierName intern(final HierName hn) {
        // cache.get compares using equals, so this will be sort of slow
        /*
        final HierName hnOld = (HierName) cache.get(hn);
        if (hnOld != null)
            return hnOld;
        else {
            cache.put(hn, hn);
            return hn;
        }
        */
        return hn;
    }

    /**
     * Returns a name with the same prefix as <code>name</code>,
     * but a different suffix.  Ie makeSiblingName("a.b.c", "d") ==
     * "a.b.d".  If newSuffix contains invalid characters,
     * IllegalArgumentException is thrown.
     **/
    public static HierName makeSiblingName(final HierName name,
                                           final String newSuffix) {
        try {
            return makeHierName(name.prefix, newSuffix);
        } catch (InvalidHierNameException e) {
            throw (IllegalArgumentException)
                new IllegalArgumentException("bad hiername: "
                    + e.getMessage()).initCause(e);
        }
    }

    /**
     * Returns a name with the same prefix as <code>name</code> but with a
     * suffix one character shorter than before.  Ie trim("a.b.c#") == "a.b.c"
     ***/
    public static HierName trim(final HierName name) { 
        try {
            return makeHierName(name.prefix, name.getSuffixTrimmed());
        } catch (InvalidHierNameException e) {
            throw (IllegalArgumentException)
                new IllegalArgumentException("bad hiername: "
                    + e.getMessage()).initCause(e);
        }
    }
    
    public int hashCode() {
        int h = 0;
        if (prefix != null && !isGlobal())
            h = prefix.hashCode();

        for (int i = 0; i < name.length; ++i)
            h = 31 * h + name[i];

        return h;
    }

    /**
     * Returns true if the other object is a HierName, and they are equal.
     * Two HierNames are equal if 
     **/
    public boolean equals(final Object o) {
        return o instanceof HierName && equals((HierName) o);
    }

    /**
     * Two HierNames are equal if all components of their names are equal,
     * and they have the same number of components.  This computes equality
     * by actually testing the prefix and local name, so should
     * only be used within this file.
     *
     * @bug kiniry 23 July 2002 - Hot spot for optimization.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1173">Bug#1173</a>
     *
     * @see boolean equals(Object)
     **/
    public boolean equals(final HierName hn) {
        if (hn == null)
            return false;
        else if (!ByteArrayString.equals(name, hn.name))
            return false;
        else if (prefix == null)
            return hn.prefix == null;
        else
            return prefix.equals(hn.prefix);
    }

    /**
     * Returns the class number of the node.  Smaller class numbers
     * are less for the purposes of compareTo.  
     * <code>global &lt; local &lt; generated</code>
     *
     * @bug kiniry 23 July 2002 - Hot spot for optimization.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1173">Bug#1173</a>
     **/
    private int getOrderClass() {
        return flags & ORDER_CLASS_MASK;
    }

    /**
     * Used by <code>initializeFlags()</code> to initialize the
     * order class to be returned by <code>getOrderClass()</code>.
     **/
    private int computeOrderClass(final boolean global,
                                  final boolean generated,
                                  final boolean anonymous) {
        if (global)
            return 0;
        else
            return (anonymous ? IS_ANONYMOUS : 0) + (generated ? 2 : 1);
    }

    /**
     * Returns true if the first component of the HierName is 
     * <code>hn</code>.  <code>hn</code> must have exactly one
     * component.
     *
     * @todo jmr An iterative implemenation could make this more efficient.
     **/
    public boolean prefixIs(final HierName hn) {
        Debug.assertTrue(hn.getNumComponents() == 1);
        HierName n = this;

        while (n.prefix != null)
            n = n.prefix;

        return ByteArrayString.equals(n.name, hn.name);
    }

    /**
     * Returns this HierName, but without the first component, or
     * null if there is only one component.
     **/
    public HierName tail() {
        if (prefix == null)
            return null;
        else
            return new HierName(prefix.tail(), name);
    }

    /**
     * Return this HierName, but with the first <code>n</code> components
     * removed, or <code>null</code> if <code>n</code> is larger than the
     * number of components.
     **/
    public HierName tail(final int n) {
        HierName name = this;
        for (int i = 0; i < n && name != null; ++i) {
            name = name.tail();
        }
        return name;
    }
    
    /**
     *  Returns true if this successively matches all components of hn
     **/
    public boolean isChildOf(HierName hn) {
        if( hn == null )
            return true;
        else {
            if( this.head().equals(hn.head() ) ) {
                final HierName t = tail();
                return t != null && t.isChildOf(hn.tail());
            } else
                return false;
        }
    }

    /**
     * Same as isChildOf, but avoids creating new HierName in the process.
     **/
    public boolean isChildOf2(HierName hn) {
        if (hn == null) return true;

        final int hnSize = hn.getNumComponents();
        final int mySize = getNumComponents();
        final int diff = mySize - hnSize;
        if (diff <= 0) return false;

        HierName trimmed = this;
        for (int i = 0; i < diff; ++i) trimmed = trimmed.prefix;

        return trimmed.equals(hn);
    }
        
    /**
     *  Returns the first component of this HierName
     **/
    public HierName head() {
        if( prefix == null )
            return this;
        else
            return prefix.head();
    }

    /**
     * Returns the components of this HierName as a collection.
     **/
    public Collection<String> getComponents(Collection<String> result) {
        if (prefix != null) prefix.getComponents(result);
        result.add(ByteArrayString.getString(name));
        return result;
    }

    /**
     * Returns true if this HierName ends with <code>hn</code>
     **/
    public boolean endsWith(HierName hn) {
        boolean same = ByteArrayString.equals(name, hn.name);
        if (same) {
            if (hn.prefix == null) {
                return true;
            } else if (prefix == null) {
                return false;
            } else {
                return prefix.endsWith(hn.prefix);
            }
        } else {
            return false;
        }
    }

    //
    // implements Comparable
    //

    /**
     * @exception ClassCastException if o is not a HierName
     **/
    public int compareTo(final Object o) {
        return compareTo((HierName) o);
    }

    /**
     * Compares two HierNames lexicographically.  
     * First, the class of the name is compared.  All global names
     * are less than all local names are less than all generated names
     * If they are the same class, the one with fewer path components 
     * is smaller.  If they have the same number of components, 
     * the one that comes earlier in the alphabet is smaller.
     *
     * @bug kiniry 23 July 2002 - Hot spot for optimization.
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1173">Bug#1173</a>
     **/
    public int compareTo(final HierName that) {
        // first check the class
        final int c1 = this.getOrderClass();
        final int c2 = that.getOrderClass(); 
        if (c1 != c2)
            return c1 - c2;

        // both are in the same class
        // check the number of components, then alpha order

        return lexicographicCompareTo(that);
    }

    private static int lexicographicCompareByteArrayStrings( final byte[] a, final byte[] b ) {
        if ( ( a.length != 0 ) && ( b.length != 0 ) ) {
            final int dollarChar = ( int ) '$';
            final int aFirstChar = a[0];
            final int bFirstChar = b[0];
            if ( aFirstChar == bFirstChar ) {
                return ByteArrayString.compare( a, b );
            }
            else {
                if ( aFirstChar == dollarChar ) {
                    return 1;
                }
                else if ( bFirstChar == dollarChar ) {
                    return -1;
                }
                else {
                    return ByteArrayString.compare( a, b );
                }
            }
        }
        else {
            return ByteArrayString.compare( a, b );
        }
    }

    /**
     * If the two names have a different number of components,
     * the one with fewer is smaller.
     * If the two names have the same number of components,
     * compare component by component from parent to child;
     * the first one with a component lexiographically
     * smaller than the other is smaller.
     **/
    private int lexicographicCompareTo(final HierName that) {
        if (this == that)
            return 0;
        
        if ( prefix == null ) {
            if (that.prefix == null)
                return lexicographicCompareByteArrayStrings(name, that.name);
            else
                return -1;
        } 
        else if (that.prefix == null) {
            return 1;
        } 
        else {
            // neither at end
            final int c = prefix.lexicographicCompareTo(that.prefix);
            if (c != 0)
                return c;
            else
                return lexicographicCompareByteArrayStrings(name, that.name);
        }
    }

    /**
     * return the comparator that compares HierNames with their natural
     * ordering.
     **/
    public static Comparator getComparator() {
        return new HierNameComparator();
    }

    /**
     * Comparator using HierName's natural order
     **/
    private static final class HierNameComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            final HierName hn1 = (HierName) o1;
            final HierName hn2 = (HierName) o2;

            return hn1.compareTo(hn2);
        }

        public boolean equals(final Object o) {
            return getClass() == o.getClass();
        }
    }

    /**
     * return the comparator that compares HierNames with their natural
     * ordering, giving higher priority to names designated as ports
     **/
    public static Comparator<HierName> getPortComparator() {
        return PortNameComparator.getInstance();
    }

    private static final class PortNameComparator
        implements Comparator<HierName> {
        private static PortNameComparator singleton = null;

        private PortNameComparator() { }

        public int compare(final HierName n1, final HierName n2) {
            final boolean port1 = n1.isPort();
            final boolean port2 = n2.isPort();
            if (port1 == port2) {
                return n1.compareTo(n2);
            } else {
                return port1 ? -1 : 1;
            }
        }

        public boolean equals(final Object o) {
            return this == o;
        }

        public int hashCode() {
            return 0;
        }

        public static PortNameComparator getInstance() {
            if (singleton == null) singleton = new PortNameComparator();
            return singleton;
        }
    }

    /**
     * If <code>name</code> is a global name, return it, otherwise,
     * return it prefixed by <code>subcellName</code>.
     **/
    public static HierName prefixName(final HierName subcellName,
                                       final HierName name) {
        if (name.isGlobal())
            return name;
        else
            return HierName.append(subcellName, name);
    }

    /**
     * returns name of parent.
     **/
    public HierName getParent() {
        return prefix;
    }

    /**
     * Iterate over all prefix-suffix pairs.  For example, for
     * <code>a.b.c.d</code>, the iterator would iterate over
     * <code>[null, a.b.c.d]</code>,
     * <code>[a, b.c.d]</code>,
     * <code>[a.b, c.d]</code>,
     * <code>[a.b.c, d]</code>, and
     * <code>[a.b.c.d, null]</code>.
     **/
    public static Iterator<Pair<HierName,HierName>>
        getSplitsIterator(final HierName name) {
        return new Iterator<Pair<HierName,HierName>>() {
            HierName prefix = null;
            HierName suffix = name;
            public boolean hasNext() {
                return prefix != null || suffix != null;
            }
            public Pair<HierName,HierName> next() {
                if (!hasNext()) throw new NoSuchElementException();
                final Pair<HierName,HierName> result =
                    new Pair<HierName,HierName>(prefix, suffix);
                if (suffix == null) {
                    prefix = null; // signal termination
                } else {
                    prefix = HierName.append(prefix, suffix.head());
                    suffix = suffix.tail();
                }
                return result;
            }
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    /**
     * Splits a string with 1 glob into 4 sets of strings:
     * a.b.c*d.e.f -&gt; a,b c d e,f, for use in comparing globs
     * to HierNames.
     **/
    public static class Glob1 {
        private final byte [] prefixes[], suffixes[];
        private final byte [] globPrefix, globSuffix;

        public Glob1(String glob) throws UnrepresentableCharException {
            int star = glob.indexOf('*');
            String head = glob.substring(0, star);
            String tail = glob.substring(star+1);
            String [] heads = StringUtil.split(head,'.');
            String [] tails = StringUtil.split(tail,'.');
            if (heads.length == 0) {
                prefixes = new byte [0][];
                globPrefix = new byte [0];
            } else {
                prefixes = new byte [heads.length - 1][];
                for (int i = 0; i < heads.length - 1; i++) {
                    prefixes[i] = ByteArrayString.fromString(heads[i]);
                }
                globPrefix =
                    ByteArrayString.fromString(heads[heads.length - 1]);
            }

            if (tails.length == 0) {
                suffixes = new byte [0][];
                globSuffix = new byte [0];
            } else {
                suffixes = new byte [tails.length - 1][];
                for (int i = 1; i < tails.length; i++) {
                    suffixes[i-1] = ByteArrayString.fromString(tails[i]);
                }
                globSuffix =
                    ByteArrayString.fromString(tails[0]);
            }
        }

        public boolean matches(HierName h) {
            int numComponents = h.getNumComponents();
            if (numComponents < prefixes.length + suffixes.length + 1) {
                return false;
            }

            byte [][] components = getComponents(h);
            if (!suffixesMatch(components)) {
                return false;
            }
            if (!prefixesMatch(components)) {
                return false;
            }
            int first = prefixes.length;
            int last = components.length - suffixes.length - 1;
            if (!bigEnough(components, first, last)) {
                return false;
            }
            if (!prefixMatch(components[first])) {
                return false;
            }
            if (!suffixMatch(components[last])) {
                return false;
            }
            return true;
        }

        private boolean suffixesMatch(byte [][] components) {
            int sl = suffixes.length;
            int offset = components.length - sl;
            for (int i = 0; i < sl; i++) {
                if (!ByteArrayString.equals(suffixes[i],
                                            components[i+offset])) {
                    return false;
                }
            }
            return true;
        }

        private boolean prefixesMatch(byte [][] components) {
            for (int i = 0; i < prefixes.length; i++) {
                if (!ByteArrayString.equals(prefixes[i], components[i])) {
                    return false;
                }
            }
            return true;
        }

        private boolean prefixMatch(byte [] string) {
            if (globPrefix.length > string.length) {
                return false;
            }
            for (int i = 0; i < globPrefix.length; i++) {
                if (globPrefix[i] != string[i]) {
                    return false;
                }
            }
            return true;
        }

        private boolean suffixMatch(byte [] string) {
            if (globSuffix.length > string.length) {
                return false;
            }
            int offset = string.length - globSuffix.length;
            for (int i = 0; i < globSuffix.length; i++) {
                if (globSuffix[i] != string[i+offset]) {
                    return false;
                }
            }
            return true;
        }

        private boolean bigEnough(byte[][] components, int first, int last) {
            if (first == last) {
                if (components[first].length <
                        globSuffix.length + globPrefix.length) {
                    return false;
                }
            } else if (components[first].length < globPrefix.length) {
                return false;
            } else if (components[last].length < globSuffix.length) {
                return false;
            }
            return true;
        }

        private byte [][] getComponents(HierName h) {
            byte [][] comps = new byte[h.getNumComponents()][];
            for (int i = comps.length - 1; i >= 0; i--) {
                comps[i] = h.name;
                h = h.prefix;
            }
            return comps;
        }
    }
}
