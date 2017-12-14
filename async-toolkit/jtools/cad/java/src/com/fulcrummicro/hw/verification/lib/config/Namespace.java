/**
 * INTEL CONFIDENTIAL
 * Copyright 2003 - 2011 Intel Corporation
 * All Rights Reserved.
 *
 * The source code contained or described herein and all documents related to
 * the source code ("Material") are owned by Intel Corporation or its suppliers
 * or licensors. Title to the Material remains with Intel Corporation or its
 * suppliers and licensors. The Material contains trade secrets and proprietary
 * and confidential information of Intel or its suppliers and licensors. The
 * Material is protected by worldwide copyright and trade secret laws and
 * treaty provisions. No part of the Material may be used, copied, reproduced,
 * modified, published, uploaded, posted, transmitted, distributed, or
 * disclosed in any way without Intel's prior express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or delivery
 * of the Materials, either expressly, by implication, inducement, estoppel or
 * otherwise. Any license under such intellectual property rights must be
 * express and approved by Intel in writing.
 */

package com.fulcrummicro.hw.verification.lib.config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Frederik Eaton
 */
public class Namespace {
    // TODO:

    // expose less internal structure - children should be presented
    // as namespaces

    // deep copy operation

    // allow iterator traversal of keys, entries

    public static Namespace rootConfig = null;

    /** The root of the namespace. */
    private final Map<String, Object> root;
    
    private static final Pattern dot = Pattern.compile("\\.");

    public Namespace() {
        this(newMap());
    }

    public Namespace(Map<String, Object> root) {
        this.root = root;
    }

    /***************************************************************************
     * Public Interface
     **************************************************************************/

    /**
     * Retrieves the value associated with the specified key.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the value associated with the specified key or {@code null} if no
     *         value is associated with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public Object get(String key) throws Error {
        try {
            return doOp(rootEntry(), split(key), NamespaceOperation.GET, null);
        } catch (NotADirectoryException e) {
            throw new Error(key + ": No such directory");
        }
    }

    /**
     * Retrieves the value associated with the specified key. The value is
     * guaranteed to be non-{@code null}
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the value associated with the specified key.
     * @throws NoSuchElementException
     *             if either the specified key could not be found or the value
     *             associated with the key is {@code null}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public Object getValid(String key) throws Error, NoSuchElementException {
        Object o = get(key);
        if (o == null) {
            throw new NoSuchElementException("Missing entry " + key);
        }
        return o;
    }

    /**
     * Adds the specified {@literal<key,value>} pair to this namespace. If the
     * specified key already exists, its associated value is replaced by the
     * specified value.
     *
     * @param key
     *            is the key that is to be added.
     * @param o
     *            is the value to be associated with the specified key.
     * @return the value that has been added.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public Object put(String key, Object o) throws Error {
        try {
            return doOp(rootEntry(), split(key), NamespaceOperation.PUT, o);
        } catch (NotADirectoryException e) {
            throw new Error(key + ": No such directory");
        }
    }

    /**
     * Removes the specified key and its associated value from this namespace.
     *
     * @param key
     *            is the key that is to be removed.
     * @return the value associated with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public Object remove(String key) throws Error {
        return put(key, null);
    }

    /**
     * Merges the specified namespace with this namespace at the specified key.
     *
     * @param key
     *            is the key at which the merge is to start.
     * @param ns
     *            is the namespace to merge with this namespace.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public void merge(String key, Namespace ns) throws Error {
        try {
            doOp(rootEntry(), split(key), NamespaceOperation.MERGE, ns.root);
        } catch (NotADirectoryException e) {
            throw new Error(key + ": No such directory");
        }
    }

    /***************************************************************************
     * Helper Functions
     **************************************************************************/

    /**
     * Retrieves the string representation of the value associated with the
     * specified key.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the string representation of the value associated with the
     *         specified key.
     * @throws NoSuchElementException
     *             if either the specified key could not be found or the value
     *             associated with the key is {@code null}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public String stringValue(String key) throws Error, NoSuchElementException {
        return (String) getValid(key);
    }

    /**
     * Retrieves the string representation of the value associated with the
     * specified key or the specified default value if the key could not be
     * found or its associated value is {@code null}.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the string representation of the value associated with the
     *         specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public String stringValue(String key, String defaultValue) throws Error {
        try {
            return stringValue(key);
        } catch (NoSuchElementException e) {
            return defaultValue;
        }
    }

    /**
     * Retrieves the 32-bit signed integer representation of the value
     * associated with the specified key.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the 32-bit signed integer representation of the value associated
     *         with the specified key.
     * @throws NoSuchElementException
     *             if either the specified key could not be found or the value
     *             associated with the key is {@code null}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public int intValue(String key) throws Error, NoSuchElementException {
        return ((Number) getValid(key)).intValue();
    }

    /**
     * Retrieves the 32-bit signed integer representation of the value
     * associated with the specified key or the specified default value if the
     * key could not be found or its associated value is {@code null}.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the 32-bit signed integer representation of the value associated
     *         with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public int intValue(String key, int defaultValue) throws Error {
        try {
            return intValue(key);
        } catch (NoSuchElementException e) {
            return defaultValue;
        }
    }

    /**
     * Retrieves the 64-bit signed integer representation of the value
     * associated with the specified key.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the 64-bit signed integer representation of the value associated
     *         with the specified key.
     * @throws NoSuchElementException
     *             if either the specified key could not be found or the value
     *             associated with the key is {@code null}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public long longValue(String key) throws Error, NoSuchElementException {
        return ((Number) getValid(key)).longValue();
    }

    /**
     * Retrieves the 64-bit signed integer representation of the value
     * associated with the specified key or the specified default value if the
     * key could not be found or its associated value is {@code null}.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the 64-bit signed integer representation of the value associated
     *         with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public long longValue(String key, long defaultValue) throws Error {
        try {
            return longValue(key);
        } catch (NoSuchElementException e) {
            return defaultValue;
        }
    }

    /**
     * Retrieves the boolean representation of the value associated with the
     * specified key.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the boolean representation of the value associated with the
     *         specified key.
     * @throws NoSuchElementException
     *             if either the specified key could not be found or the value
     *             associated with the key is {@code null}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public boolean booleanValue(String key) throws Error,
                                           NoSuchElementException {
        return ((Boolean) getValid(key)).booleanValue();
    }

    /**
     * Retrieves the boolean representation of the value associated with the
     * specified key or the specified default value if the key could not be
     * found or its associated value is {@code null}.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the boolean representation of the value associated with the
     *         specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public boolean booleanValue(String key, boolean defaultValue) throws Error {
        try {
            return booleanValue(key);
        } catch (NoSuchElementException e) {
            return defaultValue;
        }
    }

    /**
     * Retrieves the single precision floating point representation of the value
     * associated with the specified key.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the single precision floating point representation of the value
     *         associated with the specified key.
     * @throws NoSuchElementException
     *             if either the specified key could not be found or the value
     *             associated with the key is {@code null}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public float floatValue(String key) throws Error, NoSuchElementException {
        return ((Float) getValid(key)).floatValue();
    }

    /**
     * Retrieves the single precision floating point representation of the value
     * associated with the specified key or the specified default value if the
     * key could not be found or its associated value is {@code null}.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the single precision floating point representation of the value
     *         associated with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public float floatValue(String key, float defaultValue) throws Error {
        try {
            return floatValue(key);
        } catch (NoSuchElementException e) {
            return defaultValue;
        }
    }

    /**
     * Retrieves the 32-bit signed integer list representation of the value
     * associated with the specified key.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the 32-bit signed integer list representation of the value associated
     *         with the specified key.
     * @throws NoSuchElementException
     *             if either the specified key could not be found or the value
     *             associated with the key is {@code null}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public ArrayList<Integer> intSequenceValue(String key)  throws Error, NoSuchElementException, NumberFormatException {
        ArrayList<Integer> result = new ArrayList<Integer>();
        String str = stringValue(key);
        String[] parts = str.split(",");
        Pattern seq = Pattern.compile("(\\d+)..(\\d+)");
        for (String part : parts) {
            Matcher m = seq.matcher(part);
            if(m.matches()) {
                Integer a = Integer.parseInt(m.group(1));
                Integer b = Integer.parseInt(m.group(2));
                if(a > b) throw new Error("invalid seq " + m.group() + " in " + str + " for key " + key);
                for(Integer i=a; i<=b; i++) {result.add(i);}
            } else {
                result.add(Integer.parseInt(part));
            }
        }

        return result;
    }

    /**
     * Retrieves the value associated with the specified key as an instance of
     * {@literal Map<String, Object>}.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the value associated with the specified key as an instance of
     *         {@literal Map<String, Object>}.
     * @throws ClassCastException
     *             if the value associated with the specified key is not an
     *             instance of {@literal Map<String, Object>}.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    @SuppressWarnings("unchecked")
    public Map<String, Object> mapValue(String key) throws ClassCastException, Error {
        Object o = getValid(key);
        if (o instanceof Map) {
            // It is assumed that any Map at this point is an instance of
            // Map<String, Object>.
            return (Map<String, Object>) o;
        } else {
            throw new ClassCastException("Cannot cast Object to Map<String, Object>");
        }
    }

    /**
     * Retrieves the value associated with the specified key as a Namespace
     * object.
     *
     * @param key
     *            is the key whose associated value is to be retrieved.
     * @return the value associated with the specified key as a Namespace
     *         object. *
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public Namespace namespaceValue(String key) {
        return wrapObject(getValid(key));
    }

    public Namespace namespaceValue(String key, Namespace defaultValue) {
        try {
            return namespaceValue(key);
        } catch (NoSuchElementException e) {
            return defaultValue;
        }
    }

    /**
     * Adds the specified {@literal <}key,{@code String}{@literal >} pair to
     * this namespace replacing the existing value if necessary.
     *
     * @param key
     *            is the key to be added.
     * @param value
     *            is the string to be associated with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public void putString(String key, String value) throws Error {
        put(key, value);
    }

    /**
     * Adds the specified {@literal <}key,{@code int}{@literal >} pair to
     * this namespace replacing the existing value if necessary.
     *
     * @param key
     *            is the key to be added.
     * @param value
     *            is the 32-bit signed integer to be associated with the
     *            specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public void putInt(String key, int value) throws Error {
        put(key, new Integer(value));
    }

    /**
     * Adds the specified {@literal <}key,{@code long}{@literal >} pair to
     * this namespace replacing the existing value if necessary.
     *
     * @param key
     *            is the key to be added.
     * @param value
     *            is the 64-bit signed integer to be associated with the
     *            specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public void putLong(String key, long value) throws Error {
        put(key, new Long(value));
    }

    /**
     * Adds the specified {@literal <}key,{@code boolean}{@literal >} pair to
     * this namespace replacing the existing value if necessary.
     *
     * @param key
     *            is the key to be added.
     * @param value
     *            is the boolean to be associated with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public void putBoolean(String key, boolean value) throws Error {
        put(key, new Boolean(value));
    }

    /**
     * Adds the specified {@literal <}key,{@code float}{@literal >} pair to
     * this namespace replacing the existing value if necessary.
     *
     * @param key
     *            is the key to be added.
     * @param value
     *            is the single precision floating point value to be associated
     *            with the specified key.
     * @throws Error
     *             if a regular, i.e. {@literal non-Map<String, Object>},
     *             object is found while traversing the key tree.
     */
    public void putFloat(String key, float value) throws Error {
        put(key, new Float(value));
    }

    // XXX: should ask for opinions on what the best format would be,
    // this is concise but might be rather cryptic
    public String toString() {
        return toPrefixString("");
    }

    /* Medium-public interface */
    static public class Entry implements Map.Entry<String, Object> {
        Map<String, Object> map;

        private String key;

        Entry(Map<String, Object> map, String key) {
            this.map = map;
            this.key = key;
        }

        public String getKey() {
            return this.key;
        }

        public Object getValue() {
            return this.map.get(this.key);
        }

        public Object setValue(Object v) {
            if (v == null) {
                return this.map.remove(this.key);
            } else {
                return this.map.put(this.key, v);
            }
        }
    }

    /***************************************************************************
     * Implementation
     **************************************************************************/

    /** The list of valid Namespace operations.  */
    private enum NamespaceOperation {
        GET, PUT, MERGE;
    }

    private static Map<String, Object> newMap() {
        return new LinkedHashMap<String, Object>();
    }

    /**
     * Returns an entry whose getValue() returns our root, and whose setValue()
     * does not have any external effect.
     */
    private Entry rootEntry() {
        Map<String, Object> m = newMap();
        m.put("_", this.root);
        return new Entry(m, "_");
    }

    private Entry lookupEntry(String key) {
        return new Entry(this.root, key);
    }

    /**
     * Breaks the specified key into a list of substrings using a period as the
     * pattern to split on.
     *
     * @param key
     *            is the key to be broken up.
     * @return the list of substrings
     */
    private static List<String> split(String key) {
        if (key.equals(""))
            return new ArrayList<String>();
        else
            return Arrays.asList(dot.split(key));
    }

    /**
     * Wraps the specified object in a Namespace object.
     *
     * @param o
     *            is the object to be wrapped.
     * @return the object wrapped in a Namespace object.
     * @throws ClassCastException
     *             if the value associated with the specified key is not an
     *             instance of {@literal Map<String, Object>}.
     *
     */
    @SuppressWarnings("unchecked")
    private Namespace wrapObject(Object o) throws ClassCastException {
        if (o instanceof Map) {
            return new Namespace((Map<String, Object>) o);
        } else {
            throw new ClassCastException("Cannot cast Object to Map<String, Object>");
        }
    }

    /**
     * Performs a {@code GET}, {@code PUT} or {@code MERGE} operation on the
     * specified namespace entry.
     *
     * @param e
     *            is the namespace entry on which to operate.
     * @param path
     *            is the namespace path.
     * @param op
     *            is the operation to be performed.
     * @param arg
     * @return
     * @throws NotADirectoryException
     *             if the value contained by {@code e} is not a Map instance.
     */
    @SuppressWarnings("unchecked")
    private Object doOp(Entry e,
                        List<String> path,
                        NamespaceOperation op,
                        Object arg) throws NotADirectoryException {
        if (path.size() > 0) {
            Object o = e.getValue();
            if (o == null) {
                if (op == NamespaceOperation.GET) {
                    return null;
                } else {
                    e.setValue(o = newMap());
                }
            }
            if (!(o instanceof Map)) {
                throw new NotADirectoryException();
            }
            // It is assumed that any Map at this point is an instance of
            // Map<String, Object>.
            Namespace ns = new Namespace((Map<String, Object>) o);
            String head = path.get(0);
            List<String> rest = path.subList(1, path.size());
            return doOp(ns.lookupEntry(head), rest, op, arg);
        } else {
            switch (op) {
                case GET:
                    return e.getValue();
                case PUT:
                    return e.setValue(arg);
                case MERGE:
                    Object o = e.getValue();
                    if (o == null
                            || (!(o instanceof Map) && !(arg instanceof Map))) {
                        return e.setValue(arg);
                    }
                    // It is assumed that any Map at this point is an instance
                    // of Map<String, Object>.
                    Iterator<Map.Entry<String, Object>> i =
                        ((Map<String, Object>) arg).entrySet().iterator();
                    while (i.hasNext()) {
                        Map.Entry<String, Object> field = i.next();
                        List<String> list = new ArrayList<String>();
                        list.add(field.getKey());
                        doOp(e, list, op, field.getValue());
                    }
                    return null;
                default:
                    throw new RuntimeException(op.toString()
                            + ": Unknown operation");
            }
        }
    }

    @SuppressWarnings("unchecked")
    private String toPrefixString(String prefix) {
        StringBuilder sb = new StringBuilder();
        Iterator<Map.Entry<String, Object>> i = this.root.entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry<String, Object> e = i.next();
            String key = e.getKey();
            Object value = e.getValue();
            if (value instanceof Map) {
                // It is assumed that any Map at this point is an instance of
                // Map<String, Object>.
                Namespace ns = new Namespace((Map<String, Object>) value);
                sb.append(ns.toPrefixString(prefix + key + "."));
            } else {
                sb.append(prefix + key + "=" + value + "\n");
            }
        }
        return sb.toString();
    }

}