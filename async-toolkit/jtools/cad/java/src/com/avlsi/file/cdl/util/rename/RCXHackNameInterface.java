/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import com.avlsi.csp.util.UniqueLabel;

/**
 * The same as <code>CadenceNameInterface</code>, except cell names are
 * translated to unique names less than 40 characters long to work around an
 * RCX issue that occurs for cell names longer than 80 characters.  Optionally
 * generates a cell mapping file appropriate for passing to <code>pipo</code>.
 * The reverse naming is not implemented, but instead depends on
 * <code>ReloadableNameInterface</code> to load back a previous encoding.
 **/
public class RCXHackNameInterface {
    public static class Primitive extends CadenceNameInterface {
        private static Pattern METAPARAM = Pattern.compile("(.*)\\(.*\\).*");

        /**
         * The maximum number of characters before a numeric sequence number is
         * added.  Thus this should probably be conservative.
         **/
        private final int lengthLimit;

        private final UniqueLabel labels;

        /**
         * A mapping of cell names from the CAST name space to this name
         * space.
         **/
        protected final Map/*<String,String>*/ cellNameMap;

        public Primitive(final int lengthLimit) {
            this.lengthLimit = lengthLimit;
            this.labels = new UniqueLabel(new HashMap());
            this.cellNameMap = new HashMap();
        }
        public String renameCell(final String castName) {
            // if a name has been translated already, use the previous
            // translation
            String result = (String) cellNameMap.get(castName);
            if (result == null) result = castName;
            else return result;

            // if the name exceeds the length limit, remove any metaparameters
            if (result.length() > lengthLimit) {
                final Matcher m = METAPARAM.matcher(result);
                if (m.matches()) {
                    result = m.group(1);
                }
            }

            // if the name exceeds the length limit without metaparameters
            // divide the name into . delimited components and use as many
            // components as possible starting from the right end
            if (result.length() > lengthLimit) {
                final int len = result.length();
                int last = len;
                int dot;
                while (last > 0 &&
                       (dot = result.lastIndexOf('.', last - 1)) != -1) {
                    if (len - dot - 1 > lengthLimit &&
                        len - last - 1 < lengthLimit) {
                        break;
                    }
                    last = dot;
                }
                if (last != len) {
                    result = result.substring(last + 1);
                }
            }

            // truncate to the length limit if name is still too long
            if (result.length() > lengthLimit) {
                result = result.substring(0, lengthLimit);
            }

            // replace non-alphanumeric characters with _
            final StringBuffer buf = new StringBuffer(result);
            for (int i = 0; i < buf.length(); ++i) {
                if (!Character.isLetterOrDigit(buf.charAt(i)))
                    buf.setCharAt(i, '_');
            }
            result = buf.toString();

            // add a numeric suffix
            result = result + "_" + labels.getLabel(castName);
            cellNameMap.put(castName, result);

            return result;
        }
    }

    public static class Forward extends Primitive
    implements CDLRenamer.EndObserver {
        private final Writer castMappingWriter, cadenceMappingWriter;
        public Forward(final int lengthLimit, final Writer castMappingWriter,
                       final Writer cadenceMappingWriter) {
            super(lengthLimit);
            this.castMappingWriter = castMappingWriter;
            this.cadenceMappingWriter = cadenceMappingWriter;
        }
        public void renameEnd() throws CDLRenameException {
            final CadenceNameInterface cadenceRenamer =
                new CadenceNameInterface();
            try {
                for (Iterator i = cellNameMap.entrySet().iterator();
                     i.hasNext(); ) {
                    final Map.Entry entry = (Map.Entry) i.next();

                    final String key = (String) entry.getKey();
                    assert !key.matches("\\s") : "White space in " + key;

                    final String val = (String) entry.getValue();
                    assert !val.matches("\\s") : "White space in " + val;

                    castMappingWriter.write(key + " " + val + "\n");
                    final String cadenceKey = cadenceRenamer.renameCell(key);
                    cadenceMappingWriter.write(cadenceKey + " " + val + "\n");
                }
                castMappingWriter.flush();
                cadenceMappingWriter.flush();
            } catch (IOException e) {
                throw new CDLRenameException(e);
            }
        }
    }

    public static class Reverse extends CadenceReverseNameInterface
    implements CDLRenamer.StartObserver {
        private final Reader cellMapReader;
        private final Map cellNameMap;
        private final static Pattern MAP_PATTERN =
            Pattern.compile("^\\s*(\\S+)\\s*(\\S+)\\s*$");
        public Reverse(final Reader cellMapReader) {
            this.cellMapReader = cellMapReader;
            this.cellNameMap = new HashMap();
        }
        public void renameStart() throws CDLRenameException {
            try {
                final BufferedReader br = new BufferedReader(cellMapReader);
                String line;
                while ((line = br.readLine()) != null) {
                    final Matcher m = MAP_PATTERN.matcher(line);
                    if (m.matches()) {
                        final String castName = m.group(1);
                        final String rcxName = m.group(2);
                        assert !cellNameMap.containsKey(rcxName) :
                               "Cell map not one-to-one";
                        cellNameMap.put(rcxName, castName);
                    } else {
                        throw new CDLRenameException("Invalid cell name " +
                                                     "mapping: " + line);
                    }
                }
            } catch (IOException e) {
                throw new CDLRenameException(e);
            }
        }
        public String renameCell(final String rcxName)
        throws CDLRenameException {
            final String result = (String) cellNameMap.get(rcxName);
            if (result == null) {
                throw new CDLRenameException(rcxName + " not defined in the " +
                                             "cell mapping file");
            } else {
                return result;
            }
        }
    }

    /**
     * This class cannot be constructed.  Use {@link getForwardNamer} and
     * {@link getReverseNamer} to get an instance that does the forward and
     * reverse mapping respectively.
     **/
    private RCXHackNameInterface() { }

    public static CDLNameInterface getForwardNamer(final int lengthLimit,
            final Writer castMappingWriter, final Writer cadenceMappingWriter) {
        return new Forward(lengthLimit, castMappingWriter,
                           cadenceMappingWriter);
    }

    public static CDLNameInterface getReverseNamer(final Reader cellMapReader) {
        return new Reverse(cellMapReader);
    }
}
