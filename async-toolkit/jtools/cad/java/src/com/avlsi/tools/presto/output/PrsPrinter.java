/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.io.PrintWriter;
import com.avlsi.util.text.StringUtil;

/**
 * The purpose here is to take a bunch of WholeOperators (which have
 * already been simplified by Organizer) and convert them to a
 * pretty-printed textual representation.
 */

public class PrsPrinter implements WholeOperatorSink {
    private int serialCounter = 0;

    private class Stuff extends LineAndSection {
        public final int fromIndex;
        public int toIndex; // yes, this is the one mutable field!
        public final int serial;

        public Stuff(LineAndSection las, int index) {
            super(las.line, las.section);
            fromIndex = index;
            toIndex = index;
            serial = serialCounter++;
        }
    }

    // List of Stuff
    private final List stuff = new LinkedList();

    public void hereYaGo(WholeOperator wo) {
        LineAndSection[] las = wo.expr.evaluate(wo.dest, wo.preferredDirection,
                                                wo.upSection, wo.downSection);
        for (int i = 0; i < las.length; i++) {
            stuff.add(new Stuff(las[i], wo.index));
        }
    }

    /**
     * @param w            output goes here
     * @param baseIndent   all lines are indented this many spaces
     * @param extraIndent  "indented" lines are indented this many more spaces
     * @param commentColumn same-line comments go in this column
     * @param ancestorSection a Section which is less than all other sections
     */

    public void emit(PrintWriter w, int baseIndent, int extraIndent,
                     int commentColumn, Section ancestorSection) {
        // sort by index (needed to condense loops)
        Collections.sort(stuff, new Comparator() {
                public int compare(Object o1, Object o2) {
                    Stuff s1 = (Stuff) o1;
                    Stuff s2 = (Stuff) o2;

                    return s1.fromIndex - s2.fromIndex;
                }
            });

        // try to condense contiguous indices into loops
        Map loopy = new HashMap(); // key = PrsLine, value = Stuff
        for (Iterator it = stuff.iterator(); it.hasNext(); ) {
            Stuff current = (Stuff) it.next();
            Stuff prev = (Stuff) loopy.get(current.line);
            assert (current.fromIndex == current.toIndex);
            if (prev != null && current.fromIndex == prev.toIndex + 1) {
                assert (current.line.isParameterized() &&
                        prev.line.isParameterized());
                prev.toIndex++;
                it.remove();
            } else {
                loopy.put(current.line, current);
            }
        }

        /* Now put it back in the order we want.  (Sorted first by section,
         * and then by serial number.  (The serial number is to mimic
         * a stable sort, even though we screwed up the order in the
         * previous step.)) */
        Collections.sort(stuff, new Comparator() {
                public int compare(Object o1, Object o2) {
                    Stuff s1 = (Stuff) o1;
                    Stuff s2 = (Stuff) o2;

                    int result = s1.section.compareTo(s2.section);
                    if (result == 0)
                        result = s1.serial - s2.serial;

                    return result;
                }
            });

        /* Now print it all out */
        String bi = StringUtil.repeatString(" ", baseIndent);
        String ei = StringUtil.repeatString(" ", baseIndent + extraIndent);

        Section prevSect = ancestorSection;
        for (Iterator it = stuff.iterator(); it.hasNext(); ) {
            Stuff current = (Stuff) it.next();
            boolean blankLine = current.section.blankLineBefore(prevSect);
            String commentBefore = current.section.commentBefore(prevSect);
            String commentWith = current.section.commentWith(prevSect);

            if (blankLine || commentBefore != null)
                w.println();

            if (commentBefore != null)
                w.println(bi + "// " + commentBefore);

            StringBuffer line =
                new StringBuffer(current.line.isIndented() ? ei : bi);
            boolean looped = (current.fromIndex != current.toIndex);
            if (looped) {
                assert (current.toIndex > current.fromIndex);
                assert (current.line.isParameterized());
                line.append("<i:");
                if (current.fromIndex == 0) {
                    line.append(current.toIndex+1);
                } else {
                    line.append(current.fromIndex);
                    line.append("..");
                    line.append(current.toIndex);
                }
                line.append(": ");
                line.append(current.line.toStringWithVariable("i"));
                line.append(" >");
            } else {
                line.append(current.line.toStringWithIndex(current.fromIndex));
            }

            if (commentWith != null) {
                do {
                    line.append(' ');
                } while (line.length() < commentColumn);

                line.append("// ");
                line.append(commentWith);
            }

            w.println(line);
            prevSect = current.section;
        }
    }
}
