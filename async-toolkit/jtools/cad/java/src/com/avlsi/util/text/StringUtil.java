/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.text;

import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

/**
 * Class of string utilities.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class StringUtil {

    /**
     * Returns an array of words in the string; whitespace is discarded.
     * @param s  string to be broken up into words
     * @return  non-null array of the words in s
     **/
    public static String[] wordBreak(final String s) {
        final BreakIterator boundary = BreakIterator.getWordInstance();
        final List l = new ArrayList();

        boundary.setText(s);

        int start = boundary.first();
        for (int end = boundary.next();
             end != -1;
             start = end, end = boundary.next()) {
            if (!Character.isWhitespace(s.charAt(start))) {
                l.add(unshare(s.substring(start, end)));
            }
        }

        return (String[]) l.toArray(new String[l.size()]);
    }

    /**
     * Breaks s into substrings at each occurrence of the character c.
     * (If you want to split on whitespace, you probably want to call
     * tokenize() instead, because tokenize() will ignore consecutive
     * whitespace.)
     * @param  s  string to be broken up
     * @param  c  character to split string on
     * @return non-null array of the substrings of s
     **/
    public static String[] split(final String s, final char c) {
        final List l = new ArrayList();
        int i = 0;
        int j;
        while (i < s.length() && (j = s.indexOf(c, i)) != -1) {
            l.add(unshare(s.substring(i, j)));
            i = j + 1;
        }

        if (s.length() > 0)
            l.add(unshare(s.substring(i)));

        return (String[]) l.toArray(new String[l.size()]);
    }

    /**
     * Joins strings together into a single string, separated by the
     * character c.  This is the opposite of split().
     * @param  strings  an array of zero or more strings to join
     * @param  c        character to put between the strings
     * @return the joined-together string
     **/
    public static String join(final String[] strings, final char c) {
	final StringBuffer buf = new StringBuffer();
	for (int i = 0; i < strings.length; i++) {
	    if (i != 0)
		buf.append(c);
	    buf.append(strings[i]);
	}
	return buf.toString();
    }

    /**
     * Returns the string <code>s</code> repeated <code>n</code> time.
     **/
    public static String repeatString(final String s, final int n) {
        final StringBuffer sb = new StringBuffer(n * s.length());

        for (int i = 0; i < n; ++i)
            sb.append(s);

        return sb.toString();
    }

    /**
     * Replaces all ocurrences of character oldCh in s with newCh.
     **/
    public static String replaceChars(final String s,
                                      final char oldCh,
                                      final char newCh)
    {
        return replaceChars(new StringBuffer(s), oldCh, newCh).toString();
    }

    /**
     * Replaces all ocurrences of character oldCh in sb with newCh.
     * Both modifies and returns sb.
     * Use this method for speed when doing a chain of conversions.
     **/
    public static StringBuffer replaceChars(final StringBuffer sb,
                                            final char oldCh,
                                            final char newCh)
    {
        for (int i = 0; i < sb.length(); ++i)
            if (sb.charAt(i) == oldCh)
                sb.setCharAt(i, newCh);

        return sb;
    }

    /**
     * Replaces all ocurrences of characters in oldChs in s with 
     * corresponding characters in newChs. Ie: 
     * replaceChars("bar", "br", "ct") => "cat".
     **/
    public static String replaceChars(final String s,
                                      final String oldChs,
                                      final String newChs)
    {
        return replaceChars(new StringBuffer(s), oldChs, newChs).toString();
    }

    /**
     * Replaces all ocurrences of characters in oldChs in s with 
     * corresponding characters in newChs. Ie: 
     * replaceChars(new StringBuffer("bar"), "br", "ct").toString() => "cat"
     * Both modifies and returns sb.
     * Use this method for speed when doing a chain of conversions.
     **/
    public static StringBuffer replaceChars(final StringBuffer sb,
                                            final String oldChs,
                                            final String newChs)
    {
        for (int i = 0; i < sb.length(); ++i) {
            final int idx = oldChs.indexOf(sb.charAt(i));

            if (idx != -1)
                sb.setCharAt(i, newChs.charAt(idx));
        }

        return sb;
    }

    /**
     * Replace occurences of one substring with another string.
     * replaces occurences of oldStr with newStr in s.
     **/
    public static String replaceSubstring(final String s,
                                          final String oldStr,
                                          final String newStr)
    {
        return replaceSubstring(
                new StringBuffer(s), s, oldStr, newStr).toString();
    }

    /**
     * Replace occurences of one substring with another string.
     * replaces occurences of oldStr with newStr in sb.
     **/
    public static StringBuffer replaceSubstring(final StringBuffer sb,
                                                final String oldStr,
                                                final String newStr)
    {
        return replaceSubstring(sb, sb.toString(), oldStr, newStr);
    }

    /**
     * Replace occurences of one substring with another string.
     * replaces occurences of oldStr with newStr in sb.
     **/
    private static StringBuffer replaceSubstring(final StringBuffer sb,
                                                 final String s,
                                                 final String oldStr,
                                                 final String newStr)
    {
        Debug.assertTrue(s.equals(sb.toString()));

        int idxDiff = 0;
        int oldFromIdx = 0;  // position from which to start search in oldStr
        int oldFoundIdx;  // found position in oldStr
        while ((oldFoundIdx = s.indexOf(oldStr, oldFromIdx)) != -1) {
            final int newFoundIdx = oldFoundIdx + idxDiff;
            Debug.assertTrue(sb.toString()
                    .substring(newFoundIdx, newFoundIdx + oldStr.length())
                    .equals(s.substring(oldFoundIdx,
                            oldFoundIdx + oldStr.length())));
            sb.replace(newFoundIdx, newFoundIdx + oldStr.length(), newStr);
            oldFromIdx = oldFoundIdx + oldStr.length();
            idxDiff += newStr.length() - oldStr.length();
        }

        return sb;
    }

    /**
     * Returns a new String representing the same value as <code>s</code>,
     * but sharing no storage with <code>s</code>.  This is useful in case
     * <code>s</code> has a needlessly large char[].
     **/
    public static String unshare(final String s) {
        return (s == null) ? null : new String(s);
    }

    /**
     * If the string ends with suffix, return the string with suffix 
     * removed; if not, return null.
     **/
    public static String chompEnd(final String s, final String suffix) {
        if (s.endsWith(suffix))
            return s.substring(0, s.length() - suffix.length());
        else
            return null;
    }

    /**
     * If the string starts with prefix, return the string with prefix 
     * removed; if not, return null.
     **/
    public static String chompStart(final String s, final String prefix) {
        if (s.startsWith(prefix))
            return s.substring(prefix.length(), s.length());
        else
            return null;
    }


    /**
     * Break up a string into tokens with a StringTokenizer, return as a
     * String[].  Delimiters are not returned.
     **/
    public static String[] tokenize(final String arg) {
        final StringTokenizer st = new StringTokenizer(arg);
        final int count = st.countTokens();
        final String result[] = new String[count];
        for (int i = 0; i < count; ++i) {
            result[i] = st.nextToken();
        }
        return result;
    }

    /**
     * Make sure str is at least len long, adding ch at the left
     * if necessary.
     * @param  str the string to pad
     * @param  len minimum length of the new string
     * @param  ch  character to pad string with
     * @return newly padded string
     **/
    public static String padLeft(final String str, final int len,
                                 final char ch) {
        int more = len - str.length();
        String result = str;
        if (more > 0) {
            StringBuffer sb = new StringBuffer(len);
            while (more-- > 0)
                sb.append(ch);
            sb.append(str);
            result = sb.toString();
        }
        return result;
    }

    /**
     * Make sure str is at least len long, adding ch at the right
     * if necessary.
     * @param  str the string to pad
     * @param  len minimum length of the new string
     * @param  ch  character to pad string with
     * @return newly padded string
     **/
    public static String padRight(final String str, final int len,
                                  final char ch) {
        int more = len - str.length();
        String result = str;
        if (more > 0) {
            StringBuffer sb = new StringBuffer(len);
            sb.append(str);
            while (more-- > 0)
                sb.append(ch);
            result = sb.toString();
        }
        return result;
    }

    /**
     * For each location where a bracketed range of numbers (such as
     * "[30-45]") appears in the string, make a copy of the string
     * with each number in the range substituted into the string.
     */
    public static String[] expandRanges(String s) {
        int leftSquare = s.indexOf('[');

        if (leftSquare == -1) {
            // there are no ranges; simply return the string in a
            // one-element array
            return new String[] { s };
        } else {
            String prefix = s.substring(0, leftSquare);

            int i = leftSquare + 1;
            int first = 0;
            int last = 0;
            boolean isFirst = true;
            try {
                char c;
                for ( ; (c = s.charAt(i)) != ']'; i++) {
                    if (c == '-') {
                        if (isFirst)
                            isFirst = false;
                        else    // more than one '-' in range
                            throw new IllegalArgumentException(s);
                    } else {
                        int n = Character.digit(c, 10);
                        if (n == -1) // not a legal digit
                            throw new IllegalArgumentException(s);
                        if (isFirst)
                            first = n + first*10;
                        else
                            last = n + last*10;
                    }
                }
            } catch (IndexOutOfBoundsException e) {
                // '[' without matching ']'
                throw new IllegalArgumentException(s);
            }

            if (isFirst)        // no '-' in range
                throw new IllegalArgumentException(s);

            String[] suffixes = expandRanges(s.substring(i + 1));
            if (last < first) {
                int tmp = last;
                last = first;
                first = tmp;
            }

            String[] result = new String[suffixes.length * (1 + last - first)];
            int p = 0;
            for (int j = first; j <= last; j++) {
                String num = Integer.toString(j);
                for (int k = 0; k < suffixes.length; k++)
                    result[p++] = prefix + num + suffixes[k];
            }

            return result;
        }
    }

    /**
     * Given a String, substitute anything that looks like
     * "$(variable)" with the value of the variable, looked up
     * in the given Map.
     */
    public static String substituteVariables(String s, Map vars) {
        StringBuffer result = new StringBuffer();
        boolean dollar = false;

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (dollar && c == '(') {
                result.setLength(result.length() - 1); // remove '$'
                int j = s.indexOf(')', i);
                if (j == -1)
                    throw new IllegalArgumentException(s);
                String varname = s.substring(i + 1, j);
                i = j;
                result.append(vars.get(varname));
            } else {
                result.append(c);
            }
            dollar = (c == '$');
        }

        return result.toString();
    }

    /**
     * Return an escaped string using \ as the escape character.  Non-printable
     * characters, \, and &quot; are escaped.  If a control character exists in
     * <code>m</code>, then the String corresponding to that character from
     * <code>m</code> is used as its escape sequence, otherwise, a 3-digit
     * octal escape sequence is used.
     **/
    public static String quoteASCIIString(final String s, final Map m) {
        final StringBuffer buf = new StringBuffer();
        for (int i = 0; i < s.length(); ++i) {
            final char c = s.charAt(i);
            assert c <= 255;
            if (Character.isISOControl(c)) {
                final String mapped = (String) m.get(new Character(c));
                if (mapped == null) {
                    buf.append('\\');
                    buf.append(padLeft(Integer.toOctalString(c), 3, '0'));
                } else {
                    buf.append(mapped);
                }
            } else if (c == '\\') {
                buf.append("\\\\");
            } else if (c == '"') {
                buf.append("\\\"");
            } else {
                buf.append(c);
            }
        }
        return buf.toString();
    }

    /**
     * Converts a glob expression to a regular expression.
     **/
    public static String getRegexForGlob(String regex) {
        StringBuilder buf = new StringBuilder("\\Q");
        for (int loop=0;loop<regex.length();loop++) {
            switch(regex.charAt(loop)) {
              case '?':
                buf.append("\\E[^\\.]*\\Q");
                break;
              case '*':
                buf.append("\\E.*\\Q");
                break;
              default:
                buf.append(regex.charAt(loop));
            }
        }
        buf.append("\\E");
        return buf.toString();
    }

    /**
     * DO NOT USE, class has only static methods.
     * @deprecated Do not invoke, this class has only static methods.
     **/
    private StringUtil() {
        throw new AssertionFailure("StringUtil self-instantiation");
    }

    // private static class TestStringUtil {
        public static void main(String[] args) {
            final String orig = "icacher.s2p.XR[6][0][0].0";
            final String desired = "icacher.s2p.XR[6,0,0].0";
            final String got = StringUtil.replaceSubstring(orig, "][", ",");

            if (got.equals(desired))
                System.err.println("ok");
            else
                System.err.println("failed: got " + got);
        }
    // }
}
