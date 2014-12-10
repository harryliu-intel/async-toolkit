package com.avlsi.csp.grammar;

import com.avlsi.util.debug.Debug;
import antlr.Token;
import com.avlsi.cast.impl.TokenWithInfo;

// XXX: use a different class, or put this class in a different place
import com.avlsi.csp.coverage.ParseException;

/**
 * Class representing a position in a file, used for reporting parse
 * errors and such. @see ParseRange
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class ParsePosition implements Cloneable {
    public int line;
    public int column;
    public String filename;

    private static final String emptyFilename="empty_parse_position";

    /** create an empty ParsePosition for use as a placeholder **/
    public ParsePosition() {
        line=0; column=0; filename=emptyFilename;
    }

    public ParsePosition(int line, int column, String filename) {
        this.line = line; this.column = column; 
        this.filename = filename.intern();
    }
    /** Create a ParsePosition pointing to the first character of a
     * token **/
    public ParsePosition(Token t) {
        this.line = t.getLine();
        this.column = t.getColumn()-1;
        this.filename = ((TokenWithInfo)t).getFilename().intern();
    }
    /** Assuming that the string s directly follows this
     * ParsePosition, return a ParsePosition pointing to the character
     * after s. **/
    public ParsePosition addString(String s) {
        /* XXX: could scan string for newlines and increment line
           number accordingly, although probably not necessary since
           no tokens have newlines in them */
        ParsePosition p=null;
        try {
            p=(ParsePosition)this.clone();
        } catch(CloneNotSupportedException e) {
            Debug.assertTrue(false);
        }
        p.column += s.length();
        return p;
    }
    public int compareTo(ParsePosition pp) {
        if(line!=pp.line)
            return line-pp.line;
        else
            return column-pp.column;
    }
    public boolean equals(Object o) {
        if (o instanceof ParsePosition) {
            ParsePosition p = (ParsePosition) o;
            return line == p.line && column == p.column &&
                   filename == p.filename;
        } else {
            return false;
        }
    }
    public int hashCode() {
        return filename.hashCode() + line + column;
    }
    public static void checkFiles(ParsePosition a, ParsePosition b) {
        Debug.assertTrue(a.filename == b.filename
                         || a.filename == emptyFilename
                         || b.filename == emptyFilename);
    }
    public static ParsePosition max(ParsePosition a,
                                    ParsePosition b) {
        checkFiles(a,b);
        if(a.compareTo(b)<0) return b;
        else return a;
    }
    public static ParsePosition min(ParsePosition a,
                                    ParsePosition b) {
        checkFiles(a,b);
        if(a.compareTo(b)<0) return a;
        else return b;
    }

    private static final String separator=":";
    /** Create a ParsePosition from a string of the form AA:BB, and a
     * file name **/
    public ParsePosition(String s, String filename)
        throws ParseException {
        String substrs[]=s.split(separator,2);
        if(substrs.length!=2) throw new ParseException("Expected "+separator);
        try {
            line = new Integer(substrs[0]).intValue();
            column = new Integer(substrs[1]).intValue();
        } catch(NumberFormatException e) {
            throw new ParseException("Expected an integer");
        }
        this.filename = filename.intern();
    }
    /** Return a string of the form AA:BB **/
    public String toString() {
        return line+separator+column;
    }
}
