package com.avlsi.csp.grammar;

import com.avlsi.util.debug.Debug;
import com.avlsi.csp.coverage.ParseException;
import antlr.Token;

/**
 * Class representing a start and end position in a file, used for
 * reporting parse errors and such.
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class ParseRange {
    public final ParsePosition start;
    public final ParsePosition end;
    public final static ParseRange EMPTY = new ParseRange();
    
    /** Generate an empty parse range when one can't be found **/
    private ParseRange() {
        start = new ParsePosition(); 
        end = new ParsePosition();
    }
    /** Create a new ParseRange, specifying start and end positions **/
    public ParseRange(ParsePosition start,
                      ParsePosition end) {
        this.start = start; this.end = end;
        Debug.assertTrue(start.compareTo(end)<=0,
                     "ParseRange instantiated with invalid range");
        // XXX: eventually it might be better to allow statements
        // across files (e.g. if there is a preprocessor)
        Debug.assertTrue(start.filename==end.filename);
    }
    /** Create a new ParseRange with starting and ending line and
     * column numbers and a file name **/
    public ParseRange(int sl, int sc, int el, int ec, String filename) {
        this.start = new ParsePosition(sl,sc,filename);
        this.end = new ParsePosition(el,ec,filename);
    }
    /** Create a new ParseRange which is the convex closure of two
     * ParseRange's **/
    public ParseRange(ParseRange a, ParseRange b) {
        this.start = ParsePosition.min(a.start,b.start);
        this.end = ParsePosition.max(a.end,b.end);
    }
    /** Create a new ParseRange representing the span of characters in
     * a token **/
    public ParseRange(Token t) {
        this.start = new ParsePosition(t);
        this.end = (new ParsePosition(t)).addString(t.getText());
    }
    /** Separator character for printing and parsing **/
    private static final String separator="-";
    /** Create a new ParseRange from an expression of the form
     * AA:BB-CC:DD and a file name **/
    public ParseRange(String s, String filename) throws ParseException {
        // parse a range expression of the form AA:BB-CC:DD
        String substrs[]=s.split(separator,2);
        if(substrs.length!=2) throw new ParseException("Expected "+separator);
        start = new ParsePosition(substrs[0], filename);
        end = new ParsePosition(substrs[1], filename);
    }
    /** Return a string representing the range in form AA:BB-CC:DD**/
    public String toString() {
        return start.toString()+separator+end.toString();
    }
    /** Same as toString, but prepend filename+":". Use this for most
     * error messages. **/
    public String fullString() {
        return start.filename+":"+this;
    }
    public boolean equals(Object o) {
        if (o instanceof ParseRange) {
            ParseRange p = (ParseRange) o;
            return start.equals(p.start) && end.equals(p.end);
        } else {
            return false;
        }
    }
    public int hashCode() {
        return start.hashCode() + end.hashCode();
    }
}
