package com.avlsi.csp.coverage;

import com.avlsi.csp.grammar.ParseRange;

/** Encapsulates information about a coverage probe (@see Monitor),
 * namely the corresponding location in the source file and a string
 * representing the kind of probe.
**/
public class ProbeInfo {
    final ParseRange range;
    final String kind;
    final String cellType;

    public ProbeInfo(ParseRange range, String kind, String cellType) {
        this.range = range;
        this.kind = kind;
        this.cellType = cellType;
    }

    public ProbeInfo(String s) throws ParseException {
        String words[]=s.split(" ",4); // XXX: how about " +" ?
        if(words.length!=4) throw new ParseException("Malformed probe info line");
        String filename=words[0];
        range=new ParseRange(words[1],filename);
        cellType=words[2];
        kind=words[3];
    }

    /** return a space-delimited string: range.start.filnename + range
     * + kind **/
    public String toString() {
        return range.start.filename+" "+range.toString()+" "+cellType+" "+kind;
    }

    /** return a string for use in error messages **/
    public String missedString() {
        return range.start.filename+":"+range.toString()+" ("+cellType+"):  "+kind;
    }

    /** return a string of java code which can be used to reconstruct
     * this object **/
    public String constructorString() {
        return "new ProbeInfo(new ParseRange("+
            range.start.line+", "+range.start.column+", "+
            range.end.line+", "+range.end.column+", \""+
            range.start.filename+"\"), \""+kind+"\", \""+
            cellType+"\")";
    }
}
