//
// Copyright 2002 Fulcrum Microsystems.  All rights reserved.
//
// $Id$
// $DateTime$
// $Author$
//

// author: Jesse Rosenstock
// version: $Revision$ $Date$

header {
    package com.avlsi.tools.cosim.spec;
}
{
    import java.util.ArrayList;
    import java.util.List;
}

class CoSimParser extends Parser;
options {
    buildAST = false;
    k = 3;
    defaultErrorHandler = false;
}
{
    /**
     * The default mode list when one isn't supplied in envLevelSpec.
     * Also used by defaultCoSimSpecList() for the default in a levelSpec.
     * Default is: <code>java, csp, subcells, prs, spice</code>
     **/
    private ModeList defaultModeList() {
        return new ModeList(new Mode[] {
            Mode.JAVA, Mode.CSP, Mode.SUBCELLS, Mode.PRS, Mode.SPICE
        });
    }

    /**
     * Returns a CoSimSpec containing one CoSimSpec specifying the
     * given modes and no instance exceptions.
     **/
    private CoSimSpec coSimSpecFromModeList(final ModeList modeList) {
        return new CoSimSpec(
            new ModeListLevelSpec(modeList),
            new InstSpecList(new InstSpec[0]));
    }
    /**
     * Returns a CoSimSpecList containing one CoSimSpec specifying the
     * given modes and no instance exceptions.
     **/
    private CoSimSpecList coSimSpecListFromModeList(final ModeList modeList) {
        return new CoSimSpecList(new CoSimSpec[]{
            coSimSpecFromModeList(modeList)
        });
    }

    /**
     * The default cosimSpecList when one isn't supplied in a levelSpec.
     * Default is: <code>java,csp,subcells,prs,spice</code>
     **/
    private CoSimSpecList defaultCoSimSpecList() {
        return coSimSpecListFromModeList(defaultModeList());
    }

    /**
     * The default CoSimSpecList when one isn't specified at the top level.
     * Default is:
     * <code>java,csp,subcells,prs,spice | prs,subcells,csp,java</code>.
     **/
    private CoSimSpecList defaultTopCoSimSpecList() {
        return new CoSimSpecList(new CoSimSpec[] {
            coSimSpecFromModeList(defaultModeList()),
            defaultEnvCoSimSpec()
        });
    }

    /**
     * The default CoSimSpec when one isn't supplied as the envSpec.
     * Default is: <code>prs,subcells,csp,java</code>.
     **/
    private CoSimSpec defaultEnvCoSimSpec() {
        return new CoSimSpec(
            new ModeListLevelSpec(
                new ModeList(new Mode[] {
                    Mode.PRS, Mode.SUBCELLS, Mode.CSP, Mode.JAVA
                })),
            new InstSpecList(new InstSpec[0]));
    }

    private int intValue(final String s) {
        // XXX: throws NumberFormatException if number is too large
        if (s.startsWith("0x"))
            return Integer.parseInt(s.substring(2), 16);
        else
            return Integer.parseInt(s);
    }

    private CoSimSpecList topCoSimSpecList = defaultTopCoSimSpecList();
    private CoSimSpec envCoSimSpec = defaultEnvCoSimSpec();

    public void setTopCoSimSpecList(final CoSimSpecList topCoSimSpecList) {
        this.topCoSimSpecList = topCoSimSpecList;
    }
    public void setEnvCoSimSpec(final CoSimSpec envCoSimSpec) {
        this.envCoSimSpec = envCoSimSpec;
    }
}
// ANTLR only expects EOFs after "start rules", which are rules
// that are not referred to by any in the grammar.  If we don't
// have this rule, EOF will break us!
cosim returns [CoSim coSim]
    { String cellType, envName;
      CoSimSpecList coSimSpecList = null;
      CoSimSpec envSpec = null; }
    // XXX
    // default cosimSpecList is 0(java,csp,prs,spice)|*(prs,csp,java)
    // default envSpec is *(prs,csp,java)
    : cellType=type ( LCURLY coSimSpecList=cosimSpecList RCURLY )?
      COLON envName=type ( LCURLY envSpec=envSpec RCURLY )?
    { coSim =
          new CoSim(cellType,
              coSimSpecList != null ? coSimSpecList : topCoSimSpecList,
              envName,
              envSpec != null ?  envSpec : envCoSimSpec); }
    ;
cosimEnvOptional returns [CoSim coSim]
    { String cellType, envName = null;
      CoSimSpecList coSimSpecList = null;
      CoSimSpec envSpec = null; }
    // XXX
    // default cosimSpecList is 0(java,csp,prs,spice)|*(prs,csp,java)
    // default envSpec is *(prs,csp,java)
    : cellType=type ( LCURLY coSimSpecList=cosimSpecList RCURLY )?
      ( COLON envName=type ( LCURLY envSpec=envSpec RCURLY )? )?
    { coSim =
          new CoSim(cellType,
              coSimSpecList != null ? coSimSpecList : topCoSimSpecList,
              envName,
              envSpec != null ?  envSpec : envCoSimSpec); }
    ;

cosimSpecDirective returns [CoSimSpec coSimSpec]
    : coSimSpec=cosimSpec EOF
    ;

// consider moveing LCURLY ... RCURLY inside cosimSpecList
cosimSpecList returns [CoSimSpecList coSimSpecList]
    { List l = new ArrayList(); CoSimSpec coSimSpec; }
    : coSimSpec=cosimSpec {l.add(coSimSpec);}
      ( PIPE coSimSpec=cosimSpec {l.add(coSimSpec);} )*
    { coSimSpecList =
          new CoSimSpecList((CoSimSpec[]) l.toArray(new CoSimSpec[0])); }
    ;
cosimSpec returns [CoSimSpec coSimSpec]
    { LevelSpecInterface levelSpec; InstSpecList instSpecList; }
    : levelSpec=levelSpec instSpecList=instSpecList
    { coSimSpec = new CoSimSpec(levelSpec, instSpecList); }
    ;
levelSpec returns [LevelSpecInterface levelSpec]
    { int level; ModeList modeList;
      CoSimSpecList coSimSpecList = null; }
    : level=level ( LCURLY coSimSpecList=cosimSpecList RCURLY )?
      { levelSpec = new CoSimLevelSpec(level, coSimSpecList != null ?
                                                  coSimSpecList :
                                                  defaultCoSimSpecList()); }
    | modeList=modeList
      { levelSpec = new ModeListLevelSpec(modeList); }
    ;
modeList returns [ModeList modeList]
    { Mode mode; List l = new ArrayList(); }
    : ( mode=mode {l.add(mode);} ( COMMA mode=mode {l.add(mode);} )* )?
    { modeList = new ModeList((Mode[]) l.toArray(new Mode[0])); }
    ;
level returns [int level]
    { String l; }
    : l=integer { level = intValue(l); }
    ;
mode returns [Mode mode]
    { String level = null; }
    : JAVA  { mode = Mode.JAVA;  }
    | CSP   { mode = Mode.CSP;   }
    | SUBCELLS { mode = Mode.SUBCELLS;   }
    | PRS   { mode = Mode.PRS;   }
    | SPICE { mode = Mode.SPICE; }
    | VERILOG ( DOT level=ident )? { mode = new Mode.VerilogMode(level); }
    ;
instSpecList returns [InstSpecList instSpecList]
    { InstSpec instSpec; List l = new ArrayList(); }
    : ( MINUS instSpec=instSpec {l.add(instSpec);} )*
    { instSpecList =
          new InstSpecList((InstSpec[]) l.toArray(new InstSpec[0])); }
    ;
instSpec returns [InstSpec instSpec]
    { String instName; CoSimSpecList coSimSpecList; }
    : instName=instance LCURLY coSimSpecList=cosimSpecList RCURLY
    { instSpec = new InstSpec(instName, coSimSpecList); }
    ;
envSpec returns [CoSimSpec coSimSpec]
    { LevelSpecInterface levelSpec; InstSpecList instSpecList; }
    : levelSpec=envLevelSpec instSpecList=envInstSpecList
    { coSimSpec = new CoSimSpec(levelSpec, instSpecList); }
    ;
envLevelSpec returns [LevelSpecInterface levelSpec]
    { int level; ModeList modeList = null; }
    : level=level ( LCURLY modeList=modeList RCURLY )?
      { levelSpec =
            new CoSimLevelSpec(level,
                coSimSpecListFromModeList(modeList != null ?
                                              modeList :
                                              defaultModeList())); }
    | modeList=modeList
      { levelSpec = new ModeListLevelSpec(modeList); }
    ;
envInstSpecList returns [InstSpecList instSpecList]
    { InstSpec instSpec; List l = new ArrayList(); }
    : ( MINUS instSpec=envInstSpec {l.add(instSpec);} )*
    { instSpecList =
          new InstSpecList((InstSpec[]) l.toArray(new InstSpec[0])); }
    ;
envInstSpec returns [InstSpec instSpec]
    { String instName; ModeList modeList; }
    : instName=instance LCURLY modeList=modeList RCURLY
    { instSpec =
          new InstSpec(instName, coSimSpecListFromModeList(modeList)); }
    ;
type returns [String s]
    { String id; StringBuffer sb = new StringBuffer(); }
    : id=ident {sb.append(id);} ( metaParamList[sb] )?
      ( DOT {sb.append('.');}
        id=ident {sb.append(id);} ( metaParamList[sb] )? )*
    { s = sb.toString(); }
    ;
metaParamList[StringBuffer sb]
    : LPAREN {sb.append('(');}
      metaParam[sb] ( COMMA {sb.append(',');} metaParam[sb] )*
      RPAREN {sb.append(')');}
    ;
metaParam[StringBuffer sb]
    { String i; }
    : ( MINUS {sb.append('-');} )? i=integer {sb.append(i);}
    | TRUE      {sb.append("true"); }
    | FALSE     {sb.append("false");}
    | LCURLY {sb.append('{');}
      metaParam[sb] ( COMMA {sb.append(',');} metaParam[sb] )*
      RCURLY {sb.append('}');}
    | s:QuotedString {sb.append(s.getText());}
    ;
// this could be tightened up since x[0][1] is not supported, only x[0,1]
instance returns [String s]
    { String id; StringBuffer sb; }
    : id=ident {sb = new StringBuffer(id);} ( selector[sb] )*
    { s = sb.toString(); }
    ;
selector[StringBuffer sb]
    { String i, id; }
    : DOT {sb.append('.');}
      id=ident {sb.append(id);}
    | LBRACK {sb.append('[');}
      i=integer {sb.append(i);}
      ( COMMA i=integer {sb.append(','); sb.append(i);} )*
      RBRACK {sb.append(']');}
    ;
ident returns [String s]
    : id:IDENT { s = id.getText(); }
    | i:INT { s = i.getText(); }
    ;
integer returns [String i]
    : t:INT { i = t.getText(); }
    ;
// need to handle "0" or "spice" quoting

class CoSimLexer extends Lexer;
options {
    k = 3;
    charVocabulary = '\3' .. '\377';  // needed for ~ to make sense
}
tokens {
    JAVA="java";
    CSP="csp";
    SUBCELLS="subcells";
    PRS="prs";
    SPICE="spice";
    VERILOG="verilog";
    FALSE="false";
    TRUE="true";
}

WS : (' ' | '\t' | '\r' | '\n' { newline(); } ) { _ttype = Token.SKIP; } ;
protected ALPHA : (('a'..'z') | ('A'..'Z') | '_') ;
protected DIGIT : ('0'..'9') ;
protected HEX_DIGIT : DIGIT | 'a'..'f' | 'A'..'F' ;
protected REAL_IDENT : ALPHA (ALPHA | DIGIT)* ;
IDENT  : ('@')? REAL_IDENT ;

INT : { LA(1) == '0' && LA(2) == 'x' }? "0x" (HEX_DIGIT)+
    | (DIGIT)+ (REAL_IDENT { $setType(IDENT); })?
    ;

LBRACK : '[' ; RBRACK : ']' ;
LCURLY : '{' ; RCURLY : '}' ;
LPAREN : '(' ; RPAREN : ')' ;
COLON  : ':';
COMMA  : ',';
DOT    : '.';
MINUS  : '-' ;
PIPE   : '|' ;
QuotedString: '\'' (~'\'')* '\'';
