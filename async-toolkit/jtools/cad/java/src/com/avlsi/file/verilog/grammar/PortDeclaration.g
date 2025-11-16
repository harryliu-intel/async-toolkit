//
// $Id$
//
// ------------------------------------------------------------------------
// Original author: Steve Eckmann        eckmann@anl.gov
// ------------------------------------------------------------------------

header {
    package com.avlsi.file.verilog.grammar;
}

// Import the necessary classes
{
    import java.io.FileInputStream;
    import java.util.Iterator;
    import java.util.ArrayList;
    import java.util.HashMap;
    import java.util.List;
    import java.util.Map;
    import antlr.CommonAST;
    import antlr.collections.AST;
}

//-----------------------------------------------------------------------------
// A TreeParser to extract a module's port declarations
//-----------------------------------------------------------------------------
class PortDeclarationTreeParser extends TreeParser;
options {
    importVocab = Verilog;
    defaultErrorHandler = false;
}
{
    public static class Range {
        private final int left, right;
        public Range(int left, int right) {
            this.left = left;
            this.right = right;
        }
        public int getRight() {
            return right;
        }
        public int getLeft() {
            return left;
        }
        public int hashCode() {
            return left << 4 + right;
        }
        public boolean equals(Object o) {
            if (o instanceof Range) {
                final Range r = (Range) o;
                return left == r.left && right == r.right;
            } else {
                return false;
            }
        }
        public String toString() {
            return "[" + left + ":" + right + "]";
        }
    }

    public static void getPortDeclarations(final FileInputStream s,
                                           final Map result) throws Exception {
        final VerilogLexer lexer = new VerilogLexer(s);
        final VerilogParser parser = new VerilogParser(lexer);
        //antlr.BaseAST.setVerboseStringConversion(true, parser.getTokenNames());
        parser.source_text();
        final AST code = parser.getAST();
        final PortDeclarationTreeParser treeParser =
            new PortDeclarationTreeParser();
        treeParser.goal(code, result);
    }

    public static void main(String[] args) throws Exception {
        for(int i = 0; i < args.length; i++) {
            final FileInputStream fis = new FileInputStream(args[i]);
            System.out.println("Port declarations in " + args[i] + ":");
            final Map result = new HashMap();
            getPortDeclarations(fis, result);
            System.out.println(result);
        }
    }
}

goal [Map modules]
    : (elements[modules])*
    ;

elements [Map modules]
    : module[modules]
    | .
    ;

module [Map modules]
    {
        String name;
        Map decls = new HashMap();
    }
    : LITERAL_module name = local_identifier
      ( options { greedy = false; } : module_elements[decls] )*
      LITERAL_endmodule { modules.put(name, decls); }
    ;

module_elements [Map decls]
    {
        List vars;
        Range r = null;
    }
    : ( LITERAL_input | LITERAL_output ) ( r = range )?
      vars = list_of_variables SEMI {
          for (Iterator i = vars.iterator(); i.hasNext(); ) {
              decls.put(i.next(), r);
          }
      }
    | .
    ;

range returns [Range range = null]
    {
        int left, right;
    }
    : LBRACK l:NUMBER { left = Integer.parseInt(l.getText()); }
      COLON r:NUMBER { right = Integer.parseInt(r.getText()); }
      RBRACK { range = new Range(left, right); }
    ;

list_of_variables returns [List vars]
    {
        String var;
        vars = new ArrayList();
    }
    : var = local_identifier { vars.add(var); }
      ( COMMA var = local_identifier { vars.add(var); } )*
    ;

local_identifier returns [String id = null]
    : id1:IDENTIFIER { id = id1.getText(); }
    | id2:ESCAPED_IDENTIFIER { id = id2.getText(); }
//  | DEFINE
    ;
