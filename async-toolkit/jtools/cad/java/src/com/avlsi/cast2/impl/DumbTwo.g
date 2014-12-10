//
// Copyright 2000 Asynchronous Digital Design.  All rights reserved.
//
// $Id$
//

//
// dumb parser for blocks we do not yet understand.
// (for use with antlr http://www.antlr.org/)
//
// author: Jesse Rosenstock
//

header {
    ///////////////////////////////////////////////////////////////////////
    //
    // Copyright 2000 Asynchronous Digital Design.  All rights reserved.
    //
    // Warning:  This file was AUTOMATICALLY GENERATED!!!
    //
    // DO NOT check in.
    // DO NOT modify.
    //
    // You want to modify DumbTwo.g instead.
    //
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.cast2.impl;
}

{
    import java.io.*;

    import com.avlsi.cast.impl.DumbParserInterface;
}

class DumbTwoParser extends Parser;
options {
    classHeaderSuffix = DumbParserInterface;
    exportVocab = DumbTwo;
}

{
  // Define a main
  public static void main(String[] args) {
    // Use a try/catch block for parser exceptions
    try {
      // if we have at least one command-line argument
      if (args.length > 0 ) {
        System.err.println("Parsing...");

        // for each directory/file specified on the command line
        for(int i=0; i< args.length;i++)
          doFile(new File(args[i])); // parse it
      }
      else
        System.err.println("Usage: java DumbTwoParser <file/directory name>");

    }
    catch(Exception e) {
      System.err.println("exception: "+e);
      e.printStackTrace(System.err);   // so we can get stack trace
    }
  }


  // This method decides what action to take based on the type of
  //   file we are looking at
  public static void doFile(File f) throws Exception {
    // If this is a directory, walk each file/dir in that directory
    if (f.isDirectory()) {
      String files[] = f.list();
      for(int i=0; i < files.length; i++)
        doFile(new File(f, files[i]));
    }

    // otherwise, if this is a DumbTwo file, parse it!
    else {
        System.err.println(f.getName());
      System.err.println("   "+f.getAbsolutePath());
      parseFile(new FileInputStream(f));
    }
  }

  // Here's where we do the real work...
  public static void parseFile(InputStream s) throws Exception {
    try {
      // Create a scanner that reads from the input stream passed to us
      DumbTwoLexer lexer = new DumbTwoLexer(s);

      // Create a parser that reads from the scanner
      DumbTwoParser parser = new DumbTwoParser(lexer);

      // start parsing at the program rule
        parser.goal();
    }
    catch (Exception e) {
      System.err.println("parser exception: "+e);
      e.printStackTrace();   // so we can get stack trace
    }

}
}

goal returns [String content]
{ content = null; 
  StringBuffer g = new StringBuffer(); }
    : (d:DUMB_BLOCK { g.append(d.getText()); })* RCURLY!
        {
            content = g.toString();
        }
    ;

class DumbTwoLexer extends Lexer;

options {
    classHeaderSuffix = com.avlsi.cast.impl.DumbLexerInterface;
    charVocabulary = '\0'..'\377';
    exportVocab = DumbTwo;
    filter = true;
}

RCURLY : '}' ;

DUMB_BLOCK
    : ( BRACED_BLOCK | NONBRACE )
    ;

protected
BRACED_BLOCK
    : '{' (DUMB_BLOCK)* '}'
    ;

protected
NONBRACE
    : (options { generateAmbigWarnings = false; } :
    ('\n' { newline(); } ) | ('\0'..'z') | '|' | ('~'..'\377'))+
    { $setType(Token.SKIP); }
    ;
