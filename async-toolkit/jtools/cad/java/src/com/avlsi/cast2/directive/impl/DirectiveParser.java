/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import com.avlsi.cast2.directive.impl.DirectiveFactoryInterface;
import com.avlsi.cast2.directive.impl.DirectiveStatement;
import com.avlsi.cast2.directive.impl.DirectiveSyntaxException;
import com.avlsi.cast2.directive.impl.TokenInfo;
import com.avlsi.io.PositionStackReader;

public class DirectiveParser {
    private final PositionStackReader reader;
    private final DirectiveFactoryInterface factory;
    private static final HashSet valueSet = new HashSet();
    private static final HashSet identSet = new HashSet();
    private int lineNum, colNum;
    private final String file;
    private final Stack lineStack;

    static {
        valueSet.add(new Character(';'));
        identSet.add(new Character(':'));
    }

    public DirectiveParser(Reader in, DirectiveFactoryInterface factory,
                           int lineNum, int colNum, String file) {
        this.reader = new PositionStackReader(in);
        this.factory = factory;
        this.lineNum = lineNum;
        this.colNum = colNum;
        this.file = file;
        this.lineStack = new Stack();
    }

    private int read() throws IOException {
        int c = reader.read();
        colNum++;
        if (c == '\n') {
            lineNum++;
            colNum = 0;
        }
        return c;
    }

    private void save() throws IOException {
        reader.savePosition();
        lineStack.push(new int[] { lineNum, colNum });
    }

    private void restore() throws IOException {
        reader.restorePosition();
        int[] saved = (int[]) lineStack.pop();
        lineNum = saved[0];
        colNum = saved[1];
    }

    private void discard() throws IOException {
        reader.discardPosition();
        lineStack.pop();
    }

    private boolean skipCPPComment() throws IOException {
        boolean status = false;
        save();
        int c = read();
        if (c != '/') { restore(); return status; }
        c = read();
        if (c != '/') { restore(); return status; }
        discard(); save();
        c = read();
        while (c != -1 && c != '\n') {
            status = true;
            discard();
            save();
            c = read();
        }
        restore();
        return status;
    }

    private boolean skipCComment() throws IOException {
        boolean status = false;
        boolean nostar = true;
        save();
        int c = read();
        if (c != '/') { restore(); return status; }
        c = read();
        if (c != '*') { restore(); return status; }
        discard(); save();
        c = read();
        while (c != -1 && (nostar || c != '/')) {
            status = true;
            discard();
            save();
            nostar = c != '*';
            c = read();
        }
        if (c == '/') discard();
        else restore();
        return status;
    }

    private boolean skipRealWS() throws IOException {
        boolean status = false;
        save();
        int c = read();
        while (c != -1 && Character.isWhitespace((char) c)) {
            status = true;
            discard();
            save();
            c = read();
        }
        restore();
        return status;
    }

    private void skipWS() throws IOException {
        while (skipCComment() || skipCPPComment() || skipRealWS());
    }

    private String lexKey() throws IOException {
        StringBuffer buf = new StringBuffer();
        save();
        char c = (char) read();
        if (Character.isLetter(c) || c == '_') {
            discard();
            buf.append(c);
        } else {
            restore();
            return null;
        }

        save();
        c = (char) read();
        while (Character.isLetter(c) || Character.isDigit(c) || c == '_') {
            discard();
            buf.append(c);
            save();
            c = (char) read();
        }

        if (c == '+' || c == '-') {
            buf.append(c);
            discard();
        } else {
            restore();
        }

//System.err.println("key = " + buf);
        return buf.toString();
    }

    private String lexUntil(Set until) throws IOException {
        StringBuffer buf = new StringBuffer();
        int c;
        save();
        c = read();
        while (c != -1 && !until.contains(new Character((char) c))) {
            buf.append((char) c);
            discard();
            save();
            c = read();
        }
        if (c == -1) {
            discard();
        } else {
            restore();
        }
//System.err.println("lexUntil = " + buf);
        return buf.toString();
    }

    private String lexUntil(final String str) throws IOException {
        StringBuffer buf = new StringBuffer();
        StringBuffer sstr = new StringBuffer();
        int c;
        c = read();
        while (c != -1 && !str.equals(sstr.toString())) {
            buf.append((char) c);
            if (sstr.length() == str.length()) {
                sstr.deleteCharAt(0);
            }
            sstr.append((char) c);
            c = read();
        }
//System.err.println("lexUntil = " + buf);
        return buf.toString();
    }

    private String lexParameter() throws IOException {
        StringBuffer buf = new StringBuffer();
        int c, count = 0;
        save();
        c = read();
        while (c != -1 && (count != 0 || c != ')')) {
            if (c == '(') count++;
            else if (c == ')') {
                count--;
                if (count < 0) System.err.println("Unmatched parenthesis");
            }
            buf.append((char) c);
            discard();
            save();
            c = read();
        }
        if (c == -1) {
            discard();
            System.err.println("Premature EOF while reading parameter");
        } else {
            restore();
        }
//System.err.println("parameter = " + buf);
        return buf.toString();
    }

    private String lexIdent() throws IOException {
        String ident = lexUntil(identSet);
        return ident;
    }

    private String lexValue() throws IOException {
        String value = lexUntil(valueSet);
        return value;
    }

    public DirectiveStatement parseDirective() throws IOException {
        skipWS();
        final int current = lineNum;
        String key = lexKey();
        if (key == null) return null;
        skipWS();
        save();
        char c = (char) read();
        String param = null, value = null;
        if (c == '(') {
            param = lexParameter();
            c = (char) read();
            if (c != ')') {
                restore();
                return null;
            }
            skipWS();
            c = (char) read();
        }
        if (c != '=') {
            restore();
            return null;
        }
        value = lexValue();
        if (value == null) return null;

        c = (char) read();
        if (c != ';') {
            restore();
            return null;
        }

        // XXX: In the future, need to handle an array of values
        if (param == null) {
            return factory.makeDefinition(key, value,
                                          new TokenInfo(current, file));
        } else {
            return factory.makeDefinition(key, param, value,
                                          new TokenInfo(current, file));
        }
    }

    public void parseRange(String[] range) throws IOException {
        skipWS();
        String fromto = lexUntil(identSet);
        final int index = fromto.indexOf("..");
        if (index < 0) {
            range[0] = fromto;
            range[1] = null;
        } else {
            range[0] = fromto.substring(0, index);
            range[1] = fromto.substring(index + 2);
        }
    }

    public DirectiveStatement parseConditional()
        throws IOException, DirectiveSyntaxException {
        save();
        char c = (char) read();
        final int current = lineNum;
        if (c != '[') {
            restore();
            return null;
        }

        String guard = lexUntil("->");
        if (guard.endsWith("->")) {
            guard = guard.substring(0, guard.length() - 2);
        } else {
            restore();
            return null;
        }

        DirectiveStatement[] body = parseStatements(true);

        c = (char) read();
        if (c != ']') {
            restore();
            return null;
        }
//System.err.println("loop " + ident + " " + range[0] + " " + range[1]);
        discard();
        return factory.makeConditional(guard, body, new TokenInfo(current, file));
    }

    public DirectiveStatement parseLoop() throws IOException,
                                                 DirectiveSyntaxException {
        save();
        char c = (char) read();
        final int current = lineNum;
        if (c != '<') {
            restore();
            return null;
        }

        String ident = lexIdent();

        if (ident == null) {
            restore();
            return null;
        }

        c = (char) read();
        if (c != ':') {
            restore();
            return null;
        }

        String[] range = new String[2];
        parseRange(range);

        c = (char) read();
        if (c != ':') {
            restore();
            return null;
        }

        DirectiveStatement[] body = parseStatements(true);

        c = (char) read();
        if (c != '>') {
            restore();
            return null;
        }
//System.err.println("loop " + ident + " " + range[0] + " " + range[1]);
        discard();
        return factory.makeLoop(ident, range[0], range[1], body,
                                new TokenInfo(current, file));
    }

    public DirectiveStatement parseStatement(boolean failOkay)
        throws IOException, DirectiveSyntaxException {
        save();
        int c = read();
        final int curLine = lineNum;
        final int curCol = colNum;
        restore();
        DirectiveStatement stmnt;
        if (c == '<') {
            stmnt = parseLoop();
            if (stmnt == null)
                throw new DirectiveSyntaxException(
                    "Malformed directive loop statement", file, curLine, curCol);
        } else if (c == '[') {
            stmnt = parseConditional();
            if (stmnt == null)
                throw new DirectiveSyntaxException(
                    "Malformed directive if statement", file, curLine, curCol);
        } else {
            stmnt = parseDirective();
            if (stmnt == null && !failOkay)
                throw new DirectiveSyntaxException(
                    "Malformed directive statement", file, curLine, curCol);
        }
        return stmnt;
    }

    public DirectiveStatement[] parseStatements() throws IOException,
                                                  DirectiveSyntaxException {
        return parseStatements(false);
    }

    public DirectiveStatement[] parseStatements(boolean failOkay)
        throws IOException, DirectiveSyntaxException {
        ArrayList body = new ArrayList();
        int count = 0;
        skipWS();
        save();
        int c = read();
        while (c != -1) {
//System.err.println("count = " + count);
            restore();
            DirectiveStatement line = parseStatement(failOkay);
            if (line == null) break;
            else body.add(line);
            skipWS();
            save();
            c = read();
            count++;
        }
        return (DirectiveStatement[]) body.toArray(new DirectiveStatement[0]);
    }
}
