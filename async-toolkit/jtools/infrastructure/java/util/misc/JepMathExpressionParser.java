package com.fulcrummicro.util.misc;

import java.util.ArrayList;
import java.util.Stack;

import org.nfunk.jep.JEP;
import org.nfunk.jep.ParseException;
import org.nfunk.jep.function.PostfixMathCommand;

public class JepMathExpressionParser extends JEP {

    public JepMathExpressionParser() {
        super();
        addParserFunctions(this);
        addCosimFunction(this);
    }
    
    public static void addCosimFunction(JEP parser) {
        abstract class TwoStringsPostfixMathCommand extends PostfixMathCommand {
            public TwoStringsPostfixMathCommand() {
                numberOfParameters = 2;
            }
            
            @Override
            @SuppressWarnings("unchecked")
            public void run(Stack s) throws ParseException {
                checkStack(s);

                Object b = s.pop();
                Object a = s.pop();
                
                if(a instanceof String && b instanceof String) {
                    s.push(command((String)a, (String)b));
                } else {
                    throw new ParseException("Invalid parameter type");
                }
            }
            
            protected abstract String command(String a, String b);
        }
        class Cosim extends TwoStringsPostfixMathCommand {
            private void add(ArrayList<String> found, String s) {
                if(found.contains(s)) {
                    //System.err.println("Cosim: Duplicate " + s);
                } else {
                    found.add(s);
                }
            }
            protected String convertExcept(String exceptString) {
                if(exceptString.length() == 0) {return "";}
                char[] except = exceptString.toCharArray();
                int depth = 0;
                int start = 0;
                ArrayList<String> found = new ArrayList<String>();
                for (int i=0; i<except.length; i++) {
                    if(except[i] == '{') {
                        depth ++;
                    } else if(except[i] == '}') {
                        depth --;
                    }
                    
                    if((depth == 0) && (except[i] == ',')) {
                        add(found, exceptString.substring(start, i));
                        start = i+1;
                    }
                }
                add(found, exceptString.substring(start, except.length));
                
                StringBuilder result = new StringBuilder();
                for(String s : found) {
                    result.append('-');
                    result.append(s);
                }
                
                return result.toString();
            }
            @Override
            protected String command(String spec, String except) {
                int pos; // position of last '}' in design part. Will insert directly before.
                if (spec.contains(":")) {
                    pos = spec.indexOf(':') - 1;
                    while (spec.charAt(pos) != '}') {
                        pos--;
                    }
                } else {
                    pos = spec.length() - 1;
                }
                return spec.substring(0, pos) + convertExcept(except) + spec.substring(pos);
            }
        }
        parser.addFunction("cosim", new Cosim());
    }
    

    public static void addParserFunctions(JEP parser) {
        abstract class TwoDoublesPostfixMathCommand extends PostfixMathCommand {
            public TwoDoublesPostfixMathCommand() {
                numberOfParameters = 2;
            }
            
            @Override
            @SuppressWarnings("unchecked")
            public void run(Stack s) throws ParseException {
                checkStack(s);

                Object b = s.pop();
                Object a = s.pop();
                
                if(a instanceof Number && b instanceof Number) {
                    Number an = (Number)a;
                    Number bn = (Number)b;
                    s.push(new Double(command(an.doubleValue(), bn.doubleValue())));
                } else {
                    throw new ParseException("Invalid parameter type");
                }
            }
            
            protected abstract double command(double a, double b);
        }
        
        abstract class DoublePostfixMathCommand extends PostfixMathCommand {
            public DoublePostfixMathCommand() {
                numberOfParameters = 1;
            }
            
            @Override
            @SuppressWarnings("unchecked")
            public void run(Stack s) throws ParseException {
                checkStack(s);
                
                Object a = s.pop();
                
                if(a instanceof Number) {
                    Number an = (Number)a;
                    s.push(new Double(command(an.doubleValue())));
                } else {
                    throw new ParseException("Invalid parameter type");
                }
            }
            
            protected abstract double command(double a);
        }
        class Max extends TwoDoublesPostfixMathCommand {
            @Override
            protected double command(double a, double b) {
                return Math.max(a,b);
            }
        }
        class Min extends TwoDoublesPostfixMathCommand {
            @Override
            protected double command(double a, double b) {
                return Math.min(a,b);
            }
        }

        /** rounds UP - http://internal/eng/depot/sw/cad/doc/specs/cast/castv3modeling.html */
        class LogN extends DoublePostfixMathCommand {
            final double n; 
            public LogN(double n) {
                super();
                this.n = n;
            }
            
            @Override
            protected double command(double a) {
                return Math.ceil(Math.log(a)/Math.log(n));
            }
        }

        parser.addStandardFunctions();
        parser.addFunction("max", new Max());
        parser.addFunction("min", new Min());
        parser.addFunction("log2", new LogN(2));
        parser.addFunction("log4", new LogN(4));
    }
    

}
