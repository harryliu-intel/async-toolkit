/**
 * INTEL TOP SECRET
 * Copyright 2009 - 2013 Intel Corporation
 * All Rights Reserved.
 */

package com.fulcrummicro.util.misc;

import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.nfunk.jep.JEP;
import org.nfunk.jep.ParseException;
import org.nfunk.jep.function.PostfixMathCommand;

public class JepMathExpressionParser extends JEP {

    /**************************************************************************
     * Types
     **************************************************************************/

    private static abstract class JepFunction<A, R> extends PostfixMathCommand {

        private final Class<?> klass;

        public JepFunction(int numberOfParameters) {
            this.klass = getParameterClass(this.getClass());
            this.numberOfParameters = numberOfParameters;
        }

        @Override
        @SuppressWarnings({ "unchecked", "rawtypes" })
        public void run(Stack stack) throws ParseException {
            List<A> args = new ArrayList<A>();
            Object arg;

            this.checkStack(stack);
            for (int i = 0; i < this.getNumberOfParameters(); i++) {
                arg = stack.pop();
                if (this.klass.isInstance(arg)) {
                    args.add(0, (A) arg);
                } else {
                    throw new ParseException("argument " + i + ": Invalid parameter type");
                }
            }
            stack.push(this.command(args));
        }

        protected abstract R command(List<A> args);

        private static Class<?> getParameterClass(Class<?> klass) {
            ParameterizedType type;

            type = (ParameterizedType) klass.getGenericSuperclass();
            return (Class<?>) type.getActualTypeArguments()[0];
        }

    }

    private static class Cosim extends JepFunction<String, String> {

        public Cosim() {
            super(2);
        }

        protected String command(List<String> args) {
            // Strip all embedded whitespace.
            String spec = args.get(0).replaceAll("\\s+", new String());
            String except = args.get(1).replaceAll("\\s+", new String());
            // position of last '}' in design part. Will insert directly before.
            int pos;

            if (spec.contains(":")) {
                pos = spec.indexOf(':') - 1;
                while (spec.charAt(pos) != '}') {
                    pos--;
                }
            } else {
                pos = spec.length() - 1;
            }
            String c = spec.substring(0, pos) + convertExcept(except) + spec.substring(pos);
            return c;
        }

        private void add(ArrayList<String> found, String s) {
            if (!(s.length() == 0 || found.contains(s))) {
                found.add(s);
            }
        }

        private String convertExcept(String exceptString) {
            if (exceptString.length() == 0) {
                return "";
            }
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

                if ((depth == 0) && (except[i] == ',')) {
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

    }

    /**
     * Returns the minimum number of N-bit channels needed to represents a
     * value.
     *
     * @see http://internal/eng/depot/sw/cad/doc/specs/cast/castv3modeling.html
     *
     */
    private static class LogN extends JepFunction<Number, Double> {

        private final double n;

        public LogN(double n) {
            super(1);
            this.n = n;
        }

        protected Double command(List<Number> args) {
            double a = args.get(0).doubleValue();
            return Math.ceil(Math.log(a) / Math.log(this.n));
        }
    }

    private static class Max extends JepFunction<Number, Double> {

        public Max() {
            super(2);
        }

        protected Double command(List<Number> args) {
            double a = args.get(0).doubleValue();
            double b = args.get(1).doubleValue();
            return Math.max(a, b);
        }

    }

    private static class Min extends JepFunction<Number, Double> {

        public Min() {
            super(2);
        }

        protected Double command(List<Number> args) {
            double a = args.get(0).doubleValue();
            double b = args.get(1).doubleValue();
            return Math.min(a, b);
        }

    }

    private static class Lowercase extends JepFunction<String, String> {

        public Lowercase() {
            super(1);
        }

        protected String command(List<String> args) {
            return args.get(0).toLowerCase();
        }

    }

    private static class Uppercase extends JepFunction<String, String> {

        public Uppercase() {
            super(1);
        }

        protected String command(List<String> args) {
            return args.get(0).toUpperCase();
        }

    }

    /**************************************************************************
     * Public Methods
     **************************************************************************/

    public JepMathExpressionParser() {
        super();

        this.addStandardFunctions();
        this.addFunction("cosim", new Cosim());
        this.addFunction("lc", new Lowercase());
        this.addFunction("log2", new LogN(2));
        this.addFunction("log4", new LogN(4));
        this.addFunction("max", new Max());
        this.addFunction("min", new Min());
        this.addFunction("uc", new Uppercase());
    }

}
