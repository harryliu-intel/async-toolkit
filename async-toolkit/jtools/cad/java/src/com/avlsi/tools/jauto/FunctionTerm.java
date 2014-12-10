/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.jauto;

import com.avlsi.util.text.NumberFormatter;


public class FunctionTerm
{
    public static final class Type {

        private Type() { }

        public static final int OBJECTIVE = 1;
        public static final int CONSTRAINT_LESS_THAN = 2;
        public static final int CONSTANT = 3;
        public static final int VAR = 4;
        public static final int ONE_OVER_VAR = 5;
        public static final int VAR_OVER_VAR = 6;
        public static final int CONSTRAINT_EQUALS = 7;
    }

    public int               type;
    /*
        Possible term types values:

        1: OBJECTIVE - only used in the first term of a list to indicate the list as an objective function
               The last term is always zero in an objective function.
               Only valid as the 0th entry in a List<FunctionTerm>.
        2: CONSTRAINT_LESS_THAN - only used in the first term of a list to indicate the list as a constraint
               Only valid as the 0th entry in a List<FunctionTerm>.
               Constraint is:
               List<FunctionTerm> terms;
               (\sum int i; 1 <= i && i < terms.size() - 1;
                            terms.get(i)) <= terms.get(terms.size() - 1)
               Where terms.get(terms.size() - 1).type == CONSTANT
        3: CONSTANT - just a constant number
               Not valid as the 0th entry in a List<FunctionTerm>.
        4: VAR - term in the form of coefficient * variableName_1
               Not valid as the 0th entry in a List<FunctionTerm>.
        5: ONE_OVER_VAR - term in the form of coefficient * (1 / variableName_1)
               Not valid as the 0th entry in a List<FunctionTerm>.
        6: VAR_OVER_VAR - term in the form of coefficient * (variableName_1 / variableName_2)
               Not valid as the 0th entry in a List<FunctionTerm>.
        7: CONSTRAINT_EQUALS - constrains variable_1 to be equal to
               an expression
               Constraint is:
               List<FunctionTerm> terms;
               (\sum int i; 1 <= i && i < terms.size(); terms.get(i))
                   == terms.get(0).variable_1
    */

    public String            variableName_1;
    public String            variableName_2;

    public double            coefficient;


    public int setType(int t)
    {
        assert t >= Type.OBJECTIVE && t <= Type.CONSTRAINT_EQUALS
            : "invalid type of FunctionTerm";

        type = t;

        return type;
    }


    public int getType()
    {
        return type;
    }


    public String setVariableName_1(String s)
    {
        assert s.length() <= 10
            : "variable name must NOT have more than 10 characters";

        variableName_1 = s;

        return variableName_1;
    }


    public String getVariableName_1()
    {
        return variableName_1;
    }


    public String setVariableName_2(String s)
    {
        assert s.length() <= 10
            : "variable name must NOT have more than 10 characters";

        variableName_2 = s;

        return variableName_2;
    }


    public String getVariableName_2()
    {
        return variableName_2;
    }


    public double setCoefficient(double d)
    {
        coefficient = d;

        return coefficient;
    }


    public double getCoefficient()
    {
        return coefficient;
    }


    public FunctionTerm(final int type,
                        final String variableName_1,
                        final String variableName_2,
                        final double coefficient) {
        this.type = type;
        this.variableName_1 = variableName_1;
        this.variableName_2 = variableName_2;
        this.coefficient = coefficient;
    }


    // constructor 1
    public FunctionTerm()
    {
        this(Type.CONSTANT, "", "", 0.0);
    }

    // constructor 2
    public FunctionTerm(FunctionTerm ft)
    {
        this(ft.getType(), ft.getVariableName_1(), ft.getVariableName_2(),
             ft.getCoefficient());
    }

    public FunctionTerm(final double coefficient) {
        this(Type.CONSTANT, "", "", coefficient);
    }

    public FunctionTerm(final int type,
                        final String variableName_1,
                        final double coefficient) {
        this(type, variableName_1, "", coefficient);
    }

    // dump out infomation for debug
    public void print()
    {
        System.out.println("type: " + type);
        System.out.println("var1: " + variableName_1);
        System.out.println("var2: " + variableName_2);
        System.out.println("coef: " + coefficient);
    }


    /**
     * Human readable string.
     **/
    public String toString()
    {
        /*
        String s = "type: " + type + "\n";
        s += "var1: " + variableName_1 + "\n";
        s += "var2: " + variableName_2 + "\n";
        s += "coef: " + coefficient + "\n";
        */

        switch(type){
            case Type.CONSTANT:
                        return NumberFormatter.format(coefficient, 6);

            case Type.VAR:
                        return NumberFormatter.format(coefficient, 6) + " * "
                            + variableName_1;

            case Type.ONE_OVER_VAR:
                        return NumberFormatter.format(coefficient, 6) + " / "
                            + variableName_1;

            case Type.VAR_OVER_VAR:
                        return NumberFormatter.format(coefficient, 6) + " * "
                            + variableName_1 + " / "
                            + variableName_2;

            default:    throw new AssertionError
                            ("Unknown FunctionTerm type: " + type);
        }
    }


    /**
     * Function term in postfix notation for CG solver.
     **/
    public String toStringPostfix(String variablePrefix)
    {
        switch(type){
            case Type.CONSTANT:
                        return NumberFormatter.format(coefficient, 6);

            case Type.VAR:
                        if (coefficient == 0.0)
                            return "0";
                        else if (coefficient == 1.0)
                            return variablePrefix + variableName_1;
                        else
                            return NumberFormatter.format(coefficient, 6) + " "
                                + variablePrefix + variableName_1 + " *";

            case Type.ONE_OVER_VAR:
                        if (coefficient == 0.0)
                            return "0";
                        else
                            return NumberFormatter.format(coefficient, 6) + " "
                                + variablePrefix + variableName_1 + " /";

            case Type.VAR_OVER_VAR:
                        if (coefficient == 0.0)
                            return "0";
                        else if (coefficient == 1.0)
                            return variablePrefix + variableName_1 + " "
                                + variablePrefix + variableName_2 + " /";
                        else
                            return NumberFormatter.format(coefficient, 6) + " "
                                + variablePrefix + variableName_1 + " "
                                + variablePrefix + variableName_2 + " / *";

            default:    throw new AssertionError();
        }
    }

    String toStringPostfix() {
        return toStringPostfix("");
    }

    /**
     * Function term in notation for LP solver.  Omits the * for
     * multiplication.
     **/
    public String toStringLP() {
        switch(type){
        case Type.CONSTANT:
            return NumberFormatter.format(coefficient, 6);
            
        case Type.VAR:
            if (coefficient == 0.0)
                return "0";
            else if (coefficient == 1.0)
                return  variableName_1;
            else
                return NumberFormatter.format(coefficient, 6) + " "
                    +  variableName_1;
            
        case Type.ONE_OVER_VAR:
            if (coefficient == 0.0)
                return "0";
            else
                return NumberFormatter.format(coefficient, 6) + " / "
                    +  variableName_1;
            
        case Type.VAR_OVER_VAR:
            if (coefficient == 0.0)
                return "0";
            else if (coefficient == 1.0)
                return  variableName_1 + " / "
                    +  variableName_2;
            else
                return NumberFormatter.format(coefficient, 6) + " "
                    +  variableName_1 + " / "
                    +  variableName_2;
            
        default:    throw new AssertionError();
        }
    }


    /**
     * Derivative of function term with respect to <code>var1</code>
     * in postfix notation for CG solver.
     **/
    public String getDerivativeString(String var1)
    {
        switch(type){
            case Type.CONSTANT:
                        return "0";

            case Type.VAR:
                        if(var1.equals(variableName_1)){
                            return NumberFormatter.format(coefficient, 6);
                        }
                        else{
                            return "0";
                        }

            case Type.ONE_OVER_VAR:
                        if(var1.equals(variableName_1)){
                            return NumberFormatter.format(-coefficient, 6) + " "
                                + variableName_1 + " "
                                + variableName_1 + " * /";
                        }
                        else{
                            return "0";
                        }

            case Type.VAR_OVER_VAR:
                        if(var1.equals(variableName_1)){
                            return NumberFormatter.format(coefficient, 6) + " "
                                + variableName_2 + " /";
                        }
                        else{
                            if(var1.equals(variableName_2)){
                                return NumberFormatter.format(-coefficient, 6) + " "
                                    + variableName_1 + " " 
                                    + variableName_2 + " "
                                    + variableName_2 + " * / *";
                            }
                            else{
                                return "0";
                            }
                        }

            default:    throw new AssertionError();
        }
    }

    private static FunctionTerm multiply(final FunctionTerm ftm1,
                                         final FunctionTerm ftm2) {
        final int t1 = ftm1.getType();
        final int t2 = ftm2.getType();
        final FunctionTerm result;
        if (t1 == Type.CONSTANT) {
            result = new FunctionTerm(ftm2);
        } else if (t2 == Type.CONSTANT) {
            result = new FunctionTerm(ftm1);
        } else if (t1 == Type.VAR && t2 == Type.ONE_OVER_VAR) {
            result = new FunctionTerm(ftm1);
            result.setType(Type.VAR_OVER_VAR);
            result.setVariableName_2(ftm2.getVariableName_1());
        } else if (t2 == Type.VAR && t1 == Type.ONE_OVER_VAR) {
            result = new FunctionTerm(ftm2);
            result.setType(Type.VAR_OVER_VAR);
            result.setVariableName_2(ftm1.getVariableName_1());
        } else {
            throw new RuntimeException("Can't multiply " + ftm1 +
                                       " and " + ftm2);
        }

        result.coefficient = ftm1.coefficient * ftm2.coefficient;

        return result;
    }

    public FunctionTerm multiply(final FunctionTerm other) {
        return multiply(this, other);
    }
}
