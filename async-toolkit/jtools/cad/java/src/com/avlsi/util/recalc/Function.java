/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.recalc;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;

/**
 * This class defines what function expressions are
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class Function implements ExpressionInterface {
    /** Object upon which to invoke function, or null for static function. **/
    private final Object classObject;

    /** List of expressions for function arguments. **/
//GGG Why does this have to be protected and not private?
    protected final ExpressionInterface[] expList;

    /** Function name. **/
    private final String name;

    /** Method object associated with function. **/
    private final java.lang.reflect.Method method;

    /**
     * Construct static function.
     * @param className containing static function to call
     * @param methodName of function to call
     * @param expList list of expressions to evaluate for actual arguments
     *                to function call
     **/
    public Function(String className, String methodName,
                    ExpressionInterface[] expList)
        throws NoSuchMethodException, ClassNotFoundException
    {
        if (className.equals(""))
            this.name = methodName;
        else
            this.name = className + "." + methodName;

        Class c;
        try {
            c = Class.forName(className);
        } catch (ClassNotFoundException e1) {
            try {
                if (className.equals(""))
                    c = Class.forName("java.lang");
                else
                    c = Class.forName("java.lang." + className);
            } catch (ClassNotFoundException e2) {
                // Don't tell the user we tried a second class name for them.
                throw e1;
            }
        }

        this.expList = expList;
        this.classObject = null;
        this.method = lookupMethod(c, methodName);

        if (!isStatic())
            throw new NoSuchMethodException(name + " is not static");
    }

    /**
     * Construct Function
     * @param classObject Class object on which to invoke method
     * @param methodName of function to call
     * @param expList list of expressions to evaluate for actual arguments
     *                to function call
     **/
    public Function(Object classObject, String methodName,
                    ExpressionInterface[] expList)
        throws NoSuchMethodException
    {
        Class c = classObject.getClass();

        this.name = c.getName() + "." + methodName;

        this.expList = expList;
        this.classObject = classObject;
        this.method = lookupMethod(c, methodName);
    }

    /**
     * Lookup a Method object for a given class and method name.
     * @param c Class object representing class containing method.
     * @param name of method to lookup
     * @return Method object suitable for invoke().
     **/
    private Method lookupMethod(Class c, String name)
        throws NoSuchMethodException
    {
        Class[] syntax;
        if (expList == null) {
            syntax = null;
        } else {
            // Since we currently only work with doubles, our syntax should
            // only access double values
            syntax = new Class[expList.length];
            for (int i = 0; i < syntax.length; i++)
                syntax[i] = double.class;
        }

        Method method = c.getMethod(name, syntax);

        if (!method.getReturnType().equals(double.class))
            throw new NoSuchMethodException(name + " does not return double");

        if (!java.lang.reflect.Modifier.isPublic(method.getModifiers()))
            throw new NoSuchMethodException(name + " is not public");

        return method;
    }

    /**
     * Evaluate the expression
     * @return result of expression
     **/
    public double eval() {
        Double[] args;
        if (expList == null) {
            args = null;
        } else {
            args = new Double[expList.length];
            for (int i = 0; i < expList.length; i++)
                args[i] = new Double(expList[i].eval());
        }

        try {
            return ((Double) method.invoke(classObject, args)).doubleValue();
        } catch (IllegalAccessException e) {
            // This shouldn't happen because we confirmed that the
            // method is public
            System.out.println("How on earth did we get this exception: "+e);
            return Double.NaN;
        } catch (InvocationTargetException e) {
            // In case the target function does something bad
            System.out.println("Recalc Method exception: "+e);
            return Double.NaN;
        }
    }

    /**
     * Get list of varaibles this expression depends on
     * @return list of references to variables used in this expression
     **/
    public HashSet getDependant() {
        HashSet dependant = new HashSet();
        for (int i = 0; i < expList.length; i++)
            dependant.addAll(expList[i].getDependant());

        return dependant;
    }

    /**
     * Determine if this function invokes upon a real object or not
     * @return true if the underlying method object is static.
     **/
    protected boolean isStatic() {
        return java.lang.reflect.Modifier.isStatic(method.getModifiers());
    }

    /**
     * Object to String
     * @return String describing expression
     **/
    public String toString() {
        String desc = name + "(";

        for (int i = 0; i < expList.length; i++) {
            desc += expList[i].toString();
            if (i < expList.length-1);
                desc += ", ";
        }

        return desc + ")";
    }
}
