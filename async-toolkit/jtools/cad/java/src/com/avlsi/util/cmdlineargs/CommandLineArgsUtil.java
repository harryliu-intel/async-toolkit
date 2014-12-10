/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs;


/**
 * Convenience methods for operating on {@link CommandLineArgs}.
 *
 * @author Jesse Rosenstock
 **/
public final class CommandLineArgsUtil {

    /**
     * Class constructor.  This class consists entirely of static methods
     * and should never be instantiated.
     **/
    //@ requires false;
    private CommandLineArgsUtil() {
    }

    /**
     * Returns the double value of the argument with the specified name
     * if one was in the list of command line arguments or the default
     * value if the argument was not specified.  If the argument's value
     * is not a valid format for a double as specified by
     * {@link Double#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * @param defValue The return value of this method if no argument with the
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list, or
     * the specified default value if it was not.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for a double as
     *         specified by {@link Double#valueOf(String)}.
     **/
    public static Double getDoubleArgValue(
            /*@ non_null @*/ CommandLineArgs args,
            /*@ non_null @*/ String argName,
            Double defValue)
        throws CommandLineArgFormatException {
        final String value = args.getArgValue(argName, null);

        if (value == null)
            return defValue;

        try {
            return Double.valueOf(value);
        } catch (NumberFormatException e) {
            throw new CommandLineArgFormatException(argName, value, e);
        }
    }

    /**
     * Returns the float value of the argument with the specified name
     * if one was in the list of command line arguments or the default
     * value if the argument was not specified.  If the argument's value
     * is not a valid format for a float as specified by
     * {@link Float#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * @param defValue The return value of this method if no argument with the
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list, or
     * the specified default value if it was not.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for a float as
     *         specified by {@link Float#valueOf(String)}.
     **/
    public static Float getFloatArgValue(
            /*@ non_null @*/ CommandLineArgs args,
            /*@ non_null @*/ String argName,
            Float defValue)
        throws CommandLineArgFormatException {
        final String value = args.getArgValue(argName, null);

        if (value == null)
            return defValue;

        try {
            return Float.valueOf(value);
        } catch (NumberFormatException e) {
            throw new CommandLineArgFormatException(argName, value, e);
        }
    }

    /**
     * Parses a comma separated list of Double values.  Returns null
     * if none.
     **/
    public static double [] getDoubleArgList(/*@ non_null @*/ CommandLineArgs args,
                                             /*@ non_null @*/ String argName) 
        throws CommandLineArgFormatException {
        final String value = args.getArgValue(argName, null);
        
        if (value == null)
            return new double[0];

        try {
            String [] strValues = value.split(",");
            double [] doubleValues = new double[strValues.length];
            for (int i=0; i<strValues.length; i++) {
                doubleValues[i] = Double.parseDouble(strValues[i]);
            }
            return doubleValues;
        } catch (NumberFormatException e) {
            throw new CommandLineArgFormatException(argName, value, e);
        }
    }

    /**
     * Parses a comma separated list of Int values.  Returns null
     * if none.
     **/
    public static int [] getIntArgList(/*@ non_null @*/ CommandLineArgs args,
                                       /*@ non_null @*/ String argName) 
        throws CommandLineArgFormatException {
        final String value = args.getArgValue(argName, null);
        
        if (value == null)
            return new int [0];

        try {
            String [] strValues = value.split(",");
            int [] intValues = new int[strValues.length];
            for (int i=0; i<strValues.length; i++) {
                intValues[i] = Integer.parseInt(strValues[i]);
            }
            return intValues;
        } catch (NumberFormatException e) {
            throw new CommandLineArgFormatException(argName, value, e);
        }
    }

    /**
     * Returns the integer value of the argument with the specified name
     * if one was in the list of command line arguments or the default
     * value if the argument was not specified.  If the argument's value
     * is not a valid format for an integer as specified by
     * {@link Integer#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * @param defValue The return value of this method if no argument with the
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list, or
     * the specified default value if it was not.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for an integer as
     *         specified by {@link Integer#valueOf(String)}.
     **/
    public static Integer getIntegerArgValue(
            /*@ non_null @*/ CommandLineArgs args,
            /*@ non_null @*/ String argName,
            Integer defValue)
        throws CommandLineArgFormatException {
        final String value = args.getArgValue(argName, null);

        if (value == null)
            return defValue;

        try {
            return Integer.valueOf(value);
        } catch (NumberFormatException e) {
            throw new CommandLineArgFormatException(argName, value, e);
        }
    }

    /**
     * Returns the value of the argument with the specified name if one was in
     * the list of command line arguments.  If one is not present, throws a
     * <code>MissingCommandLineArgException</code>.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list.
     *
     * @throws MissingCommandLineArgException
     *         If no value is present for the named argument.
     **/
    public static /*@ non_null @*/ String getRequiredArgValue(
            /*@ non_null @*/ CommandLineArgs args,
            /*@ non_null @*/ String argName)
        throws MissingCommandLineArgException {
        final String value = args.getArgValue(argName, null);

        if (value == null)
            throw new MissingCommandLineArgException(argName);
        else
            return value;
    }

    /**
     * Returns the double value of the argument with the specified name
     * if one was in the list of command line arguments.  If one is not
     * present, throws a <code>MissingCommandLineArgException</code>.  
     * If one is present, but not a valid format for a double as specified
     * by {@link Double#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for a double as
     *         specified by {@link Double#valueOf(String)}.
     * @throws MissingCommandLineArgException
     *         If no value is present for the named argument.
     **/
    public static double getRequiredDoubleArgValue(
            /*@ non_null @*/ CommandLineArgs args,
            /*@ non_null @*/ String argName)
        throws CommandLineArgFormatException,
               MissingCommandLineArgException {
        final Double value = getDoubleArgValue(args, argName, null);

        if (value == null)
            throw new MissingCommandLineArgException(argName);
        else
            return value.doubleValue();
    }

    /**
     * Returns the integer value of the argument with the specified name
     * if one was in the list of command line arguments.  If one is not
     * present, throws a <code>MissingCommandLineArgException</code>.  
     * If one is present, but not a valid format for an integer as specified
     * by {@link Integer#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for an integer as
     *         specified by {@link Integer#valueOf(String)}.
     * @throws MissingCommandLineArgException
     *         If no value is present for the named argument.
     **/
    public static int getRequiredIntegerArgValue(
            /*@ non_null @*/ CommandLineArgs args,
            /*@ non_null @*/ String argName)
        throws CommandLineArgFormatException,
               MissingCommandLineArgException {
        final Integer value = getIntegerArgValue(args, argName, null);

        if (value == null)
            throw new MissingCommandLineArgException(argName);
        else
            return value.intValue();
    }

    /**
     * Returns the double value of the argument with the specified name
     * if one was in the list of command line arguments or the default
     * value if the argument was not specified.  If the argument's value
     * is not a valid format for a double as specified by
     * {@link Double#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     * If the argument's value is not non-negative, an
     * <code>InvalidCommandLineArgException</code> wil be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * @param defValue The return value of this method if no argument with the
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list, or
     * the specified default value if it was not.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for a double as
     *         specified by {@link Double#valueOf(String)}.
     * @throws InvalidCommandLineArgException
     *         If the argument value is a floating point value, but
     *         not non-negative.
     **/
    //@ requires defaultValue >= 0.0;
    //@ ensures \result >= 0.0;
    public static double getNonNegativeDoubleArgValue(
            final /*@ non_null @*/ CommandLineArgs args,
            final /*@ non_null @*/ String argName,
            final double defaultValue)
        throws CommandLineArgFormatException,
               InvalidCommandLineArgException {
        final double value =
            getDoubleArgValue(args, argName, new Double(defaultValue))
                .doubleValue();

        if (value >= 0.0)
            return value;
        else
            throw new InvalidCommandLineArgException(argName +
                    " must be non-negative, has value " + value,
                    argName, args.getArgValue(argName, null));
    }

    /**
     * Returns the integer value of the argument with the specified name
     * if one was in the list of command line arguments or the default
     * value if the argument was not specified.  If the argument's value
     * is not a valid format for an integer as specified by
     * {@link Integer#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     * If the argument's value is not non-negative, an
     * <code>InvalidCommandLineArgException</code> wil be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * @param defValue The return value of this method if no argument with the
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list, or
     * the specified default value if it was not.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for an integer as
     *         specified by {@link Integer#valueOf(String)}.
     * @throws InvalidCommandLineArgException
     *         If the argument value is an integer value, but
     *         not non-negative.
     **/
    //@ requires defaultValue >= 0;
    //@ ensures \result >= 0;
    public static int getNonNegativeIntegerArgValue(
            final /*@ non_null @*/ CommandLineArgs args,
            final /*@ non_null @*/ String argName,
            final int defaultValue)
        throws CommandLineArgFormatException,
               InvalidCommandLineArgException {
        final int value =
            getIntegerArgValue(args, argName, new Integer(defaultValue))
                .intValue();

        if (value >= 0)
            return value;
        else
            throw new InvalidCommandLineArgException(argName +
                    " must be non-negative, has value " + value,
                    argName, args.getArgValue(argName, null));
    }

    /**
     * Returns the double value of the argument with the specified name
     * if one was in the list of command line arguments.  If one is not
     * present, throws a <code>MissingCommandLineArgException</code>.  
     * If one is present, but not a valid format for a double as specified
     * by {@link Double#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     * If the argument's value is not non-negative, an
     * <code>InvalidCommandLineArgException</code> wil be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for a double as
     *         specified by {@link Double#valueOf(String)}.
     * @throws InvalidCommandLineArgException
     *         If the argument value is a floating point value, but
     *         not non-negative.
     * @throws MissingCommandLineArgException
     *         If no value is present for the named argument.
     **/
    //@ ensures \result >= 0.0;
    public static double getRequiredNonNegativeDoubleArgValue(
            final /*@ non_null @*/ CommandLineArgs args,
            final /*@ non_null @*/ String argName)
        throws CommandLineArgFormatException,
               InvalidCommandLineArgException,
               MissingCommandLineArgException {
        final double value =
            CommandLineArgsUtil.getRequiredDoubleArgValue(args, argName);

        if (value >= 0.0)
            return value;
        else
            throw new InvalidCommandLineArgException(argName +
                    " must be non-negative, has value " + value,
                    argName, args.getArgValue(argName, null));
    }

    /**
     * Returns the integer value of the argument with the specified name
     * if one was in the list of command line arguments.  If one is not
     * present, throws a <code>MissingCommandLineArgException</code>.  
     * If one is present, but not a valid format for an integer as specified
     * by {@link Integer#value(String)}, a
     * <code>CommandLineArgFormatException</code> will be thrown.
     * If the argument's value is not non-negative, an
     * <code>InvalidCommandLineArgException</code> wil be thrown.
     *
     * @param args
     *        The arguments to query.
     * @param argName The name of the argument to look for.
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list.
     *
     * @throws CommandLineArgFormatException
     *         If the argument value is not a valid format for an integer as
     *         specified by {@link Integer#valueOf(String)}.
     * @throws InvalidCommandLineArgException
     *         If the argument value is an integer value, but
     *         not non-negative.
     * @throws MissingCommandLineArgException
     *         If no value is present for the named argument.
     **/
    //@ ensures \result >= 0;
    public static int getRequiredNonNegativeIntegerArgValue(
            final /*@ non_null @*/ CommandLineArgs args,
            final /*@ non_null @*/ String argName)
        throws CommandLineArgFormatException,
               InvalidCommandLineArgException,
               MissingCommandLineArgException {
        final int value =
            CommandLineArgsUtil.getRequiredIntegerArgValue(args, argName);

        if (value >= 0)
            return value;
        else
            throw new InvalidCommandLineArgException(argName +
                    " must be non-negative, has value " + value,
                    argName, args.getArgValue(argName, null));
    }
}
