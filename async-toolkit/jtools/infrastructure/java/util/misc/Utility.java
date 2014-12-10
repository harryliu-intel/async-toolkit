package com.fulcrummicro.util.misc;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * a bunch of miscellaneous utility static methods
 * that I don't know where else to put
 *
 * @author Naru Sundar
 */
public class Utility {

    /* converts an array of objects to an array list, similar
     * to Arrays.asList but (1) returns an ArrayList, (2) it is
     * not backed by the original array
     */
    public static ArrayList<Object> toArrayList(Object[] arr) {
        ArrayList<Object> L = new ArrayList<Object>();

        for(int i = 0; i < arr.length; i++)
            L.add(arr[i]);

        return L;
    }

    /* converts an array of strings to an array list, similar
     * to Arrays.asList but (1) returns an ArrayList<String>, (2) it is
     * not backed by the original array
     */
    public static ArrayList<String> toArrayList(String[] arr) {
        ArrayList<String> L = new ArrayList<String>();

        for(int i = 0; i < arr.length; i++)
            L.add(arr[i]);

        return L;
    }

    public static String exceptionString(Throwable e) {
        StringWriter stackWriter = new StringWriter();
        PrintWriter stackPrintWriter = new PrintWriter(stackWriter);

        e.printStackTrace(stackPrintWriter);
        stackPrintWriter.flush();
        return stackWriter.toString();
    }

    public static boolean isUnderscore(String in) {
        return Pattern.matches("\\A[0-9A-Z_]*\\z", in);
    }

    public static String setFirstLetterCase(String in, boolean firstLetterUpperCase) {
        if(firstLetterUpperCase) {
            return in.substring(0, 1).toUpperCase() + in.substring(1);
        } else {
            return in.substring(0, 1).toLowerCase() + in.substring(1);
        }
    }

    public static String underscoreToCamelCase(String in, boolean firstLetterUpperCase) {
        String out = "";
        String s[] = in.split("_");
        for(int i=0; i<s.length; i++) {
            if((s[i].length() == 0) ||                      // allow for foo__bar => foo_Bar
                ((i != 0) && (s[i].matches("\\A[0-9]")))) { // allow for foo_1 => foo_1
                out += "_" + s[i].toLowerCase();
            } else {
                out += s[i].substring(0, 1).toUpperCase() + s[i].substring(1).toLowerCase();
            }
        }

        if(!firstLetterUpperCase) {
            out = out.substring(0, 1).toLowerCase() + out.substring(1);
        }

        return out;
    }

    public static String camelCaseToUnderscore(String in) {
        in = in.substring(0, 1).toLowerCase() + in.substring(1);
        String out = "";

        Matcher m = Pattern.compile("\\p{Upper}").matcher(in);
        int index = 0;
        while(m.find()) {
            out += in.substring(index, m.start()).toUpperCase() + "_";
            index = m.end()-1;
        }
        out += in.substring(index).toUpperCase();

        return out;
    }

    public static String firstLetterUpperCase(String in) {
        return in.substring(0, 1).toUpperCase() + in.substring(1);
    }
    public static String firstLetterLowerCase(String in) {
        return in.substring(0, 1).toLowerCase() + in.substring(1);
    }
}
