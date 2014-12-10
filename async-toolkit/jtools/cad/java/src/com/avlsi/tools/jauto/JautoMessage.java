/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;


public class JautoMessage
{
    int                         type;    // type of the message
                                         // 0: Error
                                         // 1: Warning
                                         // 2: Note
                                         // 3: Time stamp
                                         // 4: Other

    int                         number;

    String                      prefix;

    String                      description;

    String                      detail;


    public JautoMessage()
    {
        type = -1;
        number = -1;
        prefix = "";
        description = "";
        detail = "";
    }


    public JautoMessage(JautoMessage msg)
    {
        type = msg.type;
        number = msg.number;
        prefix = msg.prefix;
        description = msg.description;
        detail = msg.detail;
    }


    public JautoMessage(int i1, int i2, String s1, String s2, String s3)
    {
        assert i2 < 100 : "Programmer error, change it to an assertion";

        type = i1;

        if(i2 > 0){
            number = i2;
        }

        prefix = s1;
        description = s2;
        detail = s3;
    }


    public void setMessage(int i1, int i2, String s1, String s2, String s3)
    {
        assert i2 < 100 : "Programmer error, change it to an assertion";

        type = i1;

        if(i2 > 0){
            number = i2;
        }

        prefix = s1;
        description = s2;
        detail = s3;
    }


    public void output()
    {
        assert number < 100;
        if(type > 0){ // not error
            System.out.print("\033[1;33m" + prefix + " " + number + ": " + "\033[0m");
            System.err.print("\033[1;33m" + prefix + " " + number + ": " + "\033[0m");
        }
        else{
            System.out.print("\033[1;31m" + prefix + " " + number + ": " + "\033[0m");
            System.err.print("\033[1;31m" + prefix + " " + number + ": " + "\033[0m");
        }

        System.out.print(description);
        System.out.print(detail);

        System.err.print(description);
        System.err.print(detail);
    }


    public void outputShort()
    {
        assert number < 100;
        if(type > 0){ // not error
            System.out.print("\033[1;33m" + prefix + " " + number + ": " + "\033[0m");
            System.err.print("\033[1;33m" + prefix + " " + number + ": " + "\033[0m");
        }
        else{
            System.out.print("\033[1;31m" + prefix + " " + number + ": " + "\033[0m");
            System.err.print("\033[1;31m" + prefix + " " + number + ": " + "\033[0m");
        }

        System.out.print(description);

        System.err.print(description);
    }


    public void print()
    {
        assert number < 100;
        System.out.print(prefix + " " + number + ": " + description);
        System.out.print(detail);
    }


}
