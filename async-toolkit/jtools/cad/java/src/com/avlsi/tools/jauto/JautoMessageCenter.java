/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

public class JautoMessageCenter
{
    static final String             emailSuffix = "@fulcrummicro.com";
    String                          userName;

    List/*<JautoMessage>*/          errorMsgs;
    List/*<JautoMessage>*/          warningMsgs;
    List/*<JautoMessage>*/          otherMsgs;


    public JautoMessageCenter()
    {
        errorMsgs = new ArrayList/*<JautoMessage>*/();
        warningMsgs = new ArrayList/*<JautoMessage>*/();
        otherMsgs = new ArrayList/*<JautoMessage>*/();

        userName = System.getProperty("user.name", "");
    }


    public JautoMessageCenter(JautoMessageCenter mc)
    {
        errorMsgs = new ArrayList/*<JautoMessage>*/(mc.errorMsgs);
        warningMsgs = new ArrayList/*<JautoMessage>*/(mc.warningMsgs);
        otherMsgs = new ArrayList/*<JautoMessage>*/(mc.otherMsgs);

        userName = mc.userName;
    }


    public void output()
    {
        for (Iterator ita = errorMsgs.iterator(); ita.hasNext(); ) {
            JautoMessage jma = (JautoMessage)ita.next();
            jma.output();
        }
        
        for (Iterator ita = warningMsgs.iterator(); ita.hasNext(); ) {
            JautoMessage jma = (JautoMessage)ita.next();
            jma.output();
        }
        
        for (Iterator ita = otherMsgs.iterator(); ita.hasNext(); ) {
            JautoMessage jma = (JautoMessage)ita.next();
            jma.output();
        }
    }

    public void outputShort()
    {
        for (Iterator ita = errorMsgs.iterator(); ita.hasNext(); ) {
            JautoMessage jma = (JautoMessage)ita.next();
            jma.outputShort();
        }
        
        for (Iterator ita = warningMsgs.iterator(); ita.hasNext(); ) {
            JautoMessage jma = (JautoMessage)ita.next();
            jma.outputShort();
        }
        
        for (Iterator ita = otherMsgs.iterator(); ita.hasNext(); ) {
            JautoMessage jma = (JautoMessage)ita.next();
            jma.outputShort();
        }
    }


    public JautoMessage createMessage(int i1, int i2, String s1, String s2, String s3)
    {
        int                         i;

        if((i1 >= 0) && (i1 <= 2)){
            i = i1;
        }
        else{
            i = 2;
        }

        JautoMessage jma = new JautoMessage(i, i2, s1, s2, s3);
        switch(i1){
            case 0:     errorMsgs.add(jma);
                        break;

            case 1:     warningMsgs.add(jma);
                        break;

            case 2:     otherMsgs.add(jma);
                        break;

            default:    throw new AssertionError();
        }

        jma.output();

        return jma;
    }


    public JautoMessage createMessageShort(int i1, int i2, String s1, String s2, String s3)
    {
        int                         i;

        if((i1 >= 0) && (i1 <= 2)){
            i = i1;
        }
        else{
            i = 2;
        }

        JautoMessage jma = new JautoMessage(i, i2, s1, s2, s3);
        switch(i1){
            case 0:     errorMsgs.add(jma);
                        break;

            case 1:     warningMsgs.add(jma);
                        break;

            case 2:     otherMsgs.add(jma);
                        break;

            default:    throw new AssertionError();
        }

        jma.outputShort();

        return jma;
    }


    public JautoMessage createMessageSilent(int i1, int i2, String s1, String s2, String s3)
    {
        int                         i;

        if((i1 >= 0) && (i1 <= 2)){
            i = i1;
        }
        else{
            i = 2;
        }

        JautoMessage jma = new JautoMessage(i, i2, s1, s2, s3);
        switch(i1){
            case 0:     errorMsgs.add(jma);
                        break;

            case 1:     warningMsgs.add(jma);
                        break;

            case 2:     otherMsgs.add(jma);
                        break;

            default:    throw new AssertionError();
        }

        // jma.output();

        return jma;
    }

}
