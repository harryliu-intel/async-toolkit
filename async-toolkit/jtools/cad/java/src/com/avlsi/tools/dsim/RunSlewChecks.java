/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;

public class RunSlewChecks {
    final SlewChecker checker;

    public RunSlewChecks(final DSim dsim) {
        checker = new SlewChecker(dsim);
    }

    public void setSlowSlews(final double slew) {
        checker.setSlowSlews(slew);
    }

    public void setFastSlews(final double slew) {
        checker.setFastSlews(slew);
    }

    public void saveSlowSlews(final ObjectOutputStream oos) {
        checker.saveSlowSlews(oos);
    }

    public void saveFastSlews(final ObjectOutputStream oos) {
        checker.saveFastSlews(oos);
    }

    public void readFastSlews(final ObjectInputStream ois) {
        checker.readFastSlews(ois);
    }

    public void readSlowSlews(final ObjectInputStream ois) {
        checker.readSlowSlews(ois);
    }

    public void computeSlowSlews(final double inputSlew) {
        checker.computeSlowSlews(inputSlew);
    }

    public void computeFastSlews(final double inputSlew) {
        checker.computeFastSlews(inputSlew);
    }

    public void evaluate(final double absMargin,
                         final double multMargin) {

        checker.evaluate(absMargin, multMargin, false);
    }

    public void getReport(final PrintWriter pwOutSlew,
                          final PrintWriter pwIOSlew,
                          final PrintWriter pwStatSlew,
                          final PrintWriter pwAll) {
        getReport(pwOutSlew, pwIOSlew, pwStatSlew, pwAll, false);
    }

    public void getReport(final PrintWriter pwOutSlew,
                          final PrintWriter pwIOSlew,
                          final PrintWriter pwStatSlew,
                          final PrintWriter pwAll,
                          final boolean reportVictimsOnce) {
        if (checker != null) {
            checker.printViolations(pwOutSlew, pwIOSlew, pwStatSlew, pwAll,
                                    reportVictimsOnce);
            pwOutSlew.flush();
            pwIOSlew.flush();
            pwStatSlew.flush();
        }
    }

    public void reportSlowSlews(final PrintWriter pw) {
        if (checker != null) {
            checker.printSlowSlews(pw);
            pw.flush();
        }
    }

    public void reportFastSlews(final PrintWriter pw) {
        if (checker != null) {
            checker.printFastSlews(pw);
            pw.flush();
        }
    }       
}