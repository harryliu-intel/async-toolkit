/**
 * INTEL TOP SECRET
 * Copyright 2014 Intel Corporation
 * All Rights Reserved.
 */
package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.util.List;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;

/**
 * @author mhesseli
 */
public class RegisterChecker extends RegisterInfo {

    public RegisterChecker(String filename) {
        super(filename);
    }

    public static void main(String[] args) {
        CommandLineArg arg;
        CommandLineArgs cmdline;
        CommandLineArgsIterator it;
        RegisterChecker checker;
        String xml = null;

        cmdline = new CachingCommandLineArgs(new CommandLineArgsDefImpl(args));
        it = cmdline.iterator();
        while (it.hasNext()) {
            arg = it.next();
            if (arg.getName().equals("help")) {
                usage(0);
            } else if (arg.getName().equals("xml")) {
                xml = arg.getValue();
            }
        }
        if (xml == null) {
            usage(2);
        }
        checker = new RegisterChecker(xml);
        checker.checkAtomicity();
    }

    private void checkAtomicity() {
        List<Register> regs = this.getSortedRegisterList();
        List<RegisterField> fields;
        int atomicWidth;
        int lbound;
        int ubound;

        for (Register r : regs) {
            atomicWidth = r.getAtomicWidth();
            fields = r.getSortedFields();
            for (RegisterField f : fields) {
                lbound = f.pos / 32;
                ubound = (f.pos + f.len - 1) / 32;
                if ((ubound - lbound) >= atomicWidth) {
                    System.err.printf("warning: %s.%s is not atomic\n",
                                      r.getName(),
                                      f.desc);
                }
            }
        }
    }

    private static void usage(int status) {
        String s;

        s =   "Usage: %s --xml=XML                                           \n"
            + "Options:                                                      \n"
            + "\t--help                                                      \n"
            + "\t\tDisplay a short help message.                             \n"
            + "\t--xml=XML                                                   \n"
            + "\t\tRead the register definitions from XML.                   \n";
        System.out.printf(s, RegisterChecker.class.getSimpleName());
        System.exit(status);
    }

}
