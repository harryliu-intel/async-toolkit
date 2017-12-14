/**
 * Copyright (C) 2006-2011 Fulcrum Microsystems, Inc.  All rights reserved.
 * Unauthorized disclosure prohibited.
 */

package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.util.Comparator;

/**
 * @author zloh
 * @author mhesseli
 */
public class RegisterComparator implements Comparator<Register> {

    public int compare(Register r1, Register r2) {
        int x;
        boolean a1;
        boolean a2;

        x = r1.getAddressSafe() - r2.getAddressSafe();
        if (x != 0)
            return x;
        a1 = (r1.getAlias() != null);
        a2 = (r2.getAlias() != null);
        if (!a1 && a2) {
            // r1 is "less than" r2 as r1 is not an alias, while r2 is (possibly
            // of r1).
            return -1;
        } else if (a1 && !a2) {
            // r1 is "greater than" r2 as r1 is an alias (possibly of r2), while
            // r2 is not.
            return 1;
        } else {
            // Either r1 and r2 are the same Register instance or both r1 and r2
            // are aliases. Order r1 and r2 using their names.
            return r1.getName().compareTo(r2.getName());
        }
    }

}
