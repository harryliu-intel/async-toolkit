// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.util.Comparator;

/**
 * @author mhesseli
 */
public class RegisterFieldComparator implements Comparator<RegisterField> {

    public int compare(RegisterField f1, RegisterField f2) {
        return Integer.valueOf(f1.pos).compareTo(f2.pos);
    }

}
