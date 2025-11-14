// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.*;

public class LibertyUtil {
    static void checkErr(final String func, final int[] err) {
        if (err[0] != SI2DR_NO_ERROR) {
            throw new LibertyParserException(err[0], func);
        }
    }
}
