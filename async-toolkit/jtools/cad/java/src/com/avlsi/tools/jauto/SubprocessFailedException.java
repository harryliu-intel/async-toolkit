// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.tools.jauto;

public class SubprocessFailedException extends Exception {
    public SubprocessFailedException(String why) {
        super(why);
    }
    public SubprocessFailedException(String why, Exception cause) {
        super(why, cause);
    }
}
