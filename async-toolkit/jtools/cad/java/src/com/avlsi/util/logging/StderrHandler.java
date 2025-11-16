// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.util.logging;

public class StderrHandler extends ConsoleHandler {
    public StderrHandler() {
        super(System.err);
    }
}
