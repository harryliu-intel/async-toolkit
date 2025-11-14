// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.fulcrummicro.util.cmdline;

import java.util.List;

public interface ArgProcessingInterface<T> {

    /**
     * called for every arg that matches the associated
     * regular expression, returns some object; the function
     * may choose to move the argptr within the iterator 
     * if it is processing multiple arguments out of the list
     */
    T processArg(String arg, CommandLineIterator<T> cli, List<String> argList);
}
