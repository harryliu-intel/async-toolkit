// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.util.container;

import java.util.Iterator;

public class IterableIterator<T> implements Iterable<T> {
    private final Iterator<T> iterator;
    public IterableIterator(final Iterator<T> iterator) {
        this.iterator = iterator;
    }
    public Iterator<T> iterator() {
        return iterator;
    }
}
