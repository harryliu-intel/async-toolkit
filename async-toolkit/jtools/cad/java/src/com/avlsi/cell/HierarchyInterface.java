// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.cell;

import com.avlsi.file.common.HierName;

public interface HierarchyInterface {
    HierarchyInterface getSubcell(HierName name);
}
