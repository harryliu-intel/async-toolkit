// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.tools.cosim.spec;

public interface VisitorInterface {
    void visitCoSimLevelSpec(CoSimLevelSpec coSimLevelSpec);
    void visitCoSimSpecList(CoSimSpecList coSimSpecList);
    void visitCoSimSpec(CoSimSpec coSimSpec);
    void visitInstSpecList(InstSpecList instSpecList);
    void visitInstSpec(InstSpec instSpec);
    void visitModeListLevelSpec(ModeListLevelSpec modeListLevelSpec);
    void visitModeList(ModeList modeList);
    void visitMode(Mode mode);
}
