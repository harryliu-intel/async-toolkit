# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

from com.avlsi.file.cdl.util.rename.CDLNameInterface import *

class IdentityNames( CDLNameInterface ):
    def renameCell(self, name):
        return name

    def renameNode(self, name):
        return name

    def renameDevice(self, name):
        return name

    def renameSubCellInstance(self, name):
        return name

    def renameTransistorModel(self, name):
        return name
