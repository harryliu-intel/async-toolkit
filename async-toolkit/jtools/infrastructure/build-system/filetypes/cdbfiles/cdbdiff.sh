# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

dep1=$1
dep2=$2

function getCDBFilesFromDepFile {
    local depFile=$1
    cat "$depFile" | xargs -n 1 echo | grep '^\/'
}

