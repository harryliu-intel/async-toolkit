# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# pre elab script 
puts "Initializing the coverage app before initialization"

set_current_gui cov

# Initializing the Coverage App before elaboration
check_cov -init -type all -skip_async_blocks
