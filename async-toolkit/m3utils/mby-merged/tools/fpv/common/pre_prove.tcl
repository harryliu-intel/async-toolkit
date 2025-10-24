# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# pre proof script 
puts "Pre Proof/machine setup"

set_prove_per_property_time_limit 10s
set_prove_per_property_time_limit_factor 2
set_prove_time_limit 10s
set_engine_mode {Hp Ht Bm J Q3 L R B N}
