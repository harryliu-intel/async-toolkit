# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

puts "post cov all prove"


# Prove cover items
check_cov -prove -prove_opts {-verbosity 0 -effort user -engines {Hp Ht B AM N I Tri L Q3} -time_limit 15m}

# Generate coverage report
check_cov -report -type all -exclude { reset waived } -report_file $env(MODEL_ROOT)/results/mby/coverage.html -html -force
