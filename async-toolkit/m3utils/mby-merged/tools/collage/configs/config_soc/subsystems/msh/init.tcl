# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#################################################################################
# This file contains tcl code and is sourced as part of the -init stage of 
# reading a subsystem
#
# It can include (optionally) installation of IP kits as well as tcl utility 
# procedures that are used by the subsequent stages of this subsystem
# 
#################################################################################

collage_install_ip_kit -ip_name  "mby_msh" \
                       -kit_name "mby_msh" \
                       -src_dir  $src_kits_root \
                       -dest_dir $install_kits_root

