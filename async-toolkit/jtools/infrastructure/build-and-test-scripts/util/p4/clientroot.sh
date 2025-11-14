#!/bin/bash
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$


function p4_get_client_root() {
  local p4_cmd=$1
  local client_spec_name=$2
  
  local p4_client_root=`$p4_cmd client -o $client_spec_name | \
                           $grepcmd -v "#.*$"               | \
                           $grepcmd -e "Root:"              | \
                           $sedcmd -e "s/Root:[[:space:]]*//"`

  ret="$p4_client_root"
}
