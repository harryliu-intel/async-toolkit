# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

package_root=$1

mkdir "$package_root/logs"
chmod o+rwx "$package_root/logs"
