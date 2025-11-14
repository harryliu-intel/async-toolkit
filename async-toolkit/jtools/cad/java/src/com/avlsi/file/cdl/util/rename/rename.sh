# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
python=`which python`
[[ ( $? == 0 ) && ( -d $package_root ) ]] && PYTHONPATH="$package_root/share/script/python" $python -O "$package_root/share/script/python/com/avlsi/file/cdl/util/rename/rename.pyo" $@
