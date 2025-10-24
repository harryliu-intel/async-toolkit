# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


#
# source from zsh file to set up common environment for rechar
#

mycwd=`pwd`
realcwd=`realpath $mycwd`
realward=`realpath $WARD`

if [[ "$realward" != "$realcwd" ]]; then
	echo "?error : WARD $realward != CWD $realcwd"
	exit 1
fi

TOP=${WARD}

if [[ ! -f ${RECHAR_PVTS_ZSH} ]]; then
	echo "ERROR: RECHAR_PVTS_ZSH = ${RECHAR_PVTS_ZSH} not found!"
	exit 1
fi

source ${RECHAR_PVTS_ZSH}

cell_list=$TOP/$RECHAR_CELL_LIST_FN

if [[ ! -f ${RECHAR_PVTS_ZSH} ]]; then
	echo "ERROR: RECHAR_ENV_ZSH = ${RECHAR_ENV_ZSH} not found!"
	exit 1
fi

source ${RECHAR_ENV_ZSH}
