
#
# source from zsh file to set up common environment for rechar
#

mycwd=`pwd`
realcwd=`realpath $mycwd`
realward=`realpath $WARD`

if [[ "$realward" != "$realcwd" ]]; then
	echo "WARD $realward != CWD $realcwd"
	exit 1
fi

TOP=${WARD}

source ${RECHAR_PVTS_ZSH}

cell_list=$TOP/$RECHAR_CELL_LIST_FN

source ${RECHAR_ENV_ZSH}
