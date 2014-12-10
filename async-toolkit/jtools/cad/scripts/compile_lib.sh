#!/bin/zsh
# $Id$

package_root=`echo "$0" | sed -e 's:/bin/[^/]*$::'`

os_type=`uname -s`
arch_type=`uname -m`
os_arch="$os_type-$arch_type"

sofile="$package_root/$os_arch/lib/rescuedb.so"
if [ "$os_arch" = "Linux-x86_64" ]; then
    bit64=-64
else
    bit64=
fi

if [ ! -s "$sofile" ]; then
   echo "Error: sofile $sofile not found";
   exit 1
fi
export SNPSLMD_QUEUE=true
if [ $# -ne 2 ]; then
	echo "Usage: $0 libfile dbfile"
	exit 1
fi

libfile="$PWD/$1"
if [ ! -r "$libfile" ]; then
	echo "Libfile $libfile is not readable"
	exit 1
fi

dbfile="$2"
export TMPDIR=/tmp
if workdir=`mktemp -t -d compile_lib.XXXXXX`; then
	export OUTPUT_DB="$workdir/1.db"
	script="$workdir/pt.tcl"
	cat <<EOF >"$script"
read_lib {$libfile}
exit
EOF

	(cd "$workdir"; LD_PRELOAD="$sofile" /p/rrc/tools/bin/pts pt_shell -64 -file pt.tcl >pt.log 2>pt.err)
	if [ -e "$OUTPUT_DB" ]; then
		if cp "$OUTPUT_DB" "$dbfile"; then
			rm -rf "$workdir"
			echo "Successfully generated $dbfile"
		else
			echo "Cannot create $libfile; temporary files left in $workdir."
			exit 2
		fi
	else
		echo "Unable to compile $libfile; temporary files left in $workdir."
		exit 2
	fi
else
	echo "Cannot create temporary directory."
	exit 1
fi
