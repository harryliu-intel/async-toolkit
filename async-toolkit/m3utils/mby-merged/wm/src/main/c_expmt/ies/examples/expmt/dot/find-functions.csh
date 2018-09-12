#! /bin/csh -f

/usr/bin/nm --defined-only ../model_server | egrep ' T ' | cut -d" " -f 3 | egrep -v '^_' | sort > functions.txt
