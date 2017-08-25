#!/bin/sh -x
p4 files ... | grep -v " delete " | sed 's,^//hlp/main/hw/tools/meta\([^ ]*\)#.*$,.\1,' > intel-p4.filelist

find . -type f > intel-find.filelist
