#!/bin/sh -x

BD=/nfs/sc/disks/hlp_0015/mnystroe/git/meta-git/bin
WD=/p/hlp/romanpar/RDLTest/

cd ${WD}

${BD}/svpp < lib_udp.rdl | ${BD}/perlfe | perl > /tmp/tmp$$

${BD}/parserdl --print-user-def-properties < /tmp/tmp$$

