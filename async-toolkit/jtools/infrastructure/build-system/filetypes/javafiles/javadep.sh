#!/bin/bash

function exit_func() {
  if [ -n "$straceOutput" ] ; then
    rm -f "$straceOutput"
  fi
}

javacCommand="$1"
tempJavaFile="$2"
jarFileName="$3"
depFileName="$4"

# do not generate any dependency information for now, because (1) we don't
# currently use it, (2) dependency derived via strace shows all files that were
# used to compile a Java program, instead of information on files required when
# compiling an individual file, so compilation time cannot be reduced using the
# dependency information, and (3) it's not obvious how to get the exit status
# when stracing javac (no exit, _exit, or exit_group calls).
$javacCommand

exit $?

trap exit_func EXIT

straceOutput=`mktemp /tmp/javadep.XXXXXX`

currDir=`/bin/pwd`

if [ "$(uname -s)" == "SunOS" ]; then
    strace_cmd="truss -o $straceOutput"
    exitCall="_exit"
else
    strace_cmd="strace -o $straceOutput"
    exitCall="exit_group"
fi

$strace_cmd $javacCommand

# sed is wrong if this is not done on Solaris
export PATH=/usr/intel/bin:$PATH

exitStr=`egrep "$exitCall\([0-9]+\)" $straceOutput | sed -e "s/$exitCall(\([0-9]\+\)).*/\1/"`
if [ "$exitStr" = "" ]; then
    exitStr=1
fi

javaRet="$exitStr"


if [[ "$javaRet" == "0" ]] ; then
  echo "$jarFileName $depFileName : \\" >$depFileName

  grep -e "^open([[:space:]]*\"\([^\"]\+\.java\)\"[[:space:]]*,.*O_RDONLY.*=[[:space:]][0-9]" "$straceOutput" | \
    grep -v "O_DIRECTORY"                                                                                     | \
    sed -e "s/^open([[:space:]]*\"\([^\"]\+\)\"[[:space:]]*,.*O_RDONLY.*/\1/"                                 | \
    sort                                                                                                      | \
    uniq                                                                                                      | \
    sed -e "s,^\([^/].*\),$currDir/\1,"                                                                       | \
    grep -v "$tempJavaFile"                                                                                   | \
    gawk -- "{ print \$1 \" \\\\\" }" >>$depFileName

  echo "" >>$depFileName
fi  

exit $javaRet
