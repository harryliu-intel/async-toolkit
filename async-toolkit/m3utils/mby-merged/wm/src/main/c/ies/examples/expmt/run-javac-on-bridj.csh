#! /usr/bin/csh -f

pushd .

cd src/java/HlpWm

set logdir = ../../../log

rm -rf *.class ${logdir}/*

set java_files = ( `ls *.java` )

set javac_opts = ""
#et javac_opts = "${javac_opts} -Xlint:unchecked"
set javac_opts = "${javac_opts} -Xmaxerrs 1000"
set javac_opts = "${javac_opts} -Xmaxwarns 2000"

set javac_cp = "/usr/share/java/bridj-0.7.0.jar"
set javac_cp = "${javac_cp}:."

/usr/bin/javac $javac_opts -cp $javac_cp $java_files |& tee ${logdir}/javac.log

set num_javac_errors = `egrep 'error:' ${logdir}/javac.log | wc -l`

echo "num_javac_errors = $num_javac_errors"

popd

