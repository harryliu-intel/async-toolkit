#! /usr/bin/csh -f 

pushd .

cd src/java

set jardir = ../../jar

rm -f ${jardir}/hlp-white-model.jar

echo "Creating jar for HlpWm ..."

jar cvf ${jardir}/hlp-white-model.jar HlpWm

popd

ls -l jar/hlp-white-model.jar
