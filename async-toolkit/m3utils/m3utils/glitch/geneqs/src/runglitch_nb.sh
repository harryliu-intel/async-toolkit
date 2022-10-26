#!/bin/sh
GLITCH=`pwd`/../../AMD64_LINUX/glitch
LIMIT="-limit 64"
WORKAREA=`pwd`/nb.out

cd rundir

cat << EOF 
JobsTask {
  WorkArea ${WORKAREA}
  Queue ${EC_SITE}_normal {
    Qslot /bfn/fe
  } 
  Jobs {

EOF

for dir in *; do

    cd ${dir}
    where=`pwd`

    for file in *.glitch; do
        echo "    nbjob run --class \"SLES12&&1G\" --log-file ${WORKAREA}/${file}.log ${GLITCH} ${LIMIT} -f ${where}/${file}"
    done

    cd ..
    
done

cat << EOF
  }
}

EOF
