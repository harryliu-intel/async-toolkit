#!/bin/sh

# e.g., "convert.sh *.run"
# will convert all fsdbs found in the directories passed on cmd line

dirs=$*

#echo $dirs

cat <<EOF
JobsTask {
  WorkArea /nfs/site/home/mnystroe/work/m3utils/spice/fwr_clk_tc2/work/nb.run-2024-01-12T09:53:36-08:00
  SubmissionArgs --class SLES12SP5 --class 4C --class 8G

  Queue zsc9_dts {
    Qslot /de/blk/libchar
  } 
  Jobs {

EOF

for d in $dirs; do
    fsdbs=`echo $d/*.fsdb`
    echo "nbjob run --log-file ${d}.convert  ${M3UTILS}/spice/ct/AMD64_LINUX/ct -F -threads 8 -R 1e-12 -z -translate -wd ${d}.ctwork -C ${fsdbs}"
done


cat <<EOF
        }
}
EOF
