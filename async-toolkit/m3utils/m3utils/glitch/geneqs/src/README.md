first run geneqs to make the equations
then run glitch checker

e.g.
```
mkdir inputs nb.out rundir
cp [source of files from Christina] inputs

./run_all.sh |& tee run.out
./runglitch_nb.sh > runglitch.task

make sure feeder is running

nbtask load runglitch.task
```

monitor with

nbstatus tasks

To run feeder:

nbfeeder start --work-area /netbatch/${USER}/${USER}_${HOST}_feeder/ --name ${USER}_${HOST}_feeder

e.g.

nbfeeder start --work-area /netbatch/mnystroe/mnystroe_scc928037/ --name mnystroe_scc928037




