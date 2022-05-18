first run geneqs to make the equations
then run glitch checker

e.g.
```
mkdir inputs nb.out rundir
cp [source of files from Christina] inputs

./run_all.sh |& tee run.out
./runglitch_nb.sh > runglitch.task
nbtask load runglitch.task
```


