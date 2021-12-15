#!/bin/sh -x

./buildlamb.sh 30 16 read
`cat runspice.sh` &
./buildlamb.sh 38 16 read
`cat runspice.sh` &
./buildlamb.sh 80 16 read
`cat runspice.sh` &

wait 

./buildlamb.sh 96 16 read 
`cat runspice.sh` &
./buildlamb.sh 144 16 read
`cat runspice.sh` &
./buildlamb.sh 144 24 read
`cat runspice.sh` &

wait

./buildlamb.sh 137 32 read
`cat runspice.sh` &
./buildlamb.sh 144 36 read
`cat runspice.sh` &
./buildlamb.sh 142 48 read
`cat runspice.sh` &

wait

./buildlamb.sh 137 64 read
`cat runspice.sh` &
./buildlamb.sh 110 72 read
`cat runspice.sh` &
./buildlamb.sh 137 80 read
`cat runspice.sh` &

wait 

./buildlamb.sh 137 96 read
`cat runspice.sh` &
