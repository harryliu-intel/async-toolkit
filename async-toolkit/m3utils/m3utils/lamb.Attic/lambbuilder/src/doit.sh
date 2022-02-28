#!/bin/sh -x

function="read"

./buildlamb.sh 30 16 ${function}
`cat runspice.sh` &
./buildlamb.sh 38 16 ${function}
`cat runspice.sh` &
./buildlamb.sh 80 16 ${function}
`cat runspice.sh` &

wait 

./buildlamb.sh 96 16 ${function} 
`cat runspice.sh` &
./buildlamb.sh 144 16 ${function}
`cat runspice.sh` &
./buildlamb.sh 144 24 ${function}
`cat runspice.sh` &

wait

./buildlamb.sh 137 32 ${function}
`cat runspice.sh` &
./buildlamb.sh 144 36 ${function}
`cat runspice.sh` &
./buildlamb.sh 142 48 ${function}
`cat runspice.sh` &

wait

./buildlamb.sh 137 64 ${function}
`cat runspice.sh` &
./buildlamb.sh 110 72 ${function}
`cat runspice.sh` &
./buildlamb.sh 137 80 ${function}
`cat runspice.sh` &

wait 

./buildlamb.sh 137 96 ${function}
`cat runspice.sh` &
