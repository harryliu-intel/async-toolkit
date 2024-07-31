#!/bin/sh -x

NB=""

${NB} ./run_sigma.sh i0s 1 ulvt
${NB} ./run_sigma.sh i0s 1  lvt
${NB} ./run_sigma.sh i0s 2 ulvt
${NB} ./run_sigma.sh i0s 2  lvt
${NB} ./run_sigma.sh i0s 3 ulvt
${NB} ./run_sigma.sh i0s 3  lvt
${NB} ./run_sigma.sh i0m 2 ulvt
${NB} ./run_sigma.sh i0m 2  lvt
${NB} ./run_sigma.sh i0m 3 ulvt
${NB} ./run_sigma.sh i0m 3  lvt
