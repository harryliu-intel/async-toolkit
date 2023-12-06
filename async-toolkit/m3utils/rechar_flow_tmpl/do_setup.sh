#!/bin/sh

# set up the WARD for running Siliconsmart
# variables need to be set before sourcing this file

rm -f cell_list
ln -s ${cells} cell_list

mkdir -p ${workdir}
rm siliconsmart
ln -s ${workdir} siliconsmart

rm -f env.zsh
ln -s ${env} env.zsh

rm -f pvts.zsh
ln -s ${pvts} pvts.zsh
