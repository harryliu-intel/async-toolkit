#!/bin/sh

rm -f cell_list
ln -s ${cells} cell_list

mkdir -p ${workdir}

rm -f env.zsh
ln -s ${env} env.zsh

rm -f pvts.zsh
ln -s ${pvts} pvts.zsh
