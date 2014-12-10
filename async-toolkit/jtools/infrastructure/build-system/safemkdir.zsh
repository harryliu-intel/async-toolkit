#!/bin/zsh

directory=$1

if [[ ! -d "$directory" ]] ; then
  mkdir -p "$directory"
fi
