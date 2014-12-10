#!/bin/zsh

DIR=`dirname "$1"`

if [[ ! -d $1 ]] ; then
  rm -f "$DIR"
  mkdir "$DIR"
  touch "$1"
fi 
