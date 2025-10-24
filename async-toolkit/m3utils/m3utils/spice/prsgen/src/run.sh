#!/bin/sh -x
/bin/rm -rf gnuplots
mkdir gnuplots
time ../AMD64_LINUX/prsgen -i example.prsgen -o example.prs -gp gnuplots
