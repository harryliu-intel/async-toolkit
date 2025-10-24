#!/bin/sh -x

GENDIR=gendir/
M3SRC=m3src/
WOEXT=$GENDIR/without_ext/
SRC=$GENDIR/src/
AMD=$GENDIR/AMD64_LINUX/

rm -rf ./$GENDIR
mkdir -p ./$WOEXT
cp `pwd`/m3src/* ./$WOEXT
# Gen parser extensions
python wrap/genparserext.py --token `pwd`/$WOEXT/M3.t --lexer `pwd`/$WOEXT/M3.l --parser `pwd`/$WOEXT/M3.y --names `pwd`/$WOEXT/M3.names --exttok `pwd`/$WOEXT/TokExt.e --extlex `pwd`/$WOEXT/LexerExt.e --extparse `pwd`/$WOEXT/ParserExt.e
# Gen m3makefile
python wrap/genm3make.py --indir `pwd`/$WOEXT/ --outdir `pwd`/$SRC/ --progname test
rm -rf ./$WOEXT/
# Build the source files
cd ./$SRC/ ; /p/hdk/rtl/cad/x86-64_linux30/opensource/cm3/d5.10.0-20180711/bin/cm3 -x
# Run the test and generate the debug file
cd ../../
./$AMD/test
