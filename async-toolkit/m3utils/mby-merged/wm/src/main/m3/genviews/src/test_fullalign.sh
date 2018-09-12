#!/bin/sh -x

WD=/p/hlp/mnystroe/git/hlp-hw/src/srdl

METAROOT=/p/hlp/mnystroe/git/meta-git/

SVPP=${METAROOT}/rdl/svpp/AMD64_LINUX/svpp 
PERLFE=${METAROOT}/perlfe/AMD64_LINUX/perlfe

PATHSPEC="--path ${WD}:/p/hdk/rtl/cad/x86-64_linux30/dt/nebulon/d17ww32.5/include"

input=fullalign.rdl
top_map=some_map

../AMD64_LINUX/genviews -top ${top_map} < ${input}

######################################################################
cat > build/src/Main.m3 << _EOF_
MODULE Main;
_EOF_

cat >> build/src/Main.m3 << _EOF_
IMPORT ${top_map} AS Map;
IMPORT ${top_map}_addr AS MapAddr;
IMPORT IO;
IMPORT CompAddr;
IMPORT Fmt;
IMPORT Thread;

VAR
  x : Map.T;
  y : Map.U;

PROCEDURE P(READONLY z : Map.T; READONLY u : Map.U) =
  BEGIN
    IO.Put("Hi!\n")
  END P;
  
BEGIN
  P(x,y);

  EVAL MapAddr.Init(y, CompAddr.T { 0, 0 });
  IO.Put(Fmt.Int(CompAddr.initCount) & " fields have been address initialized.\n");
  Thread.Pause(0.0d0);
END Main.
_EOF_

######################################################################
cat > build/src/m3overrides << _EOF_
include("../../../../../../m3overrides")
_EOF_

######################################################################
cat > build/src/m3makefile << _EOF_
import ("libm3")
import ("wm_support")
include ("m3makefile.maps")
implementation ("Main")
program ("testme")

_EOF_

######################################################################
cd build
cm3 -x
