

UNAME := $(shell uname)


#---------------------SED-------------------------
SED := sed
#-------------------------------------------------


#----------------------C--------------------------
# The C compiler to use.
#CCOMPILER := egcs
CCOMPILER := gcc
CPREPROCESSOR := $(CCOMPILER) -E

#----------------------CPP------------------------
CPPCOMPILER := g++
CPPPREPROCESSOR := $(CPPCOMPILER) -E


SYSTEM_INCLUDE_DIRS := /usr/local/include 
SYSTEM_LIB_DIRS := /usr/local/lib
SYSTEM_LIBS :=
SYSTEM_DEFINES :=

#-------------------------------------------------

#--------------------C-Linker---------------------
# The linker to use.
#CLINKER := egcs
CLINKER := gcc

CPPLINKER := g++

# The static library linker to use.
CSTATICLIBLINKER := ld -Ur

#-------------------------------------------------


#----------------------VisTran--------------------
VISTRAN := /usr/local/cad/bin/vistran
VISTRAN_DUP := 8
#-------------------------------------------------


#---------------------VCC-------------------------
VCC := /usr/local/cad/bin/vcc
VCCFLAGS :=
#-------------------------------------------------


#--------------------SVASM------------------------
SVASM := /usr/local/cad/bin/svasm
#-------------------------------------------------

#--------------------VASIM------------------------
VASIM := /usr/local/cad/bin/vasim
VASIMSTUBC := $(HOME)/vortex/vasim/lib/stub.c
VASIMINC := $(HOME)/vortex/vasim/include
VASIMLIBPATH := $(HOME)/vortex/vasim/lib
VASIMLIBS := sim
VASIMDEFS := 
#-------------------------------------------------

#--------------------latex------------------------
LATEX := /usr/bin/latex
DVIPS := /usr/bin/dvips
DVIPDF := /usr/bin/dvipdf
DIA := /usr/bin/dia
#-------------------------------------------------
#Irving Katz's fax number.
#949-854-9236




